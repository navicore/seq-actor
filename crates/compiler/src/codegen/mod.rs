//! LLVM IR code generation for seq-actor
//!
//! Generates LLVM IR text that can be compiled with clang.

use std::collections::HashMap;

use crate::ast::*;
use crate::types::Type;

/// LLVM IR code generator
pub struct CodeGen {
    /// Output buffer
    output: String,
    /// Local variable counter for SSA
    local_counter: usize,
    /// Label counter for control flow
    label_counter: usize,
    /// String literal pool
    strings: Vec<String>,
    /// Variable -> LLVM register mapping
    variables: HashMap<String, String>,
    /// Indentation level
    indent: usize,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            local_counter: 0,
            label_counter: 0,
            strings: Vec::new(),
            variables: HashMap::new(),
            indent: 0,
        }
    }

    /// Generate LLVM IR for a program
    pub fn generate(&mut self, program: &Program) -> String {
        // Module header
        self.emit_line("; seq-actor compiled module");
        // Use host target triple (clang will figure out the actual target)
        #[cfg(target_os = "macos")]
        {
            #[cfg(target_arch = "aarch64")]
            self.emit_line("target triple = \"arm64-apple-macosx\"");
            #[cfg(target_arch = "x86_64")]
            self.emit_line("target triple = \"x86_64-apple-macosx\"");
        }
        #[cfg(target_os = "linux")]
        self.emit_line("target triple = \"x86_64-unknown-linux-gnu\"");
        #[cfg(not(any(target_os = "macos", target_os = "linux")))]
        self.emit_line("target triple = \"x86_64-unknown-linux-gnu\"");
        self.emit_line("");

        // Runtime declarations
        self.emit_runtime_declarations();
        self.emit_line("");

        // Generate code for each function
        for item in &program.items {
            match item {
                Item::FuncDef(func) => self.generate_function(func),
                Item::TypeDef(_) => {
                    // Types are handled at compile time, no IR needed
                }
                Item::Include(_) => {
                    // Includes are resolved before codegen
                }
            }
        }

        // Generate string constants
        self.emit_string_constants();

        std::mem::take(&mut self.output)
    }

    /// Emit runtime function declarations
    fn emit_runtime_declarations(&mut self) {
        self.emit_line("; Runtime declarations");
        self.emit_line("declare i64 @sa_add_int(i64, i64)");
        self.emit_line("declare i64 @sa_sub_int(i64, i64)");
        self.emit_line("declare i64 @sa_mul_int(i64, i64)");
        self.emit_line("declare i64 @sa_div_int(i64, i64)");
        self.emit_line("declare i64 @sa_mod_int(i64, i64)");
        self.emit_line("declare i64 @sa_neg_int(i64)");
        self.emit_line("declare i1 @sa_eq_int(i64, i64)");
        self.emit_line("declare i1 @sa_lt_int(i64, i64)");
        self.emit_line("declare i1 @sa_le_int(i64, i64)");
        self.emit_line("declare i1 @sa_gt_int(i64, i64)");
        self.emit_line("declare i1 @sa_ge_int(i64, i64)");
        self.emit_line("declare double @sa_add_float(double, double)");
        self.emit_line("declare double @sa_sub_float(double, double)");
        self.emit_line("declare double @sa_mul_float(double, double)");
        self.emit_line("declare double @sa_div_float(double, double)");
        self.emit_line("declare double @sa_neg_float(double)");
        self.emit_line("declare i8* @sa_alloc(i64)");
        self.emit_line("declare void @sa_print(i8*)");
        self.emit_line("declare void @sa_println(i8*)");
        self.emit_line("declare i8* @sa_int_to_string(i64)");
        self.emit_line("declare i8* @sa_concat(i8*, i8*)");
        self.emit_line("declare i64 @sa_string_length(i8*)");
        self.emit_line("");
        self.emit_line("; StackValue type (seq-core compatible: 5 x i64 = 40 bytes)");
        self.emit_line("; slot0 = discriminant, slot1 = primary payload, slot2-4 = type-specific");
        self.emit_line("%StackValue = type { i64, i64, i64, i64, i64 }");
        self.emit_line("");
        self.emit_line("; Actor runtime declarations");
        self.emit_line("declare i64 @sa_actor_spawn(i8*, i8*, i64)");
        self.emit_line("declare void @sa_actor_send(i64, i64)");
        self.emit_line("declare i64 @sa_actor_receive()");
        self.emit_line("declare void @sa_actor_defer(i64)");
        self.emit_line("declare i64 @sa_actor_self()");
        self.emit_line("declare void @sa_actor_init()");
        self.emit_line("declare void @sa_actor_wait_all()");
        self.emit_line("");
    }

    /// Generate a function
    fn generate_function(&mut self, func: &FuncDef) {
        self.variables.clear();
        self.local_counter = 0;

        // For simplicity, we'll generate code for the first clause only
        // Full pattern matching compilation would be more complex
        let clause = &func.clauses[0];

        // Function signature
        let ret_type = self.llvm_type(&Type::Int); // Simplified: all return Int for now
        let params: Vec<String> = clause
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let param_type = self.llvm_type(&Type::Int);
                let param_name = self.pattern_name(p).unwrap_or_else(|| format!("arg{}", i));
                format!("{} %{}", param_type, param_name)
            })
            .collect();

        let is_main = func.name == "main";
        let func_name = if is_main {
            "main".to_string()
        } else {
            format!("sa_{}", func.name)
        };

        self.emit_line(&format!(
            "define {} @{}({}) {{",
            if is_main { "i32" } else { &ret_type },
            func_name,
            params.join(", ")
        ));

        // Entry basic block label
        self.emit_line("entry:");
        self.indent += 1;

        // Register parameters in variable map
        for param in &clause.params {
            if let Some(name) = self.pattern_name(param) {
                self.variables.insert(name.clone(), format!("%{}", name));
            }
        }

        // Generate function body
        let result = self.generate_expr(&clause.body);

        // Return
        if is_main {
            self.emit_line("ret i32 0");
        } else {
            self.emit_line(&format!("ret {} {}", ret_type, result));
        }

        self.indent -= 1;
        self.emit_line("}");
        self.emit_line("");
    }

    /// Generate code for an expression, returning the LLVM value
    fn generate_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Literal { value, .. } => self.generate_literal(value),

            Expr::Var { name, .. } => self
                .variables
                .get(name)
                .cloned()
                .unwrap_or_else(|| format!("@sa_{}", name)),

            Expr::BinOp {
                op, left, right, ..
            } => {
                let left_val = self.generate_expr(left);
                let right_val = self.generate_expr(right);
                self.generate_binop(op, &left_val, &right_val)
            }

            Expr::UnaryOp { op, operand, .. } => {
                let operand_val = self.generate_expr(operand);
                self.generate_unaryop(op, &operand_val)
            }

            Expr::Call { func, args, .. } => {
                let func_name = match func.as_ref() {
                    Expr::Var { name, .. } => {
                        // Check for builtin
                        match name.as_str() {
                            "println" => "sa_println".to_string(),
                            "print" => "sa_print".to_string(),
                            "int_to_string" => "sa_int_to_string".to_string(),
                            "concat" => "sa_concat".to_string(),
                            "length" => "sa_string_length".to_string(),
                            _ => format!("sa_{}", name),
                        }
                    }
                    _ => {
                        // Function pointer call (not yet supported)
                        "sa_unknown".to_string()
                    }
                };

                let arg_vals: Vec<String> = args.iter().map(|a| self.generate_expr(a)).collect();

                // Determine return type based on function
                let ret_type = match func_name.as_str() {
                    "sa_println" | "sa_print" => "void",
                    "sa_int_to_string" | "sa_concat" => "i8*",
                    "sa_string_length" => "i64",
                    _ => "i64",
                };

                let result = self.fresh_local();
                let args_str: Vec<String> = arg_vals
                    .iter()
                    .map(|v| {
                        // Infer argument type from value
                        if v.starts_with('@') || v.starts_with("getelementptr") || v.contains("i8*")
                        {
                            format!("i8* {}", v)
                        } else {
                            format!("i64 {}", v)
                        }
                    })
                    .collect();

                if ret_type == "void" {
                    self.emit_line(&format!(
                        "call {} @{}({})",
                        ret_type,
                        func_name,
                        args_str.join(", ")
                    ));
                    "0".to_string()
                } else {
                    self.emit_line(&format!(
                        "{} = call {} @{}({})",
                        result,
                        ret_type,
                        func_name,
                        args_str.join(", ")
                    ));
                    result
                }
            }

            Expr::Let {
                pattern,
                value,
                body,
                ..
            } => {
                let value_result = self.generate_expr(value);

                // Bind the pattern
                if let Some(name) = self.pattern_name(pattern) {
                    self.variables.insert(name.clone(), value_result);
                }

                // Generate body
                self.generate_expr(body)
            }

            Expr::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_val = self.generate_expr(cond);
                let then_label = self.fresh_label("then");
                let else_label = self.fresh_label("else");
                let end_label = self.fresh_label("endif");

                self.emit_line(&format!(
                    "br i1 {}, label %{}, label %{}",
                    cond_val, then_label, else_label
                ));

                // Then branch
                self.emit_label(&then_label);
                let then_val = self.generate_expr(then_branch);
                let then_block = self.fresh_label("then_end");
                self.emit_line(&format!("br label %{}", end_label));

                // Else branch
                self.emit_label(&else_label);
                let else_val = self.generate_expr(else_branch);
                let else_block = self.fresh_label("else_end");
                self.emit_line(&format!("br label %{}", end_label));

                // Merge
                self.emit_label(&end_label);
                let result = self.fresh_local();
                self.emit_line(&format!(
                    "{} = phi i64 [ {}, %{} ], [ {}, %{} ]",
                    result, then_val, then_block, else_val, else_block
                ));

                let _ = then_block;
                let _ = else_block;
                result
            }

            Expr::Tuple { elements, .. } => {
                // Allocate tuple on heap
                let size = elements.len() * 8; // 8 bytes per element
                let ptr = self.fresh_local();
                self.emit_line(&format!("{} = call i8* @sa_alloc(i64 {})", ptr, size));

                // Store elements
                for (i, elem) in elements.iter().enumerate() {
                    let val = self.generate_expr(elem);
                    let elem_ptr = self.fresh_local();
                    self.emit_line(&format!(
                        "{} = getelementptr i8, i8* {}, i64 {}",
                        elem_ptr,
                        ptr,
                        i * 8
                    ));
                    let typed_ptr = self.fresh_local();
                    self.emit_line(&format!("{} = bitcast i8* {} to i64*", typed_ptr, elem_ptr));
                    self.emit_line(&format!("store i64 {}, i64* {}", val, typed_ptr));
                }

                // Return pointer as int
                let result = self.fresh_local();
                self.emit_line(&format!("{} = ptrtoint i8* {} to i64", result, ptr));
                result
            }

            Expr::Tagged { tag, elements, .. } => {
                // Allocate tagged tuple (tag byte + elements)
                let size = 1 + elements.len() * 8;
                let ptr = self.fresh_local();
                self.emit_line(&format!("{} = call i8* @sa_alloc(i64 {})", ptr, size));

                // Store tag (use hash of tag name as byte)
                let tag_byte = (tag.bytes().fold(0u8, |acc, b| acc.wrapping_add(b))) as i64;
                self.emit_line(&format!("store i8 {}, i8* {}", tag_byte, ptr));

                // Store elements
                for (i, elem) in elements.iter().enumerate() {
                    let val = self.generate_expr(elem);
                    let elem_ptr = self.fresh_local();
                    self.emit_line(&format!(
                        "{} = getelementptr i8, i8* {}, i64 {}",
                        elem_ptr,
                        ptr,
                        1 + i * 8
                    ));
                    let typed_ptr = self.fresh_local();
                    self.emit_line(&format!("{} = bitcast i8* {} to i64*", typed_ptr, elem_ptr));
                    self.emit_line(&format!("store i64 {}, i64* {}", val, typed_ptr));
                }

                let result = self.fresh_local();
                self.emit_line(&format!("{} = ptrtoint i8* {} to i64", result, ptr));
                result
            }

            Expr::Constructor { name, args, .. } => {
                // Similar to tagged, but use constructor name
                self.generate_expr(&Expr::Tagged {
                    tag: name.clone(),
                    elements: args.clone(),
                    span: Span::default(),
                })
            }

            Expr::List { elements, tail, .. } => {
                if elements.is_empty() && tail.is_none() {
                    // Empty list = null
                    "0".to_string()
                } else {
                    // Build list from end
                    let mut current = if let Some(t) = tail {
                        self.generate_expr(t)
                    } else {
                        "0".to_string()
                    };

                    for elem in elements.iter().rev() {
                        let val = self.generate_expr(elem);
                        // Allocate cons cell (value + next pointer)
                        let cell = self.fresh_local();
                        self.emit_line(&format!("{} = call i8* @sa_alloc(i64 16)", cell));

                        // Store value
                        let val_ptr = self.fresh_local();
                        self.emit_line(&format!("{} = bitcast i8* {} to i64*", val_ptr, cell));
                        self.emit_line(&format!("store i64 {}, i64* {}", val, val_ptr));

                        // Store next pointer
                        let next_ptr_ptr = self.fresh_local();
                        self.emit_line(&format!(
                            "{} = getelementptr i8, i8* {}, i64 8",
                            next_ptr_ptr, cell
                        ));
                        let typed_next = self.fresh_local();
                        self.emit_line(&format!(
                            "{} = bitcast i8* {} to i64*",
                            typed_next, next_ptr_ptr
                        ));
                        self.emit_line(&format!("store i64 {}, i64* {}", current, typed_next));

                        let cell_int = self.fresh_local();
                        self.emit_line(&format!("{} = ptrtoint i8* {} to i64", cell_int, cell));
                        current = cell_int;
                    }

                    current
                }
            }

            Expr::Match { expr, arms, .. } => {
                let scrutinee = self.generate_expr(expr);

                // For now, simple linear pattern matching
                // Full pattern compilation would create decision trees
                let end_label = self.fresh_label("match_end");
                let result_var = self.fresh_local();

                // Allocate space for result
                self.emit_line(&format!("{} = alloca i64", result_var));

                for (i, arm) in arms.iter().enumerate() {
                    let match_label = self.fresh_label(&format!("match_arm_{}", i));
                    let next_label = if i + 1 < arms.len() {
                        self.fresh_label(&format!("match_try_{}", i + 1))
                    } else {
                        end_label.clone()
                    };

                    // Try to match pattern
                    let matches = self.generate_pattern_match(&arm.pattern, &scrutinee);
                    self.emit_line(&format!(
                        "br i1 {}, label %{}, label %{}",
                        matches, match_label, next_label
                    ));

                    // Pattern matched
                    self.emit_label(&match_label);
                    self.bind_pattern_vars(&arm.pattern, &scrutinee);
                    let arm_result = self.generate_expr(&arm.body);
                    self.emit_line(&format!("store i64 {}, i64* {}", arm_result, result_var));
                    self.emit_line(&format!("br label %{}", end_label));

                    if i + 1 < arms.len() {
                        self.emit_label(&next_label);
                    }
                }

                self.emit_label(&end_label);
                let loaded = self.fresh_local();
                self.emit_line(&format!("{} = load i64, i64* {}", loaded, result_var));
                loaded
            }

            Expr::Block { exprs, .. } => {
                let mut result = "0".to_string();
                for e in exprs {
                    result = self.generate_expr(e);
                }
                result
            }

            Expr::Lambda { .. } => {
                // Lambdas require closure allocation - simplified for now
                "0".to_string()
            }

            Expr::Receive { arms, timeout, .. } => self.generate_receive(arms, timeout),

            Expr::Become { func, args, .. } => self.generate_become(func, args),

            Expr::Spawn { func, args, .. } => self.generate_spawn(func, args),

            Expr::Send {
                target, message, ..
            } => self.generate_send(target, message),

            Expr::SelfRef { .. } => {
                // Get current actor's ID
                let result = self.fresh_local();
                self.emit_line(&format!("{} = call i64 @sa_actor_self()", result));
                result
            }

            Expr::ModuleCall {
                module, func, args, ..
            } => {
                // Generate code for module-qualified calls
                self.generate_module_call(module, func, args)
            }

            Expr::Supervised { body, .. } => {
                // Generate code for supervised block
                // For now, just generate the body expressions
                let mut result = "0".to_string();
                for expr in body {
                    result = self.generate_expr(expr);
                }
                result
            }
        }
    }

    /// Generate code for a module-qualified call
    fn generate_module_call(&mut self, module: &str, func: &str, args: &[Expr]) -> String {
        // Map module:function to runtime functions
        let runtime_func = match (module, func) {
            ("io", "print") => "sa_print",
            ("io", "println") => "sa_println",
            ("io", "read_line") => "sa_read_line",
            ("str", "concat") => "sa_concat",
            ("str", "length") => "sa_string_length",
            ("str", "split") => "sa_string_split",
            ("int", "to_string") => "sa_int_to_string",
            ("float", "to_string") => "sa_float_to_string",
            ("list", "head") => "sa_list_head",
            ("list", "tail") => "sa_list_tail",
            ("list", "is_empty") => "sa_list_is_empty",
            ("list", "cons") => "sa_list_cons",
            ("actor", "self") => "sa_actor_self",
            ("actor", "send") => "sa_actor_send",
            ("actor", "spawn") => "sa_actor_spawn",
            _ => {
                // Unknown function - generate placeholder
                return "0".to_string();
            }
        };

        // Generate arguments
        let arg_values: Vec<String> = args.iter().map(|arg| self.generate_expr(arg)).collect();

        // Determine expected argument types based on function
        let arg_types = match (module, func) {
            ("io", "print") | ("io", "println") => vec!["i8*"],
            ("str", "concat") => vec!["i8*", "i8*"],
            ("str", "length") => vec!["i8*"],
            ("int", "to_string") => vec!["i64"],
            ("float", "to_string") => vec!["double"],
            _ => vec![], // Will use inference
        };

        // Format arguments with type annotations for LLVM
        let arg_str: Vec<String> = arg_values
            .iter()
            .enumerate()
            .map(|(i, v)| {
                // Use known type if available, otherwise infer
                let typ = if i < arg_types.len() {
                    arg_types[i]
                } else if v.starts_with("getelementptr") || v.contains("i8*") {
                    "i8*"
                } else {
                    "i64"
                };
                format!("{} {}", typ, v)
            })
            .collect();
        let args_formatted = arg_str.join(", ");

        // Generate the call
        let result = self.fresh_local();

        // Determine return type based on function
        match (module, func) {
            ("io", "print") | ("io", "println") => {
                self.emit_line(&format!("call void @{}({})", runtime_func, args_formatted));
                "0".to_string()
            }
            ("int", "to_string")
            | ("float", "to_string")
            | ("str", "concat")
            | ("io", "read_line") => {
                self.emit_line(&format!(
                    "{} = call i8* @{}({})",
                    result, runtime_func, args_formatted
                ));
                result
            }
            ("str", "length") | ("list", "length") => {
                self.emit_line(&format!(
                    "{} = call i64 @{}({})",
                    result, runtime_func, args_formatted
                ));
                result
            }
            ("actor", "self") => {
                self.emit_line(&format!("{} = call i8* @{}()", result, runtime_func));
                result
            }
            _ => {
                // Generic call
                self.emit_line(&format!(
                    "{} = call i64 @{}({})",
                    result, runtime_func, args_formatted
                ));
                result
            }
        }
    }

    /// Generate code for a literal
    fn generate_literal(&mut self, lit: &Literal) -> String {
        match lit {
            Literal::Int(n) => n.to_string(),
            Literal::Float(f) => {
                // LLVM requires hex representation for floats
                format!("0x{:016X}", f.to_bits())
            }
            Literal::Bool(b) => if *b { "1" } else { "0" }.to_string(),
            Literal::String(s) => {
                // Add to string pool
                let idx = self.strings.len();
                self.strings.push(s.clone());
                format!(
                    "getelementptr inbounds ([{} x i8], [{} x i8]* @.str.{}, i64 0, i64 0)",
                    s.len() + 1,
                    s.len() + 1,
                    idx
                )
            }
            Literal::Unit => "0".to_string(),
        }
    }

    /// Generate code for a binary operation
    fn generate_binop(&mut self, op: &BinOp, left: &str, right: &str) -> String {
        let result = self.fresh_local();
        let instruction = match op {
            BinOp::Add => format!("{} = add i64 {}, {}", result, left, right),
            BinOp::Sub => format!("{} = sub i64 {}, {}", result, left, right),
            BinOp::Mul => format!("{} = mul i64 {}, {}", result, left, right),
            BinOp::Div => format!("{} = sdiv i64 {}, {}", result, left, right),
            BinOp::Mod => format!("{} = srem i64 {}, {}", result, left, right),
            BinOp::Eq => format!("{} = icmp eq i64 {}, {}", result, left, right),
            BinOp::Ne => format!("{} = icmp ne i64 {}, {}", result, left, right),
            BinOp::Lt => format!("{} = icmp slt i64 {}, {}", result, left, right),
            BinOp::Le => format!("{} = icmp sle i64 {}, {}", result, left, right),
            BinOp::Gt => format!("{} = icmp sgt i64 {}, {}", result, left, right),
            BinOp::Ge => format!("{} = icmp sge i64 {}, {}", result, left, right),
            BinOp::And => format!("{} = and i1 {}, {}", result, left, right),
            BinOp::Or => format!("{} = or i1 {}, {}", result, left, right),
            BinOp::Cons => {
                // List cons - allocate cons cell
                self.emit_line(&format!("{} = call i8* @sa_alloc(i64 16)", result));
                let val_ptr = self.fresh_local();
                self.emit_line(&format!("{} = bitcast i8* {} to i64*", val_ptr, result));
                self.emit_line(&format!("store i64 {}, i64* {}", left, val_ptr));
                let next_ptr = self.fresh_local();
                self.emit_line(&format!(
                    "{} = getelementptr i8, i8* {}, i64 8",
                    next_ptr, result
                ));
                let typed_next = self.fresh_local();
                self.emit_line(&format!(
                    "{} = bitcast i8* {} to i64*",
                    typed_next, next_ptr
                ));
                self.emit_line(&format!("store i64 {}, i64* {}", right, typed_next));
                let final_result = self.fresh_local();
                self.emit_line(&format!(
                    "{} = ptrtoint i8* {} to i64",
                    final_result, result
                ));
                return final_result;
            }
        };
        self.emit_line(&instruction);
        result
    }

    /// Generate code for a unary operation
    fn generate_unaryop(&mut self, op: &UnaryOp, operand: &str) -> String {
        let result = self.fresh_local();
        let instruction = match op {
            UnaryOp::Neg => format!("{} = sub i64 0, {}", result, operand),
            UnaryOp::Not => format!("{} = xor i1 {}, 1", result, operand),
        };
        self.emit_line(&instruction);
        result
    }

    /// Generate pattern match check (returns i1)
    fn generate_pattern_match(&mut self, pattern: &Pattern, value: &str) -> String {
        match pattern {
            Pattern::Wildcard { .. } => "1".to_string(), // Always matches
            Pattern::Var { .. } => "1".to_string(),      // Always matches
            Pattern::Literal { value: lit, .. } => {
                let lit_val = self.generate_literal(lit);
                let result = self.fresh_local();
                self.emit_line(&format!("{} = icmp eq i64 {}, {}", result, value, lit_val));
                result
            }
            _ => "1".to_string(), // Simplified - full pattern matching is complex
        }
    }

    /// Bind pattern variables to the matched value
    fn bind_pattern_vars(&mut self, pattern: &Pattern, value: &str) {
        if let Pattern::Var { name, .. } = pattern {
            self.variables.insert(name.clone(), value.to_string());
        }
        // Full implementation would handle tuple/constructor destructuring
    }

    /// Get name from a simple pattern
    fn pattern_name(&self, pattern: &Pattern) -> Option<String> {
        match pattern {
            Pattern::Var { name, .. } => Some(name.clone()),
            _ => None,
        }
    }

    /// Emit string constants at end of module
    fn emit_string_constants(&mut self) {
        if !self.strings.is_empty() {
            // Collect formatted strings first to avoid borrow conflicts
            let string_constants: Vec<String> = self
                .strings
                .iter()
                .enumerate()
                .map(|(i, s)| {
                    let escaped = s
                        .bytes()
                        .map(|b| {
                            if b.is_ascii_alphanumeric() || b == b' ' {
                                (b as char).to_string()
                            } else {
                                format!("\\{:02X}", b)
                            }
                        })
                        .collect::<String>();
                    format!(
                        "@.str.{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"",
                        i,
                        s.len() + 1,
                        escaped
                    )
                })
                .collect();

            self.emit_line("; String constants");
            for constant in string_constants {
                self.emit_line(&constant);
            }
        }
    }

    /// Generate code for spawn expression
    fn generate_spawn(&mut self, func: &str, args: &[Expr]) -> String {
        // Get function pointer for the actor function
        let func_ptr = self.fresh_local();
        self.emit_line(&format!(
            "{} = bitcast i64 ({})*  @sa_{} to i8*",
            func_ptr,
            (0..args.len())
                .map(|_| "i64")
                .collect::<Vec<_>>()
                .join(", "),
            func
        ));

        // Allocate array for arguments
        let args_array = if args.is_empty() {
            "null".to_string()
        } else {
            let array_ptr = self.fresh_local();
            self.emit_line(&format!(
                "{} = call i8* @sa_alloc(i64 {})",
                array_ptr,
                args.len() * 8
            ));
            let typed_array = self.fresh_local();
            self.emit_line(&format!(
                "{} = bitcast i8* {} to i64*",
                typed_array, array_ptr
            ));

            // Store each argument
            for (i, arg) in args.iter().enumerate() {
                let arg_val = self.generate_expr(arg);
                let elem_ptr = self.fresh_local();
                self.emit_line(&format!(
                    "{} = getelementptr i64, i64* {}, i64 {}",
                    elem_ptr, typed_array, i
                ));
                self.emit_line(&format!("store i64 {}, i64* {}", arg_val, elem_ptr));
            }
            typed_array
        };

        // Call spawn
        let result = self.fresh_local();
        self.emit_line(&format!(
            "{} = call i64 @sa_actor_spawn(i8* {}, i64* {}, i64 {})",
            result,
            func_ptr,
            args_array,
            args.len()
        ));
        result
    }

    /// Generate code for send expression
    fn generate_send(&mut self, target: &Expr, message: &Expr) -> String {
        let target_val = self.generate_expr(target);
        let msg_val = self.generate_expr(message);
        self.emit_line(&format!(
            "call void @sa_actor_send(i64 {}, i64 {})",
            target_val, msg_val
        ));
        "0".to_string() // Send returns unit
    }

    /// Generate code for receive expression
    fn generate_receive(
        &mut self,
        arms: &[crate::ast::ReceiveArm],
        _timeout: &Option<crate::ast::ReceiveTimeout>,
    ) -> String {
        // For now, implement a simple receive that just gets the next message
        // Full pattern matching would loop and defer non-matching messages

        let receive_start = self.fresh_label("receive_start");
        let receive_match = self.fresh_label("receive_match");
        let receive_end = self.fresh_label("receive_end");

        // Result storage
        let result_ptr = self.fresh_local();
        self.emit_line(&format!("{} = alloca i64", result_ptr));

        // Start receive loop
        self.emit_line(&format!("br label %{}", receive_start));
        self.emit_label(&receive_start);

        // Receive a message
        let msg = self.fresh_local();
        self.emit_line(&format!("{} = call i64 @sa_actor_receive()", msg));

        // Try to match arms
        for (i, arm) in arms.iter().enumerate() {
            let arm_match = self.fresh_label(&format!("arm_match_{}", i));
            let arm_next = if i + 1 < arms.len() {
                self.fresh_label(&format!("arm_next_{}", i))
            } else {
                receive_start.clone() // Loop back if no match
            };

            // Generate pattern match
            let matches = self.generate_pattern_match(&arm.pattern, &msg);
            self.emit_line(&format!(
                "br i1 {}, label %{}, label %{}",
                matches, arm_match, arm_next
            ));

            // Pattern matched - execute body
            self.emit_label(&arm_match);
            self.bind_pattern_vars(&arm.pattern, &msg);
            let body_result = self.generate_expr(&arm.body);
            self.emit_line(&format!("store i64 {}, i64* {}", body_result, result_ptr));
            self.emit_line(&format!("br label %{}", receive_end));

            if i + 1 < arms.len() {
                self.emit_label(&arm_next);
            }
        }

        // If no arms, defer and loop (shouldn't normally happen)
        if arms.is_empty() {
            self.emit_line(&format!("call void @sa_actor_defer(i64 {})", msg));
            self.emit_line(&format!("br label %{}", receive_start));
        }

        // Done with pattern matching - defer unmatched message and loop
        // This label is only reached if no pattern matched (fallthrough from last arm_next)
        self.emit_line(&format!("call void @sa_actor_defer(i64 {})", msg));
        self.emit_line(&format!("br label %{}", receive_start));

        // End of receive
        let _ = receive_match;
        self.emit_label(&receive_end);
        let loaded_result = self.fresh_local();
        self.emit_line(&format!(
            "{} = load i64, i64* {}",
            loaded_result, result_ptr
        ));
        loaded_result
    }

    /// Generate code for become expression (tail call to state function)
    fn generate_become(&mut self, func: &str, args: &[Expr]) -> String {
        // Generate arguments
        let arg_vals: Vec<String> = args.iter().map(|a| self.generate_expr(a)).collect();
        let args_str: Vec<String> = arg_vals.iter().map(|v| format!("i64 {}", v)).collect();

        // Tail call to the state function
        let result = self.fresh_local();
        self.emit_line(&format!(
            "{} = tail call i64 @sa_{}({})",
            result,
            func,
            args_str.join(", ")
        ));

        // Return the result (become is a tail call)
        self.emit_line(&format!("ret i64 {}", result));

        // Return unreachable value (code after become is dead)
        "unreachable".to_string()
    }

    /// Get LLVM type for a Type
    fn llvm_type(&self, typ: &Type) -> String {
        match typ {
            Type::Int => "i64".to_string(),
            Type::Float => "double".to_string(),
            Type::Bool => "i1".to_string(),
            Type::String => "i8*".to_string(),
            Type::Unit => "void".to_string(),
            Type::Tuple(_) => "i8*".to_string(), // Heap allocated
            Type::List(_) => "i8*".to_string(),  // Heap allocated
            Type::Actor(_) => "i8*".to_string(), // Actor handle
            Type::Channel(_) => "i8*".to_string(), // Channel handle
            Type::Function { .. } => "i8*".to_string(), // Function pointer
            _ => "i64".to_string(),              // Default
        }
    }

    /// Fresh local variable
    fn fresh_local(&mut self) -> String {
        let n = self.local_counter;
        self.local_counter += 1;
        format!("%{}", n)
    }

    /// Fresh label
    fn fresh_label(&mut self, prefix: &str) -> String {
        let n = self.label_counter;
        self.label_counter += 1;
        format!("{}_{}", prefix, n)
    }

    /// Emit a line of IR
    fn emit_line(&mut self, line: &str) {
        for _ in 0..self.indent {
            self.output.push_str("  ");
        }
        self.output.push_str(line);
        self.output.push('\n');
    }

    /// Emit a label
    fn emit_label(&mut self, label: &str) {
        // Labels have no indentation
        let saved_indent = self.indent;
        self.indent = 0;
        self.emit_line(&format!("{}:", label));
        self.indent = saved_indent;
    }
}

impl Default for CodeGen {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_generate_simple() {
        let source = ": main () -> println(\"Hello, World!\") ;";
        let program = parse(source).unwrap();
        let mut codegen = CodeGen::new();
        let ir = codegen.generate(&program);
        assert!(ir.contains("@main"));
        assert!(ir.contains("@sa_println"));
    }

    #[test]
    fn test_generate_arithmetic() {
        let source = ": add (x, y) -> x + y ;";
        let program = parse(source).unwrap();
        let mut codegen = CodeGen::new();
        let ir = codegen.generate(&program);
        assert!(ir.contains("@sa_add"));
        assert!(ir.contains("add i64"));
    }
}
