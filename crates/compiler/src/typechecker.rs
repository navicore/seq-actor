//! Type checker for seq-actor
//!
//! Performs type inference, type checking, and affine analysis.

use std::collections::HashMap;

use crate::ast::*;
use crate::types::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected}, found {found} at line {line}")]
    TypeMismatch {
        expected: String,
        found: String,
        line: usize,
    },

    #[error("Undefined variable '{name}' at line {line}")]
    UndefinedVariable { name: String, line: usize },

    #[error("Undefined type '{name}' at line {line}")]
    UndefinedType { name: String, line: usize },

    #[error("Unknown function '{name}' at line {line}")]
    UnknownFunction { name: String, line: usize },

    #[error("Variable '{name}' used after being consumed (affine violation) at line {line}")]
    AffineViolation { name: String, line: usize },

    #[error("Non-exhaustive patterns at line {line}")]
    NonExhaustivePatterns { line: usize },

    #[error("Duplicate definition of '{name}' at line {line}")]
    DuplicateDefinition { name: String, line: usize },

    #[error("Cannot unify types {t1} and {t2}")]
    UnificationError { t1: String, t2: String },

    #[error("Occurs check failed: {var} occurs in {typ}")]
    OccursCheck { var: String, typ: String },
}

/// Type checker state
pub struct TypeChecker {
    /// Global type environment
    env: TypeEnv,
    /// Type variable counter
    type_var_counter: usize,
    /// Unknown type counter
    unknown_counter: usize,
    /// Substitution for type inference
    substitution: HashMap<TypeId, Type>,
    /// Collected errors
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::with_builtins(),
            type_var_counter: 0,
            unknown_counter: 0,
            substitution: HashMap::new(),
            errors: Vec::new(),
        }
    }

    /// Check an entire program
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<TypeError>> {
        // First pass: collect type definitions
        for item in &program.items {
            if let Item::TypeDef(typedef) = item {
                self.register_type_def(typedef);
            }
        }

        // Second pass: collect function signatures
        for item in &program.items {
            if let Item::FuncDef(funcdef) = item {
                self.register_func_signature(funcdef);
            }
        }

        // Third pass: type check function bodies
        for item in &program.items {
            if let Item::FuncDef(funcdef) = item {
                self.check_func_def(funcdef);
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Create a fresh type variable
    fn fresh_type_var(&mut self, name: &str) -> TypeVar {
        let id = self.type_var_counter;
        self.type_var_counter += 1;
        TypeVar {
            name: format!("{}_{}", name, id),
            id,
        }
    }

    /// Create a fresh unknown type
    fn fresh_unknown(&mut self) -> Type {
        let id = self.unknown_counter;
        self.unknown_counter += 1;
        Type::Unknown(id)
    }

    /// Register a type definition
    fn register_type_def(&mut self, typedef: &TypeDef) {
        let variants = typedef
            .variants
            .iter()
            .map(|v| VariantDef {
                name: v.name.clone(),
                fields: v
                    .fields
                    .iter()
                    .map(|f| FieldDef {
                        name: f.name.clone(),
                        typ: self.convert_type_expr(&f.typ),
                    })
                    .collect(),
            })
            .collect();

        let adt = AdtDef {
            name: typedef.name.clone(),
            type_params: typedef.type_params.clone(),
            variants,
        };

        self.env.insert_adt(adt);
    }

    /// Register a function signature (for forward references)
    fn register_func_signature(&mut self, funcdef: &FuncDef) {
        // For now, create a placeholder type - will be refined during checking
        let ret_type = self.fresh_unknown();
        let param_types: Vec<Type> = funcdef.clauses[0]
            .params
            .iter()
            .map(|_| self.fresh_unknown())
            .collect();

        let func_type = Type::Function {
            params: param_types,
            ret: Box::new(ret_type),
        };

        self.env
            .insert_var(funcdef.name.clone(), TypeScheme::monomorphic(func_type));
    }

    /// Check a function definition
    fn check_func_def(&mut self, funcdef: &FuncDef) {
        for clause in &funcdef.clauses {
            let mut local_env = self.env.extend();
            let mut bindings: HashMap<String, Binding> = HashMap::new();

            // Bind pattern variables
            for param in &clause.params {
                let param_type = self.fresh_unknown();
                self.bind_pattern(param, &param_type, &mut local_env, &mut bindings);
            }

            // Check body
            let _body_type = self.check_expr(&clause.body, &local_env, &mut bindings);

            // Check for unused affine variables (warnings in future)
            // For now we allow dropping (affine, not linear)
        }
    }

    /// Bind pattern variables to types
    fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        expected_type: &Type,
        env: &mut TypeEnv,
        bindings: &mut HashMap<String, Binding>,
    ) {
        match pattern {
            Pattern::Wildcard { .. } => {
                // Wildcard consumes the value but doesn't bind
            }
            Pattern::Var { name, .. } => {
                env.insert_var(name.clone(), TypeScheme::monomorphic(expected_type.clone()));
                bindings.insert(
                    name.clone(),
                    Binding::new(name.clone(), expected_type.clone()),
                );
            }
            Pattern::Literal { value, span } => {
                let lit_type = self.literal_type(value);
                if let Err(e) = self.unify(&lit_type, expected_type) {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: expected_type.to_string(),
                        found: lit_type.to_string(),
                        line: span.line,
                    });
                    let _ = e; // Suppress unused warning
                }
            }
            Pattern::Tuple { elements, .. } => {
                let elem_types: Vec<Type> = elements.iter().map(|_| self.fresh_unknown()).collect();
                let tuple_type = Type::Tuple(elem_types.clone());
                if self.unify(&tuple_type, expected_type).is_ok() {
                    for (elem, elem_type) in elements.iter().zip(elem_types.iter()) {
                        self.bind_pattern(elem, elem_type, env, bindings);
                    }
                }
            }
            Pattern::Tagged { tag, elements, .. } => {
                let elem_types: Vec<Type> = elements.iter().map(|_| self.fresh_unknown()).collect();
                let tagged_type = Type::Tagged {
                    tag: tag.clone(),
                    fields: elem_types.clone(),
                };
                if self.unify(&tagged_type, expected_type).is_ok() {
                    for (elem, elem_type) in elements.iter().zip(elem_types.iter()) {
                        self.bind_pattern(elem, elem_type, env, bindings);
                    }
                }
            }
            Pattern::Constructor { name, args, span } => {
                // Look up constructor in ADT definitions
                // Collect field types first to avoid borrow conflicts
                let field_types: Option<Vec<Type>> = self
                    .env
                    .adts()
                    .flat_map(|adt| adt.variants.iter())
                    .find(|v| v.name == *name)
                    .map(|v| v.fields.iter().map(|f| f.typ.clone()).collect());

                if let Some(types) = field_types {
                    // Bind arguments
                    for (arg, typ) in args.iter().zip(types.iter()) {
                        self.bind_pattern(arg, typ, env, bindings);
                    }
                } else {
                    self.errors.push(TypeError::UndefinedType {
                        name: name.clone(),
                        line: span.line,
                    });
                }
            }
            Pattern::List { elements, tail, .. } => {
                let elem_type = self.fresh_unknown();
                let list_type = Type::List(Box::new(elem_type.clone()));
                if self.unify(&list_type, expected_type).is_ok() {
                    for elem in elements {
                        self.bind_pattern(elem, &elem_type, env, bindings);
                    }
                    if let Some(tail_pat) = tail {
                        self.bind_pattern(tail_pat, &list_type, env, bindings);
                    }
                }
            }
        }
    }

    /// Check an expression and return its type
    fn check_expr(
        &mut self,
        expr: &Expr,
        env: &TypeEnv,
        bindings: &mut HashMap<String, Binding>,
    ) -> Type {
        match expr {
            Expr::Literal { value, .. } => self.literal_type(value),

            Expr::Var { name, span } => {
                // Check affine usage
                if let Some(binding) = bindings.get_mut(name) {
                    // Only enforce affine constraints for non-primitive types
                    // Primitives (Int, Float, Bool, String, Unit) are implicitly copyable
                    // Also allow unknown types (type inference will resolve them)
                    let resolved_type = self.apply_substitution(&binding.typ);
                    let is_copyable =
                        resolved_type.is_primitive() || matches!(resolved_type, Type::Unknown(_));

                    if !is_copyable && binding.usage == Usage::Consumed {
                        self.errors.push(TypeError::AffineViolation {
                            name: name.clone(),
                            line: span.line,
                        });
                        return Type::Error;
                    }

                    if !is_copyable {
                        binding.usage = Usage::Consumed;
                    }
                    return binding.typ.clone();
                }

                // Check environment
                if let Some(scheme) = env.get_var(name) {
                    return self.instantiate(scheme);
                }

                self.errors.push(TypeError::UndefinedVariable {
                    name: name.clone(),
                    line: span.line,
                });
                Type::Error
            }

            Expr::BinOp {
                op,
                left,
                right,
                span,
            } => {
                let left_type = self.check_expr(left, env, bindings);
                let right_type = self.check_expr(right, env, bindings);

                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        // Numeric operations
                        if self.unify(&left_type, &right_type).is_err() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: left_type.to_string(),
                                found: right_type.to_string(),
                                line: span.line,
                            });
                        }
                        if left_type.is_numeric() {
                            left_type
                        } else {
                            Type::Int // Default to Int
                        }
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        // Comparison operations
                        if self.unify(&left_type, &right_type).is_err() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: left_type.to_string(),
                                found: right_type.to_string(),
                                line: span.line,
                            });
                        }
                        Type::Bool
                    }
                    BinOp::And | BinOp::Or => {
                        // Boolean operations
                        if self.unify(&left_type, &Type::Bool).is_err() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: "Bool".to_string(),
                                found: left_type.to_string(),
                                line: span.line,
                            });
                        }
                        if self.unify(&right_type, &Type::Bool).is_err() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: "Bool".to_string(),
                                found: right_type.to_string(),
                                line: span.line,
                            });
                        }
                        Type::Bool
                    }
                    BinOp::Cons => {
                        // List cons
                        Type::List(Box::new(left_type))
                    }
                }
            }

            Expr::UnaryOp { op, operand, span } => {
                let operand_type = self.check_expr(operand, env, bindings);
                match op {
                    UnaryOp::Neg => {
                        if !operand_type.is_numeric() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: "numeric type".to_string(),
                                found: operand_type.to_string(),
                                line: span.line,
                            });
                        }
                        operand_type
                    }
                    UnaryOp::Not => {
                        if self.unify(&operand_type, &Type::Bool).is_err() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: "Bool".to_string(),
                                found: operand_type.to_string(),
                                line: span.line,
                            });
                        }
                        Type::Bool
                    }
                }
            }

            Expr::Call { func, args, span } => {
                let func_type = self.check_expr(func, env, bindings);
                let arg_types: Vec<Type> = args
                    .iter()
                    .map(|a| self.check_expr(a, env, bindings))
                    .collect();

                match self.apply_substitution(&func_type) {
                    Type::Function { params, ret } => {
                        if params.len() != arg_types.len() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: format!("{} arguments", params.len()),
                                found: format!("{} arguments", arg_types.len()),
                                line: span.line,
                            });
                            return Type::Error;
                        }
                        for (param, arg) in params.iter().zip(arg_types.iter()) {
                            if self.unify(param, arg).is_err() {
                                self.errors.push(TypeError::TypeMismatch {
                                    expected: param.to_string(),
                                    found: arg.to_string(),
                                    line: span.line,
                                });
                            }
                        }
                        *ret
                    }
                    Type::Unknown(id) => {
                        // Create function type and unify
                        let ret = self.fresh_unknown();
                        let func_type = Type::Function {
                            params: arg_types,
                            ret: Box::new(ret.clone()),
                        };
                        self.substitution.insert(id, func_type);
                        ret
                    }
                    other => {
                        self.errors.push(TypeError::TypeMismatch {
                            expected: "function".to_string(),
                            found: other.to_string(),
                            line: span.line,
                        });
                        Type::Error
                    }
                }
            }

            Expr::Let {
                pattern,
                value,
                body,
                ..
            } => {
                let value_type = self.check_expr(value, env, bindings);
                let mut local_env = env.clone();
                self.bind_pattern(pattern, &value_type, &mut local_env, bindings);
                self.check_expr(body, &local_env, bindings)
            }

            Expr::If {
                cond,
                then_branch,
                else_branch,
                span,
            } => {
                let cond_type = self.check_expr(cond, env, bindings);
                if self.unify(&cond_type, &Type::Bool).is_err() {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: cond_type.to_string(),
                        line: span.line,
                    });
                }

                // Clone bindings for each branch to handle affine types correctly
                let mut then_bindings = bindings.clone();
                let mut else_bindings = bindings.clone();

                let then_type = self.check_expr(then_branch, env, &mut then_bindings);
                let else_type = self.check_expr(else_branch, env, &mut else_bindings);

                if self.unify(&then_type, &else_type).is_err() {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: then_type.to_string(),
                        found: else_type.to_string(),
                        line: span.line,
                    });
                }

                // Merge binding states (both branches must consume or neither)
                for (name, binding) in bindings.iter_mut() {
                    let then_usage = then_bindings.get(name).map(|b| b.usage);
                    let else_usage = else_bindings.get(name).map(|b| b.usage);
                    if then_usage == Some(Usage::Consumed) && else_usage == Some(Usage::Consumed) {
                        binding.usage = Usage::Consumed;
                    }
                }

                then_type
            }

            Expr::Match { expr, arms, span } => {
                let expr_type = self.check_expr(expr, env, bindings);
                let mut result_type: Option<Type> = None;

                for arm in arms {
                    let mut arm_env = env.clone();
                    let mut arm_bindings = bindings.clone();
                    self.bind_pattern(&arm.pattern, &expr_type, &mut arm_env, &mut arm_bindings);
                    let arm_type = self.check_expr(&arm.body, &arm_env, &mut arm_bindings);

                    if let Some(ref expected) = result_type {
                        if self.unify(expected, &arm_type).is_err() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: expected.to_string(),
                                found: arm_type.to_string(),
                                line: span.line,
                            });
                        }
                    } else {
                        result_type = Some(arm_type);
                    }
                }

                result_type.unwrap_or(Type::Unit)
            }

            Expr::Tuple { elements, .. } => {
                let types: Vec<Type> = elements
                    .iter()
                    .map(|e| self.check_expr(e, env, bindings))
                    .collect();
                Type::Tuple(types)
            }

            Expr::Tagged { tag, elements, .. } => {
                let types: Vec<Type> = elements
                    .iter()
                    .map(|e| self.check_expr(e, env, bindings))
                    .collect();
                Type::Tagged {
                    tag: tag.clone(),
                    fields: types,
                }
            }

            Expr::Constructor { name, args, span } => {
                // Look up constructor - collect info first to avoid borrow conflicts
                let constructor_info: Option<(String, Vec<Type>)> = self
                    .env
                    .adts()
                    .flat_map(|adt| {
                        adt.variants.iter().filter_map(move |v| {
                            if v.name == *name {
                                Some((
                                    adt.name.clone(),
                                    v.fields.iter().map(|f| f.typ.clone()).collect(),
                                ))
                            } else {
                                None
                            }
                        })
                    })
                    .next();

                if let Some((adt_name, field_types)) = constructor_info {
                    // Check arguments
                    if field_types.len() != args.len() {
                        self.errors.push(TypeError::TypeMismatch {
                            expected: format!("{} arguments", field_types.len()),
                            found: format!("{} arguments", args.len()),
                            line: span.line,
                        });
                    }
                    for (arg, field_typ) in args.iter().zip(field_types.iter()) {
                        let arg_type = self.check_expr(arg, env, bindings);
                        if self.unify(&arg_type, field_typ).is_err() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: field_typ.to_string(),
                                found: arg_type.to_string(),
                                line: span.line,
                            });
                        }
                    }
                    Type::Adt {
                        name: adt_name,
                        type_args: vec![],
                    }
                } else {
                    self.errors.push(TypeError::UndefinedType {
                        name: name.clone(),
                        line: span.line,
                    });
                    Type::Error
                }
            }

            Expr::List { elements, tail, .. } => {
                let elem_type = if elements.is_empty() {
                    self.fresh_unknown()
                } else {
                    let first_type = self.check_expr(&elements[0], env, bindings);
                    for elem in &elements[1..] {
                        let elem_type = self.check_expr(elem, env, bindings);
                        let _ = self.unify(&first_type, &elem_type);
                    }
                    first_type
                };

                if let Some(tail_expr) = tail {
                    let tail_type = self.check_expr(tail_expr, env, bindings);
                    let _ = self.unify(&tail_type, &Type::List(Box::new(elem_type.clone())));
                }

                Type::List(Box::new(elem_type))
            }

            Expr::Lambda { params, body, .. } => {
                let mut lambda_env = env.clone();
                let param_types: Vec<Type> = params
                    .iter()
                    .map(|p| {
                        let t = self.fresh_unknown();
                        self.bind_pattern(p, &t, &mut lambda_env, bindings);
                        t
                    })
                    .collect();

                let body_type = self.check_expr(body, &lambda_env, bindings);

                Type::Function {
                    params: param_types,
                    ret: Box::new(body_type),
                }
            }

            Expr::Block { exprs, .. } => {
                let mut result_type = Type::Unit;
                for expr in exprs {
                    result_type = self.check_expr(expr, env, bindings);
                }
                result_type
            }

            Expr::Receive { arms, timeout, .. } => {
                let mut result_type: Option<Type> = None;

                for arm in arms {
                    let msg_type = self.fresh_unknown();
                    let mut arm_env = env.clone();
                    let mut arm_bindings = bindings.clone();
                    self.bind_pattern(&arm.pattern, &msg_type, &mut arm_env, &mut arm_bindings);
                    let arm_type = self.check_expr(&arm.body, &arm_env, &mut arm_bindings);

                    if let Some(ref expected) = result_type {
                        let _ = self.unify(expected, &arm_type);
                    } else {
                        result_type = Some(arm_type);
                    }
                }

                if let Some(timeout_clause) = timeout {
                    let timeout_type = self.check_expr(&timeout_clause.body, env, bindings);
                    if let Some(ref expected) = result_type {
                        let _ = self.unify(expected, &timeout_type);
                    } else {
                        result_type = Some(timeout_type);
                    }
                }

                result_type.unwrap_or(Type::Unit)
            }

            Expr::Become { func, args, span } => {
                // become is like a tail call - check that function exists
                if let Some(scheme) = env.get_var(func) {
                    let func_type = self.instantiate(scheme);
                    if let Type::Function { params, ret: _ } = func_type {
                        if params.len() != args.len() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: format!("{} arguments", params.len()),
                                found: format!("{} arguments", args.len()),
                                line: span.line,
                            });
                        }
                        for (param, arg) in params.iter().zip(args.iter()) {
                            let arg_type = self.check_expr(arg, env, bindings);
                            let _ = self.unify(param, &arg_type);
                        }
                    }
                } else {
                    self.errors.push(TypeError::UndefinedVariable {
                        name: func.clone(),
                        line: span.line,
                    });
                }
                // become never returns (it's a tail call)
                Type::Unit
            }

            Expr::Spawn { func, args, span } => {
                // spawn returns Actor[MessageType]
                if let Some(scheme) = env.get_var(func) {
                    let func_type = self.instantiate(scheme);
                    if let Type::Function { params, ret: _ } = func_type {
                        if params.len() != args.len() {
                            self.errors.push(TypeError::TypeMismatch {
                                expected: format!("{} arguments", params.len()),
                                found: format!("{} arguments", args.len()),
                                line: span.line,
                            });
                        }
                        for (param, arg) in params.iter().zip(args.iter()) {
                            let arg_type = self.check_expr(arg, env, bindings);
                            let _ = self.unify(param, &arg_type);
                        }
                    }
                } else {
                    self.errors.push(TypeError::UndefinedVariable {
                        name: func.clone(),
                        line: span.line,
                    });
                }
                // Return Actor type (message type is unknown/inferred)
                Type::Actor(Box::new(self.fresh_unknown()))
            }

            Expr::Send {
                target,
                message,
                span,
            } => {
                let target_type = self.check_expr(target, env, bindings);
                let message_type = self.check_expr(message, env, bindings);

                // Target must be Actor[M] where M matches message type
                let expected_actor = Type::Actor(Box::new(message_type));
                if self.unify(&target_type, &expected_actor).is_err() {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: expected_actor.to_string(),
                        found: target_type.to_string(),
                        line: span.line,
                    });
                }

                Type::Unit
            }

            Expr::SelfRef { .. } => {
                // self() returns Actor[M] where M is the current actor's message type
                Type::Actor(Box::new(self.fresh_unknown()))
            }

            Expr::ModuleCall {
                module,
                func,
                args,
                span,
            } => {
                // Type check arguments
                for arg in args {
                    self.check_expr(arg, env, bindings);
                }

                // Return type based on module and function
                match (module.as_str(), func.as_str()) {
                    // io: module
                    ("io", "print") | ("io", "println") => Type::Unit,
                    ("io", "read_line") => Type::String,

                    // str: module
                    ("str", "concat") => Type::String,
                    ("str", "length") => Type::Int,
                    ("str", "split") => Type::List(Box::new(Type::String)),
                    ("str", "join") => Type::String,
                    ("str", "slice") => Type::String,

                    // int: module
                    ("int", "to_string") => Type::String,
                    ("int", "parse") => self.fresh_unknown(),
                    ("int", "abs") | ("int", "min") | ("int", "max") => Type::Int,

                    // float: module
                    ("float", "to_string") => Type::String,
                    ("float", "parse") => self.fresh_unknown(),
                    ("float", "floor") | ("float", "ceil") | ("float", "round") => Type::Int,

                    // list: module
                    ("list", "head") => self.fresh_unknown(),
                    ("list", "tail") => self.fresh_unknown(),
                    ("list", "length") => Type::Int,
                    ("list", "map") => self.fresh_unknown(),
                    ("list", "filter") => self.fresh_unknown(),
                    ("list", "fold") => self.fresh_unknown(),
                    ("list", "append") => self.fresh_unknown(),
                    ("list", "reverse") => self.fresh_unknown(),

                    // actor: module
                    ("actor", "self") => Type::Actor(Box::new(self.fresh_unknown())),
                    ("actor", "send") => Type::Unit,
                    ("actor", "spawn") => Type::Actor(Box::new(self.fresh_unknown())),

                    // Unknown module/function
                    _ => {
                        self.errors.push(TypeError::UnknownFunction {
                            name: format!("{}:{}", module, func),
                            line: span.line,
                        });
                        self.fresh_unknown()
                    }
                }
            }

            Expr::Supervised { body, .. } => {
                // Type check supervised block - returns unit
                for expr in body {
                    self.check_expr(expr, env, bindings);
                }
                Type::Unit
            }
        }
    }

    /// Get the type of a literal
    fn literal_type(&self, lit: &Literal) -> Type {
        match lit {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
            Literal::String(_) => Type::String,
            Literal::Unit => Type::Unit,
        }
    }

    /// Convert AST type expression to Type
    fn convert_type_expr(&mut self, type_expr: &TypeExpr) -> Type {
        match type_expr {
            TypeExpr::Named { name, args, .. } => {
                let type_args: Vec<Type> = args.iter().map(|a| self.convert_type_expr(a)).collect();
                match name.as_str() {
                    "Int" => Type::Int,
                    "Float" => Type::Float,
                    "Bool" => Type::Bool,
                    "String" => Type::String,
                    "List" => {
                        if type_args.len() == 1 {
                            Type::List(Box::new(type_args[0].clone()))
                        } else {
                            Type::List(Box::new(self.fresh_unknown()))
                        }
                    }
                    "Map" => {
                        if type_args.len() == 2 {
                            Type::Map {
                                key: Box::new(type_args[0].clone()),
                                value: Box::new(type_args[1].clone()),
                            }
                        } else {
                            Type::Map {
                                key: Box::new(self.fresh_unknown()),
                                value: Box::new(self.fresh_unknown()),
                            }
                        }
                    }
                    "Channel" => {
                        if type_args.len() == 1 {
                            Type::Channel(Box::new(type_args[0].clone()))
                        } else {
                            Type::Channel(Box::new(self.fresh_unknown()))
                        }
                    }
                    _ => {
                        // User-defined type
                        Type::Adt {
                            name: name.clone(),
                            type_args,
                        }
                    }
                }
            }
            TypeExpr::Actor { message_type, .. } => {
                Type::Actor(Box::new(self.convert_type_expr(message_type)))
            }
            TypeExpr::Tuple { elements, .. } => {
                Type::Tuple(elements.iter().map(|e| self.convert_type_expr(e)).collect())
            }
            TypeExpr::Function { params, ret, .. } => Type::Function {
                params: params.iter().map(|p| self.convert_type_expr(p)).collect(),
                ret: Box::new(self.convert_type_expr(ret)),
            },
            TypeExpr::Unit { .. } => Type::Unit,
        }
    }

    /// Instantiate a type scheme with fresh type variables
    fn instantiate(&mut self, scheme: &TypeScheme) -> Type {
        let mut subst: HashMap<TypeVar, Type> = HashMap::new();
        for var in &scheme.type_vars {
            subst.insert(var.clone(), Type::Var(self.fresh_type_var(&var.name)));
        }

        Self::apply_type_subst(&scheme.typ, &subst)
    }

    /// Apply a type variable substitution (standalone function, no instance state needed)
    fn apply_type_subst(typ: &Type, subst: &HashMap<TypeVar, Type>) -> Type {
        match typ {
            Type::Var(v) => subst.get(v).cloned().unwrap_or_else(|| typ.clone()),
            Type::Function { params, ret } => Type::Function {
                params: params
                    .iter()
                    .map(|p| Self::apply_type_subst(p, subst))
                    .collect(),
                ret: Box::new(Self::apply_type_subst(ret, subst)),
            },
            Type::Tuple(elems) => Type::Tuple(
                elems
                    .iter()
                    .map(|e| Self::apply_type_subst(e, subst))
                    .collect(),
            ),
            Type::List(elem) => Type::List(Box::new(Self::apply_type_subst(elem, subst))),
            Type::Map { key, value } => Type::Map {
                key: Box::new(Self::apply_type_subst(key, subst)),
                value: Box::new(Self::apply_type_subst(value, subst)),
            },
            Type::Actor(msg) => Type::Actor(Box::new(Self::apply_type_subst(msg, subst))),
            Type::Channel(elem) => Type::Channel(Box::new(Self::apply_type_subst(elem, subst))),
            Type::Tagged { tag, fields } => Type::Tagged {
                tag: tag.clone(),
                fields: fields
                    .iter()
                    .map(|f| Self::apply_type_subst(f, subst))
                    .collect(),
            },
            Type::Adt { name, type_args } => Type::Adt {
                name: name.clone(),
                type_args: type_args
                    .iter()
                    .map(|a| Self::apply_type_subst(a, subst))
                    .collect(),
            },
            _ => typ.clone(),
        }
    }

    /// Apply the current substitution to a type
    fn apply_substitution(&self, typ: &Type) -> Type {
        match typ {
            Type::Unknown(id) => {
                if let Some(t) = self.substitution.get(id) {
                    self.apply_substitution(t)
                } else {
                    typ.clone()
                }
            }
            Type::Function { params, ret } => Type::Function {
                params: params.iter().map(|p| self.apply_substitution(p)).collect(),
                ret: Box::new(self.apply_substitution(ret)),
            },
            Type::Tuple(elems) => {
                Type::Tuple(elems.iter().map(|e| self.apply_substitution(e)).collect())
            }
            Type::List(elem) => Type::List(Box::new(self.apply_substitution(elem))),
            Type::Map { key, value } => Type::Map {
                key: Box::new(self.apply_substitution(key)),
                value: Box::new(self.apply_substitution(value)),
            },
            Type::Actor(msg) => Type::Actor(Box::new(self.apply_substitution(msg))),
            Type::Channel(elem) => Type::Channel(Box::new(self.apply_substitution(elem))),
            Type::Tagged { tag, fields } => Type::Tagged {
                tag: tag.clone(),
                fields: fields.iter().map(|f| self.apply_substitution(f)).collect(),
            },
            Type::Adt { name, type_args } => Type::Adt {
                name: name.clone(),
                type_args: type_args
                    .iter()
                    .map(|a| self.apply_substitution(a))
                    .collect(),
            },
            _ => typ.clone(),
        }
    }

    /// Unify two types
    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), TypeError> {
        let t1 = self.apply_substitution(t1);
        let t2 = self.apply_substitution(t2);

        match (&t1, &t2) {
            (Type::Int, Type::Int) => Ok(()),
            (Type::Float, Type::Float) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::String, Type::String) => Ok(()),
            (Type::Unit, Type::Unit) => Ok(()),
            (Type::Error, _) | (_, Type::Error) => Ok(()),

            (Type::Unknown(id), t) | (t, Type::Unknown(id)) => {
                if let Type::Unknown(id2) = t {
                    if id == id2 {
                        return Ok(());
                    }
                }
                self.substitution.insert(*id, t.clone());
                Ok(())
            }

            (Type::Var(v1), Type::Var(v2)) if v1 == v2 => Ok(()),

            (
                Type::Function {
                    params: p1,
                    ret: r1,
                },
                Type::Function {
                    params: p2,
                    ret: r2,
                },
            ) => {
                if p1.len() != p2.len() {
                    return Err(TypeError::UnificationError {
                        t1: t1.to_string(),
                        t2: t2.to_string(),
                    });
                }
                for (a, b) in p1.iter().zip(p2.iter()) {
                    self.unify(a, b)?;
                }
                self.unify(r1, r2)
            }

            (Type::Tuple(e1), Type::Tuple(e2)) => {
                if e1.len() != e2.len() {
                    return Err(TypeError::UnificationError {
                        t1: t1.to_string(),
                        t2: t2.to_string(),
                    });
                }
                for (a, b) in e1.iter().zip(e2.iter()) {
                    self.unify(a, b)?;
                }
                Ok(())
            }

            (Type::List(e1), Type::List(e2)) => self.unify(e1, e2),

            (Type::Map { key: k1, value: v1 }, Type::Map { key: k2, value: v2 }) => {
                self.unify(k1, k2)?;
                self.unify(v1, v2)
            }

            (Type::Actor(m1), Type::Actor(m2)) => self.unify(m1, m2),

            (Type::Channel(e1), Type::Channel(e2)) => self.unify(e1, e2),

            (
                Type::Tagged {
                    tag: t1,
                    fields: f1,
                },
                Type::Tagged {
                    tag: t2,
                    fields: f2,
                },
            ) => {
                if t1 != t2 || f1.len() != f2.len() {
                    return Err(TypeError::UnificationError {
                        t1: Type::Tagged {
                            tag: t1.clone(),
                            fields: f1.clone(),
                        }
                        .to_string(),
                        t2: Type::Tagged {
                            tag: t2.clone(),
                            fields: f2.clone(),
                        }
                        .to_string(),
                    });
                }
                for (a, b) in f1.iter().zip(f2.iter()) {
                    self.unify(a, b)?;
                }
                Ok(())
            }

            (
                Type::Adt {
                    name: n1,
                    type_args: a1,
                },
                Type::Adt {
                    name: n2,
                    type_args: a2,
                },
            ) => {
                if n1 != n2 || a1.len() != a2.len() {
                    return Err(TypeError::UnificationError {
                        t1: t1.to_string(),
                        t2: t2.to_string(),
                    });
                }
                for (a, b) in a1.iter().zip(a2.iter()) {
                    self.unify(a, b)?;
                }
                Ok(())
            }

            _ => Err(TypeError::UnificationError {
                t1: t1.to_string(),
                t2: t2.to_string(),
            }),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn check(source: &str) -> Result<(), Vec<TypeError>> {
        let program = parse(source).expect("Parse error");
        let mut checker = TypeChecker::new();
        checker.check_program(&program)
    }

    #[test]
    fn test_simple_function() {
        let result = check(": double (x) -> x * 2 ;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let result = check(": bad () -> 1 + true ;");
        assert!(result.is_err());
    }

    #[test]
    fn test_undefined_variable() {
        let result = check(": bad () -> x ;");
        assert!(result.is_err());
    }

    #[test]
    fn test_let_binding() {
        let result = check(": example (x) -> let y = x * 2 y + 1 ;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_expr() {
        let result = check(": abs (x) -> if x < 0 then 0 - x else x ;");
        assert!(result.is_ok());
    }
}
