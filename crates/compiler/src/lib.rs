//! seq-actor Compiler Library
//!
//! This crate provides the compiler for the seq-actor language.
//!
//! # Pipeline
//!
//! ```text
//! Source (.act)
//!     ↓
//! [LEXER] → Tokens
//!     ↓
//! [PARSER] → AST
//!     ↓
//! [TYPE CHECKER] → Typed AST
//!     ↓
//! [CODEGEN] → LLVM IR
//!     ↓
//! [CLANG] → Binary
//! ```

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod types;
pub mod typechecker;
pub mod codegen;

pub use ast::*;
pub use lexer::{Lexer, Token, TokenKind};
pub use parser::{parse, Parser, ParseError};
pub use types::*;
pub use typechecker::{TypeChecker, TypeError};
pub use codegen::CodeGen;

use thiserror::Error;
use std::path::Path;
use std::process::Command;

/// Compiler errors
#[derive(Error, Debug)]
pub enum CompileError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Parse error: {0}")]
    Parse(#[from] ParseError),

    #[error("Type errors:\n{}", .0.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("\n"))]
    Type(Vec<TypeError>),

    #[error("Codegen error: {0}")]
    Codegen(String),

    #[error("Clang error: {0}")]
    Clang(String),
}

/// Compiler configuration
#[derive(Debug, Clone)]
pub struct CompilerConfig {
    /// Output path for the binary
    pub output: Option<String>,
    /// Emit LLVM IR only (don't compile)
    pub emit_ir: bool,
    /// Optimization level (0-3)
    pub opt_level: u8,
    /// Include debug info
    pub debug: bool,
    /// Verbose output
    pub verbose: bool,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            output: None,
            emit_ir: false,
            opt_level: 2,
            debug: false,
            verbose: false,
        }
    }
}

/// The seq-actor compiler
pub struct Compiler {
    config: CompilerConfig,
}

impl Compiler {
    pub fn new(config: CompilerConfig) -> Self {
        Self { config }
    }

    /// Compile source code to a binary
    pub fn compile(&self, source: &str, source_path: &Path) -> Result<String, CompileError> {
        // Parse
        if self.config.verbose {
            eprintln!("Parsing...");
        }
        let program = parse(source)?;

        // Type check
        if self.config.verbose {
            eprintln!("Type checking...");
        }
        let mut checker = TypeChecker::new();
        checker.check_program(&program).map_err(CompileError::Type)?;

        // Generate IR
        if self.config.verbose {
            eprintln!("Generating LLVM IR...");
        }
        let mut codegen = CodeGen::new();
        let ir = codegen.generate(&program);

        if self.config.emit_ir {
            return Ok(ir);
        }

        // Write IR to temp file
        let ir_path = source_path.with_extension("ll");
        std::fs::write(&ir_path, &ir)?;

        // Compile with clang
        if self.config.verbose {
            eprintln!("Compiling with clang...");
        }

        let output_path = self.config.output.clone().unwrap_or_else(|| {
            source_path
                .with_extension("")
                .to_string_lossy()
                .to_string()
        });

        let opt_flag = format!("-O{}", self.config.opt_level.min(3));

        // Find the runtime library
        // Look relative to the compiler executable, or use a known path
        let runtime_lib = find_runtime_library();

        let mut cmd = Command::new("clang");
        cmd.arg(&opt_flag)
            .arg("-o")
            .arg(&output_path)
            .arg(&ir_path);

        // Link against the runtime library if found
        if let Some(ref lib_path) = runtime_lib {
            cmd.arg(lib_path);
        }

        if self.config.debug {
            cmd.arg("-g");
        }

        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(CompileError::Clang(stderr.to_string()));
        }

        // Clean up IR file unless verbose
        if !self.config.verbose {
            let _ = std::fs::remove_file(&ir_path);
        }

        Ok(output_path)
    }

    /// Check source code for errors without compiling
    pub fn check(&self, source: &str) -> Result<(), CompileError> {
        let program = parse(source)?;
        let mut checker = TypeChecker::new();
        checker.check_program(&program).map_err(CompileError::Type)?;
        Ok(())
    }

    /// Parse source code and return AST
    pub fn parse(&self, source: &str) -> Result<Program, CompileError> {
        Ok(parse(source)?)
    }

    /// Generate LLVM IR from source
    pub fn emit_ir(&self, source: &str) -> Result<String, CompileError> {
        let program = parse(source)?;
        let mut checker = TypeChecker::new();
        checker.check_program(&program).map_err(CompileError::Type)?;
        let mut codegen = CodeGen::new();
        Ok(codegen.generate(&program))
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new(CompilerConfig::default())
    }
}

/// Find the runtime library for linking
fn find_runtime_library() -> Option<String> {
    // Try several locations in order of preference:

    // 1. Environment variable
    if let Ok(path) = std::env::var("SEQ_ACTOR_RUNTIME") {
        if Path::new(&path).exists() {
            return Some(path);
        }
    }

    // 2. Relative to the compiler executable
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            let lib_path = exe_dir.join("libseq_actor_runtime.a");
            if lib_path.exists() {
                return Some(lib_path.to_string_lossy().to_string());
            }
        }
    }

    // 3. Current directory's target/release
    let dev_path = Path::new("target/release/libseq_actor_runtime.a");
    if dev_path.exists() {
        return Some(dev_path.to_string_lossy().to_string());
    }

    // 4. Absolute path for development
    let abs_dev_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(|p| p.join("target/release/libseq_actor_runtime.a"));
    if let Some(path) = abs_dev_path {
        if path.exists() {
            return Some(path.to_string_lossy().to_string());
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_hello() {
        let source = r#": main () -> println("Hello, World!") ;"#;
        let compiler = Compiler::default();
        let ir = compiler.emit_ir(source).unwrap();
        assert!(ir.contains("@main"));
    }

    #[test]
    fn test_check_valid() {
        let source = ": double (x) -> x * 2 ;";
        let compiler = Compiler::default();
        assert!(compiler.check(source).is_ok());
    }

    #[test]
    fn test_check_invalid() {
        let source = ": bad () -> undefined_var ;";
        let compiler = Compiler::default();
        assert!(compiler.check(source).is_err());
    }
}
