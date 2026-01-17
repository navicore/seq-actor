//! seq-actor Compiler CLI
//!
//! Usage:
//!   seqact build <file.act>        Compile to binary
//!   seqact run <file.act>          Compile and run
//!   seqact check <file.act>        Type check without compiling
//!   seqact emit-ir <file.act>      Output LLVM IR

use seq_actor_compiler::{Compiler, CompilerConfig, CompileError};
use std::env;
use std::path::Path;
use std::process::{Command, ExitCode};

fn print_usage() {
    eprintln!("seq-actor compiler v0.1.0");
    eprintln!();
    eprintln!("Usage:");
    eprintln!("  seqact build <file.act> [-o output] [--emit-ir] [-O0|-O1|-O2|-O3] [-g] [-v]");
    eprintln!("  seqact run <file.act> [args...]");
    eprintln!("  seqact check <file.act>");
    eprintln!("  seqact emit-ir <file.act>");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  build      Compile source file to binary");
    eprintln!("  run        Compile and execute");
    eprintln!("  check      Type check without compiling");
    eprintln!("  emit-ir    Output LLVM IR to stdout");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -o <file>  Output file path");
    eprintln!("  --emit-ir  Output LLVM IR instead of binary");
    eprintln!("  -O0...-O3  Optimization level (default: -O2)");
    eprintln!("  -g         Include debug info");
    eprintln!("  -v         Verbose output");
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        return ExitCode::from(1);
    }

    let command = &args[1];

    match command.as_str() {
        "build" => cmd_build(&args[2..]),
        "run" => cmd_run(&args[2..]),
        "check" => cmd_check(&args[2..]),
        "emit-ir" => cmd_emit_ir(&args[2..]),
        "--help" | "-h" | "help" => {
            print_usage();
            ExitCode::SUCCESS
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            print_usage();
            ExitCode::from(1)
        }
    }
}

fn cmd_build(args: &[String]) -> ExitCode {
    if args.is_empty() {
        eprintln!("Error: No input file specified");
        return ExitCode::from(1);
    }

    let mut config = CompilerConfig::default();
    let mut input_file = None;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i < args.len() {
                    config.output = Some(args[i].clone());
                }
            }
            "--emit-ir" => config.emit_ir = true,
            "-O0" => config.opt_level = 0,
            "-O1" => config.opt_level = 1,
            "-O2" => config.opt_level = 2,
            "-O3" => config.opt_level = 3,
            "-g" => config.debug = true,
            "-v" | "--verbose" => config.verbose = true,
            arg if !arg.starts_with('-') => {
                input_file = Some(arg.to_string());
            }
            arg => {
                eprintln!("Unknown option: {}", arg);
                return ExitCode::from(1);
            }
        }
        i += 1;
    }

    let input_file = match input_file {
        Some(f) => f,
        None => {
            eprintln!("Error: No input file specified");
            return ExitCode::from(1);
        }
    };

    match compile_file(&input_file, config) {
        Ok(output) => {
            if output.ends_with(".ll") || output.contains('\n') {
                // IR output
                println!("{}", output);
            } else {
                println!("Compiled: {}", output);
            }
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::from(1)
        }
    }
}

fn cmd_run(args: &[String]) -> ExitCode {
    if args.is_empty() {
        eprintln!("Error: No input file specified");
        return ExitCode::from(1);
    }

    let input_file = &args[0];
    let program_args = &args[1..];

    let config = CompilerConfig {
        output: Some("/tmp/seqact_run".to_string()),
        ..Default::default()
    };

    match compile_file(input_file, config) {
        Ok(output_path) => {
            // Run the compiled program
            let status = Command::new(&output_path)
                .args(program_args)
                .status();

            match status {
                Ok(s) => {
                    if s.success() {
                        ExitCode::SUCCESS
                    } else {
                        ExitCode::from(s.code().unwrap_or(1) as u8)
                    }
                }
                Err(e) => {
                    eprintln!("Failed to run program: {}", e);
                    ExitCode::from(1)
                }
            }
        }
        Err(e) => {
            eprintln!("Compilation error: {}", e);
            ExitCode::from(1)
        }
    }
}

fn cmd_check(args: &[String]) -> ExitCode {
    if args.is_empty() {
        eprintln!("Error: No input file specified");
        return ExitCode::from(1);
    }

    let input_file = &args[0];

    match check_file(input_file) {
        Ok(()) => {
            println!("OK: {}", input_file);
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("{}", e);
            ExitCode::from(1)
        }
    }
}

fn cmd_emit_ir(args: &[String]) -> ExitCode {
    if args.is_empty() {
        eprintln!("Error: No input file specified");
        return ExitCode::from(1);
    }

    let input_file = &args[0];

    match emit_ir_file(input_file) {
        Ok(ir) => {
            println!("{}", ir);
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("{}", e);
            ExitCode::from(1)
        }
    }
}

fn compile_file(path: &str, config: CompilerConfig) -> Result<String, CompileError> {
    let source = std::fs::read_to_string(path)?;
    let compiler = Compiler::new(config);
    compiler.compile(&source, Path::new(path))
}

fn check_file(path: &str) -> Result<(), CompileError> {
    let source = std::fs::read_to_string(path)?;
    let compiler = Compiler::default();
    compiler.check(&source)
}

fn emit_ir_file(path: &str) -> Result<String, CompileError> {
    let source = std::fs::read_to_string(path)?;
    let compiler = Compiler::default();
    compiler.emit_ir(&source)
}
