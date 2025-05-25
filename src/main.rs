mod cli;
mod lexer;

use crate::cli::Cli;
use crate::lexer::{tokenize, LexError};
use std::env;
use std::path::{Path, PathBuf};
use std::process::{exit, Command};

fn main() {
    // Get the input arguments
    let cli = Cli::parse_and_validate();

    // Execute the C preprocessor
    let tmp_preprocessed = preprocess(&cli.file);

    // Compile the preprocessed file
    let tmp_assembly = compile(&tmp_preprocessed, cli.lex, cli.parse, cli.codegen);

    // Check if the user wants to stop after lexing, parsing, or code generation
    if cli.lex || cli.parse || cli.codegen {
        return;
    }

    // Assemble and link the assembly file
    assemble(&tmp_assembly, &tmp_preprocessed);

    // Clean up temporary files
    if !cli.save_preprocessed {
        std::fs::remove_file(&tmp_preprocessed).expect("Failed to delete preprocessed file");
    }

    if !cli.save_compiled {
        std::fs::remove_file(&tmp_assembly).expect("Failed to delete assembly file");
    }
}

fn preprocess(input_file: &PathBuf) -> PathBuf {
    let tmp_preprocessed: PathBuf = env::temp_dir().join(format!(
        "{}_preprocessed.c",
        input_file.file_name().unwrap().to_str().unwrap()
    ));

    let output = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(input_file.to_str().unwrap())
        .arg("-o")
        .arg(tmp_preprocessed.to_str().unwrap())
        .output()
        .expect("Failed to execute preprocessor");

    if !output.status.success() {
        eprintln!("Preprocessing failed!");
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        exit(1);
    }

    tmp_preprocessed
}

fn compile(
    preprocessed_file: &PathBuf,
    finish_at_lexing: bool,
    finish_at_parsing: bool,
    finish_at_codegen: bool,
) -> PathBuf {
    let tmp_assembly: PathBuf = env::temp_dir().join(format!(
        "{}_compiled.s",
        preprocessed_file.file_name().unwrap().to_str().unwrap()
    ));

    let (tokens, errors) = tokenize(&std::fs::read_to_string(preprocessed_file).unwrap());

    if !errors.is_empty() {
        for error in errors {
            eprintln!("Lexical error: {}", error);
        }
        exit(1);
    }

    if finish_at_lexing {
        return tmp_assembly;
    }
    // todo Parse and compile

    tmp_assembly
}

fn assemble(assembled_file: &PathBuf, input_file: &PathBuf) -> PathBuf {
    let binary_file: PathBuf = PathBuf::from(input_file.file_stem().unwrap().to_str().unwrap());

    let output = Command::new("gcc")
        .arg(assembled_file.to_str().unwrap())
        .arg("-o")
        .arg(binary_file.to_str().unwrap())
        .output()
        .expect("Failed to execute assembler");

    if !output.status.success() {
        eprintln!("Assembly failed!");
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        exit(1);
    }

    binary_file
}
