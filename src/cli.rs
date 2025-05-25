use clap::{CommandFactory, FromArgMatches, Parser};
use std::path::{Path, PathBuf};

/// A simple CLI for processing C files
#[derive(Parser, Debug)]
#[command(author, version, about)]
pub struct Cli {
    /// Path to the C source file
    #[arg(value_name = "FILE")]
    pub file: PathBuf,

    /// Save the preprocessed output
    #[arg(short = 'p', long = "preprocess")]
    pub save_preprocessed: bool,

    /// Save the compiled output
    #[arg(short = 'c', long = "compile")]
    pub save_compiled: bool,

    /// Stop after lexing
    #[arg(long = "lex")]
    pub lex: bool,

    /// Stop after parsing
    #[arg(long = "parse")]
    pub parse: bool,

    /// Stop after code generation
    #[arg(long = "codegen")]
    pub codegen: bool,
}

impl Cli {
    pub fn parse_and_validate() -> Self {
        // Ignore unknown args
        let matches = Cli::command().ignore_errors(true).get_matches();

        let cli = Cli::from_arg_matches(&matches).unwrap_or_else(|e| e.exit());

        if !cli.file.exists() {
            eprintln!("Error: File '{}' does not exist.", cli.file.display());
            std::process::exit(1);
        }

        if !cli.file.is_file() {
            eprintln!("Error: '{}' is not a file.", cli.file.display());
            std::process::exit(1);
        }

        if cli.file.extension().and_then(|ext| ext.to_str()) != Some("c") {
            eprintln!("Error: File '{}' is not a .c file.", cli.file.display());
            std::process::exit(1);
        }

        cli
    }
}
