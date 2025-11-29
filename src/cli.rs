use clap::{CommandFactory, FromArgMatches, Parser};
use std::{fs, path::PathBuf};

/// Internal CLI structure for parsing arguments. It is used to get the raw arguments from the user that then will be validated.
#[derive(Parser, Debug)]
#[command(author, version, about)]
struct CliArgs {
    /// Path to the C source file
    #[arg(value_name = "FILE")]
    input_file: PathBuf,

    /// Output folder path. If not provided, defaults to the parent directory of the input file
    #[arg(short = 'o', long = "output-folder")]
    output_folder: Option<PathBuf>,

    /// Save the preprocessed output
    #[arg(short = 'p', long = "preprocess", default_value_t = false)]
    save_preprocessed: bool,

    /// Save the compiled output
    #[arg(short = 'c', long = "compile", default_value_t = false)]
    save_compiled: bool,

    /// Stop after lexing
    #[arg(long = "lex", default_value_t = false)]
    stop_after_lexing: bool,

    /// Stop after parsing
    #[arg(long = "parse", default_value_t = false)]
    stop_after_parsing: bool,

    /// Stop after printing the program AST
    #[arg(long = "print-program-ast", default_value_t = true)]
    print_program_ast: bool,

    /// Stop after code generation
    #[arg(long = "codegen", default_value_t = false)]
    stop_after_codegen: bool,

    /// Print the intermediate representation (IR)
    #[arg(long = "print-ir", default_value_t = true)]
    print_ir: bool,

    /// Stop after generating the intermediate representation (IR)
    #[arg(long = "tacky", default_value_t = false)]
    stop_after_ir: bool,
}

/// Parsed and validated CLI structure
#[derive(Debug)]
pub struct Cli {
    /// Path to the C source file
    pub input_file: PathBuf,
    /// Resolved output folder path (computed field)
    pub output_folder: PathBuf,
    /// Save the preprocessed output
    pub save_preprocessed: bool,
    /// Save the compiled output
    pub save_compiled: bool,
    /// Stop after lexing
    pub stop_after_lexing: bool,
    /// Stop after parsing
    pub stop_after_parsing: bool,
    /// Print the program AST
    pub print_program_ast: bool,
    /// Stop after code generation
    pub stop_after_codegen: bool,
    /// Print the intermediate representation (IR)
    pub print_ir: bool,
    /// Stop after generating the intermediate representation (IR)
    pub stop_after_ir: bool,
}

impl Cli {
    pub fn parse_and_validate() -> Self {
        // Ignore unknown args
        let matches = CliArgs::command().ignore_errors(true).get_matches();

        let cli_args = CliArgs::from_arg_matches(&matches).unwrap_or_else(|e| e.exit());

        if !cli_args.input_file.exists() {
            eprintln!(
                "Error: File '{}' does not exist.",
                cli_args.input_file.display()
            );
            std::process::exit(1);
        }

        if !cli_args.input_file.is_file() {
            eprintln!("Error: '{}' is not a file.", cli_args.input_file.display());
            std::process::exit(1);
        }

        if cli_args.input_file.extension().and_then(|ext| ext.to_str()) != Some("c") {
            eprintln!(
                "Error: File '{}' is not a .c file.",
                cli_args.input_file.display()
            );
            std::process::exit(1);
        }

        // Determine output folder: use provided path or default to input file's parent directory if not provided
        let output_folder = match &cli_args.output_folder {
            Some(path) => path.clone(),
            None => cli_args
                .input_file
                .parent()
                .unwrap_or_else(|| {
                    eprintln!("Error: Could not determine parent directory of input file");
                    std::process::exit(1);
                })
                .to_path_buf(),
        };

        // Create output folder if it doesn't exist
        if !output_folder.exists() {
            if let Err(e) = fs::create_dir_all(&output_folder) {
                eprintln!(
                    "Error: Could not create output directory '{}': {}",
                    output_folder.display(),
                    e
                );
                std::process::exit(1);
            }
        }

        // Return a new Cli with the resolved output_folder
        Cli {
            input_file: cli_args.input_file,
            output_folder: output_folder,
            save_preprocessed: cli_args.save_preprocessed,
            save_compiled: cli_args.save_compiled,
            stop_after_lexing: cli_args.stop_after_lexing,
            stop_after_parsing: cli_args.stop_after_parsing,
            print_program_ast: cli_args.print_program_ast,
            stop_after_codegen: cli_args.stop_after_codegen,
            print_ir: cli_args.print_ir,
            stop_after_ir: cli_args.stop_after_ir,
        }
    }
}
