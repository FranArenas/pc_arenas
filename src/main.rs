use std::path::{Path, PathBuf};
use std::process::{exit, Command};

use pc_arenas::{generate_code, parse, tokenize, Cli, CodeEmitter};

fn main() {
    // Get the input arguments
    let cli = Cli::parse_and_validate();

    // Get the needed file paths
    let input_stem = cli
        .input_file
        .file_stem()
        .expect("Failed to get input file stem")
        .to_str()
        .expect("Failed to convert input file stem to string");
    let preprocessed_filepath = cli
        .output_folder
        .join(format!("{}_preprocessed.c", input_stem));
    let compiled_filepath = cli.output_folder.join(input_stem).with_extension("s");
    let executable_filepath = cli.output_folder.join(input_stem);

    eprint!("input file: {}\n", cli.input_file.display());
    eprint!("preprocessed file: {}\n", preprocessed_filepath.display());
    eprint!("compiled file: {}\n", compiled_filepath.display());
    eprint!("executable file: {}\n", executable_filepath.display());
    if input_stem.contains("tabs") {}

    // Execute the C preprocessor
    preprocess(&cli.input_file, &preprocessed_filepath);

    // Compile the preprocessed file
    compile(
        &preprocessed_filepath,
        &compiled_filepath,
        cli.lex,
        cli.parse,
        cli.print_program_ast,
        cli.print_assembly_ast,
    );

    // Check if the user wants to stop after lexing, parsing, or code generation
    if cli.lex || cli.parse || cli.codegen {
        return;
    }

    // Assemble and link the assembly file
    assemble(&compiled_filepath, &executable_filepath);

    // Clean up temporary files
    if !cli.save_preprocessed {
        std::fs::remove_file(&preprocessed_filepath).expect("Failed to delete preprocessed file");
    }

    if !cli.save_compiled {
        std::fs::remove_file(&compiled_filepath).expect("Failed to delete assembly file");
    }
}

fn preprocess<P: AsRef<Path>>(input_file: P, output_file: P) {
    let output = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(input_file.as_ref())
        .arg("-o")
        .arg(output_file.as_ref())
        .output()
        .expect("Failed to execute preprocessor");

    if !output.status.success() {
        eprintln!("Preprocessing failed!");
        eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
        exit(1);
    }
}

fn compile<P: AsRef<Path>>(
    preprocessed_file: P,
    output_filename: P,
    finish_at_lexing: bool,
    finish_at_parsing: bool,
    print_program_ast: bool,
    print_assembly_ast: bool,
) {
    // Lexing
    let (tokens, lex_errors) = tokenize(&std::fs::read_to_string(preprocessed_file).unwrap());

    if !lex_errors.is_empty() {
        for error in lex_errors {
            eprintln!("Lexical error: {}", error);
        }
        exit(1);
    }

    if finish_at_lexing {
        return;
    }

    // Parsing
    let ast = parse(tokens).unwrap_or_else(|parse_error| {
        for error in &parse_error {
            eprintln!("Parsing error: {}", error);
        }
        std::process::exit(1);
    });

    if print_program_ast {
        println!("{}", ast);
    }

    if finish_at_parsing {
        return;
    }

    // Code generation
    let assembly_ast = generate_code(&ast).unwrap_or_else(|codegen_error| {
        eprintln!("Code generation error: {}", codegen_error.message);
        std::process::exit(1);
    });
    if print_assembly_ast {
        println!("{}", assembly_ast);
    }

    let mut code_emitter =
        CodeEmitter::from_path(output_filename).unwrap_or_else(|code_emission_error| {
            eprintln!("Code emission error: {}", code_emission_error.message);
            std::process::exit(1);
        });

    code_emitter
        .emit_code(&assembly_ast)
        .unwrap_or_else(|code_emission_error| {
            eprintln!("Code emission error: {}", code_emission_error.message);
            std::process::exit(1);
        });
}

fn assemble(compiled_filepath: &PathBuf, output_filepath: &PathBuf) {
    let output = Command::new("gcc")
        .arg(compiled_filepath)
        .arg("-o")
        .arg(output_filepath)
        .output()
        .expect("Failed to execute assembler");

    if !output.status.success() {
        eprintln!(
            "Assembly failed! stderr: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        exit(1);
    }
}
