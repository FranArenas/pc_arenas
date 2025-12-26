use std::path::{Path, PathBuf};
use std::process::Command;

use pc_arenas::backend::intermediate_representation::ir_definition::ProgramIR;
use pc_arenas::backend::intermediate_representation::ir_generation::generate_ir;
use pc_arenas::{
    Cli, CodeEmitter, ProgramAssembly, ProgramAst, Token, generate_code, run_semantic_analysis,
};

/// Tracks which files have been created during compilation for cleanup purposes
#[derive(Default)]
struct CreatedFiles {
    preprocessed: bool,
    compiled: bool,
    executable: bool,
}

fn main() {
    // Get the input arguments
    let cli = Cli::parse_and_validate();

    // Get the needed file paths and variables
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

    let mut created_files = CreatedFiles::default();

    // Execute the compilation pipeline
    let result = compile(
        &cli,
        &preprocessed_filepath,
        &compiled_filepath,
        &executable_filepath,
        &mut created_files,
    );

    // Perform cleanup regardless of success or failure
    cleanup(
        &cli,
        &preprocessed_filepath,
        &compiled_filepath,
        &created_files,
    );

    // Handle compilation result
    if let Err(error) = result {
        eprintln!("{}", error);
        std::process::exit(1);
    }
}

fn compile(
    cli: &Cli,
    preprocessed_filepath: &PathBuf,
    compiled_filepath: &PathBuf,
    executable_filepath: &PathBuf,
    created_files: &mut CreatedFiles,
) -> Result<(), String> {
    // Execute the C preprocessor
    preprocess(&cli.input_file, preprocessed_filepath)
        .map_err(|e| format!("Preprocessing error: {}", e))?;
    created_files.preprocessed = true;

    // Step 1: Lexing
    let tokens = lex(preprocessed_filepath)?;

    if cli.stop_after_lexing {
        return Ok(());
    }

    // Step 2: Parsing
    let raw_ast = parse(tokens)?;

    if cli.print_raw_program_ast {
        println!("Program AST:\n");
        println!("{}", raw_ast.clone());
    }

    if cli.stop_after_parsing {
        return Ok(());
    }

    // step 2.5: Semantic Analysis
    let ast =
        run_semantic_analysis(raw_ast).map_err(|e| format!("Semantic analysis error: {}", e))?;

    if cli.print_program_ast {
        println!("Program AST:\n");
        println!("{}", ast);
    }

    if cli.stop_after_semantic_analysis {
        return Ok(());
    }

    // Step 3: IR Generation
    let ir = generate_ir(ast);

    if cli.print_ir {
        println!("IR Representation:\n");
        println!("{}", ir);
    }

    if cli.stop_after_ir {
        return Ok(());
    }

    // Step 4: assembly code generation
    let assembly = generate_assembly(ir)?;

    if cli.stop_after_codegen {
        return Ok(());
    }

    // Step 5: Emit code to file
    emit_code(assembly, compiled_filepath)?;
    created_files.compiled = true;

    // Step 6: Assemble and link if the user didn't request to stop earlier
    if !cli.stop_after_lexing && !cli.stop_after_parsing && !cli.stop_after_codegen {
        assemble(compiled_filepath, executable_filepath)
            .map_err(|e| format!("Assembly error: {}", e))?;
        created_files.executable = true;
    }

    Ok(())
}

/// Cleanup function that removes temporary files based on CLI flags and what was actually created
fn cleanup(
    cli: &Cli,
    preprocessed_filepath: &PathBuf,
    compiled_filepath: &PathBuf,
    created_files: &CreatedFiles,
) {
    // Clean up preprocessed file if it was created and user doesn't want to save it
    if created_files.preprocessed && !cli.save_preprocessed {
        if preprocessed_filepath.exists() {
            if let Err(e) = std::fs::remove_file(preprocessed_filepath) {
                eprintln!(
                    "Warning: Failed to delete preprocessed file '{}': {}",
                    preprocessed_filepath.display(),
                    e
                );
            }
        }
    }

    // Clean up compiled file if it was created and user doesn't want to save it
    if created_files.compiled && !cli.save_compiled {
        if compiled_filepath.exists() {
            if let Err(e) = std::fs::remove_file(compiled_filepath) {
                eprintln!(
                    "Warning: Failed to delete assembly file '{}': {}",
                    compiled_filepath.display(),
                    e
                );
            }
        }
    }
}

fn preprocess<P: AsRef<Path>>(input_file: P, output_file: P) -> Result<(), String> {
    let output = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(input_file.as_ref())
        .arg("-o")
        .arg(output_file.as_ref())
        .output()
        .map_err(|e| format!("Failed to execute preprocessor: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Preprocessing failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(())
}

fn lex<P: AsRef<Path>>(
    preprocessed_file: P,
) -> Result<Vec<pc_arenas::frontend::lexer::Token>, String> {
    let content = std::fs::read_to_string(preprocessed_file)
        .map_err(|e| format!("Failed to read preprocessed file: {}", e))?;

    let (tokens, lex_errors) = pc_arenas::frontend::lexer::tokenize(&content);

    if !lex_errors.is_empty() {
        let mut error_messages = Vec::new();
        for error in &lex_errors {
            error_messages.push(error.to_string());
        }
        return Err(error_messages.join("\n"));
    }

    Ok(tokens)
}

fn parse(tokens: Vec<Token>) -> Result<ProgramAst, String> {
    let ast = pc_arenas::frontend::parser::parse(tokens).map_err(|parse_error| {
        let mut error_messages = Vec::new();
        for error in &parse_error {
            error_messages.push(error.to_string());
        }
        error_messages.join("\n")
    })?;

    Ok(ast)
}

fn generate_assembly(ir: ProgramIR) -> Result<ProgramAssembly, String> {
    let assembly_ast = generate_code(ir);

    Ok(assembly_ast)
}

fn emit_code<P: AsRef<Path>>(
    assembly_ast: ProgramAssembly,
    output_filename: P,
) -> Result<(), String> {
    let mut code_emitter = CodeEmitter::from_path(output_filename)
        .map_err(|code_emission_error| format!("Code emission error: {}", code_emission_error))?;

    code_emitter
        .emit_code(&assembly_ast)
        .map_err(|code_emission_error| format!("Code emission error: {}", code_emission_error))?;

    Ok(())
}

fn assemble(compiled_filepath: &PathBuf, output_filepath: &PathBuf) -> Result<(), String> {
    let output = Command::new("gcc")
        .arg(compiled_filepath)
        .arg("-o")
        .arg(output_filepath)
        .output()
        .map_err(|e| format!("Failed to execute assembler: {}", e))?;

    if !output.status.success() {
        return Err(format!(
            "Assembly failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(())
}
