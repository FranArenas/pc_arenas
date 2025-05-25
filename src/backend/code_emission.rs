use std::fs::File;
use std::io::Write;

use crate::backend::assembly_ast::{FunctionDefinitionAssembly, ProgramAssembly};

#[derive(Debug, Clone)]
pub struct CodeEmissionError {
    pub message: String,
}

// Todo: Consider moving the generated code to a trait (name: GenerateAssembly) to all the nodes of the assembly AST
pub struct CodeEmitter {
    file: File,
}

impl CodeEmitter {
    pub fn new(file: File) -> Self {
        Self { file }
    }

    pub fn default() -> Self {
        Self {
            file: File::create("pcarenas.s").expect("Failed to create default output file"),
        }
    }

    pub fn from_path<P: AsRef<std::path::Path>>(path: P) -> Result<Self, CodeEmissionError> {
        let file = File::create(path).map_err(|e| CodeEmissionError {
            message: format!("Failed to create assembly file: {}", e),
        })?;
        Ok(Self { file })
    }
}

impl CodeEmitter {
    pub fn emit_code(&mut self, assembly: &ProgramAssembly) -> Result<(), CodeEmissionError> {
        match assembly {
            ProgramAssembly::ProgramAssembly(func_def) => {
                self.handle_function_definition(func_def)?
            }
        };

        writeln!(self.file, ".section .note.GNU-stack,\"\",@progbits").map_err(|e| {
            CodeEmissionError {
                message: format!(
                    "Failed to write Linux specific assembly instruction to output file: {}",
                    e
                ),
            }
        })?;
        Ok(())
    }

    fn handle_function_definition(
        &mut self,
        func_def: &FunctionDefinitionAssembly,
    ) -> Result<(), CodeEmissionError> {
        match func_def {
            FunctionDefinitionAssembly::Function {
                identifier,
                instructions,
            } => {
                let code = format!(
                    r#".globl {name}
{name}:
                    "#,
                    name = identifier,
                );
                writeln!(self.file, "{}", code).map_err(|e| CodeEmissionError {
                    message: format!("Failed to write function definition to output file: {}", e),
                })?;
                for instr in instructions {
                    writeln!(self.file, "{}", instr).map_err(|e| CodeEmissionError {
                        message: format!("Failed to write instruction to output file: {}", e),
                    })?;
                }
            }
        }

        Ok(())
    }
}
