use std::io::Write;
use std::{fmt, fs::File};

use crate::backend::assembly_definition::ProgramAssembly;

#[derive(Debug, Clone)]
pub struct CodeEmissionError {
    pub message: String,
}

impl fmt::Display for CodeEmissionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.message)
    }
}

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

    pub fn emit_code(&mut self, assembly: &ProgramAssembly) -> Result<(), CodeEmissionError> {
        let code = assembly.generate_assembly_code();

        write!(self.file, "{}", code).map_err(|e| CodeEmissionError {
            message: format!("Failed to write assembly code to output file: {}", e),
        })?;

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
}
