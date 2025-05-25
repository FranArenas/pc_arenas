// Assembly AST

use std::fmt;

// ProgramAssembly
#[derive(Debug, Clone)]
pub enum ProgramAssembly {
    ProgramAssembly(FunctionDefinitionAssembly),
}
impl fmt::Display for ProgramAssembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProgramAssembly::ProgramAssembly(func_def) => {
                writeln!(f, "Program")?;
                write!(f, "{}", func_def)
            }
        }
    }
}

// FunctionDefinitionAssembly
#[derive(Debug, Clone)]
pub enum FunctionDefinitionAssembly {
    Function {
        identifier: String,
        instructions: Vec<Instruction>,
    },
}
impl fmt::Display for FunctionDefinitionAssembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionDefinitionAssembly::Function {
                identifier,
                instructions,
            } => {
                writeln!(f, "└── Function: {}", identifier)?;
                for instr in instructions {
                    writeln!(f, "    └── {}", instr)?;
                }
                Ok(())
            }
        }
    }
}

// Instruction
#[derive(Debug, Clone)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov { src, dst } => write!(f, "MOV {} , {}", src, dst),
            Instruction::Ret => write!(f, "RET"),
        }
    }
}

// Operand
#[derive(Debug, Clone)]
pub enum Operand {
    Immediate(i64),
    Register,
}
impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Immediate(val) => write!(f, "${}", val),
            Operand::Register => write!(f, "%eax"),
        }
    }
}
