use std::fmt;

#[derive(Debug, Clone)]
pub enum ProgramIR {
    ProgramIR(FunctionDefinitionIR),
}

impl fmt::Display for ProgramIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProgramIR::ProgramIR(func_def) => {
                writeln!(f, "Program")?;
                write!(f, "{}", func_def)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionDefinitionIR {
    Function {
        identifier: String,
        body: Vec<InstructionIR>,
    },
}
impl fmt::Display for FunctionDefinitionIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionDefinitionIR::Function { identifier, body } => {
                writeln!(f, "└── Function: {}", identifier)?;
                for instr in body {
                    writeln!(f, "    └── {}", instr)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum InstructionIR {
    UnaryIR {
        unary_operator: UnaryOperatorIR,
        src: ValueIR,
        dst: ValueIR,
    },
    ReturnIR {
        value: ValueIR,
    },
}
impl fmt::Display for InstructionIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstructionIR::UnaryIR {
                unary_operator,
                src,
                dst,
            } => {
                write!(f, "{} {} -> {}", unary_operator, src, dst)
            }
            InstructionIR::ReturnIR { value } => {
                write!(f, "RETURN {}", value)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueIR {
    ConstantIR(i64),
    VariableIR(String),
}
impl fmt::Display for ValueIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueIR::ConstantIR(val) => write!(f, "{}", val),
            ValueIR::VariableIR(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorIR {
    BitwiseComplementIR,
    NegationIR,
}
impl fmt::Display for UnaryOperatorIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperatorIR::BitwiseComplementIR => write!(f, "COMPLEMENT"),
            UnaryOperatorIR::NegationIR => write!(f, "NEGATE"),
        }
    }
}
