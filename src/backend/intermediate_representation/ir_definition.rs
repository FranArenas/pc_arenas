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
    BinaryIR {
        binary_operator: BinaryOperatorIR,
        left_operand: ValueIR,
        right_operand: ValueIR,
        dst: ValueIR,
    },
    ReturnIR {
        value: ValueIR,
    },
    CopyIR {
        src: ValueIR,
        dst: ValueIR,
    },
    JumpIR {
        label_identifier: String,
    },
    JumpIfZeroIR {
        condition: ValueIR,
        label_identifier: String,
    },
    JumpIfNotZeroIR {
        condition: ValueIR,
        label_identifier: String,
    },
    LabelIR {
        identifier: String,
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
            InstructionIR::BinaryIR {
                binary_operator,
                left_operand,
                right_operand,
                dst,
            } => {
                write!(
                    f,
                    "{} {} {} -> {}",
                    binary_operator, left_operand, right_operand, dst
                )
            }
            InstructionIR::ReturnIR { value } => {
                write!(f, "RETURN {}", value)
            }
            InstructionIR::CopyIR { src, dst } => {
                write!(f, "COPY {} -> {}", src, dst)
            }
            InstructionIR::JumpIR { label_identifier } => {
                write!(f, "JUMP {}", label_identifier)
            }
            InstructionIR::JumpIfZeroIR {
                condition,
                label_identifier,
            } => {
                write!(f, "JUMP_IF_ZERO {} {}", condition, label_identifier)
            }
            InstructionIR::JumpIfNotZeroIR {
                condition,
                label_identifier,
            } => {
                write!(f, "JUMP_IF_NOT_ZERO {} {}", condition, label_identifier)
            }
            InstructionIR::LabelIR { identifier } => {
                write!(f, "LABEL {}", identifier)
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
    LogicalNotIR,
}
impl fmt::Display for UnaryOperatorIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperatorIR::BitwiseComplementIR => write!(f, "COMPLEMENT"),
            UnaryOperatorIR::NegationIR => write!(f, "NEGATE"),
            UnaryOperatorIR::LogicalNotIR => write!(f, "NOT"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperatorIR {
    AdditionIR,
    SubtractionIR,
    MultiplicationIR,
    DivisionIR,
    ModulusIR,
    BitwiseAndIR,
    BitwiseOrIR,
    BitwiseXorIR,
    ShiftLeftIR,
    ShiftRightIR,
    EqualIR,
    NotEqualIR,
    LessThanIR,
    LessThanOrEqualIR,
    GreaterThanIR,
    GreaterThanOrEqualIR,
}
impl fmt::Display for BinaryOperatorIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperatorIR::AdditionIR => write!(f, "ADD"),
            BinaryOperatorIR::SubtractionIR => write!(f, "SUBTRACT"),
            BinaryOperatorIR::MultiplicationIR => write!(f, "MULTIPLY"),
            BinaryOperatorIR::DivisionIR => write!(f, "DIVIDE"),
            BinaryOperatorIR::ModulusIR => write!(f, "MODULUS"),
            BinaryOperatorIR::BitwiseAndIR => write!(f, "BITWISE_AND"),
            BinaryOperatorIR::BitwiseOrIR => write!(f, "BITWISE_OR"),
            BinaryOperatorIR::BitwiseXorIR => write!(f, "BITWISE_XOR"),
            BinaryOperatorIR::ShiftLeftIR => write!(f, "SHIFT_LEFT"),
            BinaryOperatorIR::ShiftRightIR => write!(f, "SHIFT_RIGHT"),
            BinaryOperatorIR::EqualIR => write!(f, "EQUAL"),
            BinaryOperatorIR::NotEqualIR => write!(f, "NOT_EQUAL"),
            BinaryOperatorIR::LessThanIR => write!(f, "LESS_THAN"),
            BinaryOperatorIR::LessThanOrEqualIR => write!(f, "LESS_THAN_OR_EQUAL"),
            BinaryOperatorIR::GreaterThanIR => write!(f, "GREATER_THAN"),
            BinaryOperatorIR::GreaterThanOrEqualIR => write!(f, "GREATER_THAN_OR_EQUAL"),
        }
    }
}
