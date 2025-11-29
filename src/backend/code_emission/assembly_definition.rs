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
        instructions: Vec<InstructionAssembly>, // todo: Consider using a linked list or VecDeque  for instructions instead of a vector
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
pub enum InstructionAssembly {
    Mov {
        src: OperandAssembly,
        dst: OperandAssembly,
    },
    Unary {
        unary_operator: UnaryOperatorAssembly,
        operand: OperandAssembly,
    },
    Binary {
        binary_operator: BinaryOperatorAssembly,
        left_operand: OperandAssembly,
        right_operand: OperandAssembly,
    },
    Idiv {
        divisor: OperandAssembly,
    },
    Cdq, // sign-extend EAX into EDX:EAX (it is used before IDIV)
    AllocateStack {
        size: i32,
    },
    Ret,
}

impl fmt::Display for InstructionAssembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstructionAssembly::Mov { src, dst } => write!(f, "movl {} , {}", src, dst),
            InstructionAssembly::Unary {
                unary_operator,
                operand,
            } => {
                write!(f, "{} {}", unary_operator, operand)
            }
            InstructionAssembly::Binary {
                binary_operator,
                left_operand,
                right_operand,
            } => {
                write!(f, "{} {}, {}", binary_operator, left_operand, right_operand)
            }
            InstructionAssembly::AllocateStack { size } => {
                write!(f, "subq ${}, %rsp", size)
            }
            InstructionAssembly::Ret => write!(
                f,
                r#"movq %rbp, %rsp
popq %rbp 
ret
                    "#
            ),
            InstructionAssembly::Idiv { divisor } => {
                write!(f, "idivl {}", divisor)
            }
            InstructionAssembly::Cdq => write!(f, "Cdq"),
        }
    }
}

// Operand
#[derive(Debug, Clone)]
pub enum OperandAssembly {
    Immediate(i64),
    Register(RegisterAssembly),
    PseudoRegister(String),
    StackVariable(i32), // indicates the stack address at the given offset from RBP (Relative Base Pointer)
}
impl fmt::Display for OperandAssembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OperandAssembly::Immediate(val) => write!(f, "${}", val),
            OperandAssembly::Register(name) => write!(f, "%{}", name),
            OperandAssembly::PseudoRegister(name) => write!(f, "{}", name),
            OperandAssembly::StackVariable(offset) => write!(f, "-{}(%rbp)", offset), // Addressing from RBP downwards, for that reason it is negative
        }
    }
}

// Register
#[derive(Debug, Clone)]
pub enum RegisterAssembly {
    Ax,
    Dx,
    R10,
    R11,
}
impl fmt::Display for RegisterAssembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegisterAssembly::Ax => write!(f, "eax"), // X registers, are size agnostic
            RegisterAssembly::Dx => write!(f, "edx"),
            RegisterAssembly::R10 => write!(f, "r10d"),
            RegisterAssembly::R11 => write!(f, "r11d"),
        }
    }
}

// UnaryOperator
#[derive(Debug, Clone)]
pub enum UnaryOperatorAssembly {
    NegationAssembly,   // corresponds to NEG instruction
    BitwiseNotAssembly, // corresponds to NOT instruction
}
impl fmt::Display for UnaryOperatorAssembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperatorAssembly::NegationAssembly => write!(f, "negl"),
            UnaryOperatorAssembly::BitwiseNotAssembly => write!(f, "notl"),
        }
    }
}

// BinaryOperator
#[derive(Debug, Clone)]
pub enum BinaryOperatorAssembly {
    AdditionAssembly,       // corresponds to ADD instruction
    SubtractionAssembly,    // corresponds to SUB instruction
    MultiplicationAssembly, // corresponds to IMUL instruction
}
impl fmt::Display for BinaryOperatorAssembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperatorAssembly::AdditionAssembly => write!(f, "addl"),
            BinaryOperatorAssembly::SubtractionAssembly => write!(f, "subl"),
            BinaryOperatorAssembly::MultiplicationAssembly => write!(f, "imull"),
        }
    }
}
