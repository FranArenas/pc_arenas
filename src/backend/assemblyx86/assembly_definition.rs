// This module defines the x86 data structures and their translation to assembly code as strings.
// This is done in this module instead in a separate one like code emission because the data structures
// are already specific to x86 assembly. Keeping it here improves locality and readability as separating
// it would not change the coupling since the data structures are already logically coupled to x86.

use std::fmt::{Display, Formatter, Result as FmtResult};

// ===============================================
// Assembly Writer
// ===============================================

// A private helper struct to facilitate writing assembly code with proper indentation and buffering.
struct AsmWriter {
    pub buf: String,
    pub indent: usize,
}

impl AsmWriter {
    /// Creates a new `AsmWriter` with an empty buffer and zero indentation level.
    pub fn new() -> Self {
        Self {
            buf: String::new(),
            indent: 0,
        }
    }

    /// Executes the provided closure with increased indentation level.
    ///
    /// The indentation level is incremented before calling the closure and
    /// decremented after the closure completes, ensuring proper nesting of code blocks.
    pub fn indent_closure<F: FnOnce(&mut AsmWriter)>(&mut self, f: F) {
        self.indent += 1;
        f(self); // Pass the same writer but with increased indentation. It will be decreased after the call
        self.indent -= 1;
    }

    /// Appends a line of text to the buffer with the current indentation level.
    ///
    /// Each indentation level adds 4 spaces. A newline is automatically added at the end.
    pub fn line<S: AsRef<str>>(&mut self, s: S) {
        self.buf.push_str(&"    ".repeat(self.indent));
        self.buf.push_str(s.as_ref());
        self.buf.push('\n');
    }

    /// Consumes the writer and returns the accumulated assembly code as a `String`.
    pub fn output(self) -> String {
        self.buf
    }
}

// ===============================================
// Trait for AST → Assembly string
// ===============================================

// Private trait implemented by all assembly AST nodes to generate assembly code.
trait GenerateAssembly {
    fn write(&self, writer: &mut AsmWriter);
}

// ===============================================
// ProgramAssembly (keeps the single-variant enum)
// ===============================================

#[derive(Debug, Clone)]
pub enum ProgramAssembly {
    ProgramAssembly(FunctionDefinitionAssembly),
}

impl ProgramAssembly {
    pub fn generate_assembly_code(&self) -> String {
        let mut writer = AsmWriter::new();

        match self {
            ProgramAssembly::ProgramAssembly(func) => func.write(&mut writer),
        }

        writer.output()
    }
}

// ===============================================
// FunctionDefinitionAssembly (keeps single variant)
// ===============================================

#[derive(Debug, Clone)]
pub enum FunctionDefinitionAssembly {
    Function {
        identifier: String,
        instructions: Vec<InstructionAssembly>,
    },
}

impl GenerateAssembly for FunctionDefinitionAssembly {
    fn write(&self, writer: &mut AsmWriter) {
        match self {
            FunctionDefinitionAssembly::Function {
                identifier,
                instructions,
            } => {
                writer.line(format!(".globl {}", identifier));
                writer.line(format!("{}:", identifier));
                // Function "body" with indentation
                writer.indent_closure(|indented_writer| {
                    indented_writer.line("pushq %rbp");
                    indented_writer.line("movq %rsp, %rbp");
                    for instr in instructions {
                        instr.write(indented_writer);
                    }
                });
            }
        }
    }
}

// ===============================================
// Instructions
// ===============================================
#[derive(Debug, Clone)]
pub enum InstructionSize {
    Byte,       // 8 bits.
    Word,       // 16 bits.
    DoubleWord, // 32 bits.
    QuadWord,   // 64 bits.
}
impl Display for InstructionSize {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let s = match self {
            InstructionSize::Byte => "b",
            InstructionSize::Word => "w",
            InstructionSize::DoubleWord => "l",
            InstructionSize::QuadWord => "q",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
pub enum InstructionAssembly {
    Mov {
        src: OperandAssembly,
        dst: OperandAssembly,
        size: InstructionSize,
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
    Cmp {
        left_operand: OperandAssembly,
        right_operand: OperandAssembly,
    },
    Jmp {
        label: String,
    },
    JmpCC {
        condition: ConditionCodeAssembly,
        label: String,
    },
    SetCC {
        condition: ConditionCodeAssembly,
        dst: OperandAssembly,
    },
    Label {
        identifier: String,
    },
    Cdq,
    AllocateStack {
        size: i32,
    },
    Ret,
}

impl GenerateAssembly for InstructionAssembly {
    fn write(&self, writer: &mut AsmWriter) {
        match self {
            InstructionAssembly::Mov { src, dst, size } => {
                writer.line(format!("mov{} {}, {}", size, src, dst))
            }

            InstructionAssembly::Unary {
                unary_operator,
                operand,
            } => writer.line(format!("{} {}", unary_operator, operand)),

            InstructionAssembly::Binary {
                binary_operator,
                left_operand,
                right_operand,
            } => writer.line(format!(
                "{} {}, {}",
                binary_operator, left_operand, right_operand
            )),

            InstructionAssembly::AllocateStack { size } => {
                writer.line(format!("subq ${}, %rsp", size))
            }

            InstructionAssembly::Ret => {
                writer.line("movq %rbp, %rsp");
                writer.line("popq %rbp");
                writer.line("ret");
            }

            InstructionAssembly::Idiv { divisor } => writer.line(format!("idivl {}", divisor)),

            InstructionAssembly::Cdq => writer.line("cdq"),
            InstructionAssembly::Cmp {
                left_operand,
                right_operand,
            } => writer.line(format!("cmpl {}, {}", left_operand, right_operand)),
            InstructionAssembly::Jmp { label } => writer.line(format!("jmp .L{}", label)), // .L prefix is used to mark labels as local to avoid conflicts with user defined functions
            InstructionAssembly::JmpCC { condition, label } => {
                writer.line(format!("j{} .L{}", condition, label))
            }
            InstructionAssembly::SetCC { condition, dst } => {
                writer.line(format!("set{} {}", condition, dst))
            }
            InstructionAssembly::Label { identifier } => {
                // Labels are not indented
                writer.indent -= 1;
                writer.line(format!(".L{}:", identifier)); // .L prefix is used to mark labels as local to avoid conflicts with user defined functions
                writer.indent += 1;
            }
        }
    }
}

// ===============================================
// Operands
// ===============================================

#[derive(Debug, Clone)]
pub enum OperandAssembly {
    Immediate(i64),
    Register(RegisterAssembly),
    PseudoRegister(String),
    StackVariable(i32),
}

impl Display for OperandAssembly {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            OperandAssembly::Immediate(v) => write!(f, "${}", v),
            OperandAssembly::Register(r) => write!(f, "%{}", r),
            OperandAssembly::PseudoRegister(name) => write!(f, "{}", name),
            OperandAssembly::StackVariable(offset) => write!(f, "-{}(%rbp)", offset),
        }
    }
}

// ===============================================
// Registers
// ===============================================

#[derive(Debug, Clone)]
pub enum RegisterAssembly {
    // AX
    Eax,
    Al,
    // DX
    Edx,
    Dl,
    // CL
    Cl,
    // R10
    R10d,
    R10b,
    // R11
    R11d,
    R11b,
}

impl Display for RegisterAssembly {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let s = match self {
            RegisterAssembly::Eax => "eax",
            RegisterAssembly::Al => "al",
            RegisterAssembly::Edx => "edx",
            RegisterAssembly::Dl => "dl",
            RegisterAssembly::Cl => "cl", // Used for shift operations
            RegisterAssembly::R10d => "r10d",
            RegisterAssembly::R10b => "r10b",
            RegisterAssembly::R11d => "r11d",
            RegisterAssembly::R11b => "r11b",
        };
        write!(f, "{}", s)
    }
}

// ===============================================
// Unary Operators
// ===============================================

#[derive(Debug, Clone)]
pub enum UnaryOperatorAssembly {
    NegationAssembly,
    BitwiseNotAssembly,
}

impl Display for UnaryOperatorAssembly {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let s = match self {
            UnaryOperatorAssembly::NegationAssembly => "negl",
            UnaryOperatorAssembly::BitwiseNotAssembly => "notl",
        };
        write!(f, "{}", s)
    }
}

// ===============================================
// Binary Operators
// ===============================================

#[derive(Debug, Clone)]
pub enum BinaryOperatorAssembly {
    AdditionAssembly,
    SubtractionAssembly,
    MultiplicationAssembly,
    BitwiseAndAssembly,
    BitwiseOrAssembly,
    BitwiseXorAssembly,
    ShiftLeftAssembly,
    ShiftRightAssembly,
}

impl Display for BinaryOperatorAssembly {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let s = match self {
            BinaryOperatorAssembly::AdditionAssembly => "addl",
            BinaryOperatorAssembly::SubtractionAssembly => "subl",
            BinaryOperatorAssembly::MultiplicationAssembly => "imull",
            BinaryOperatorAssembly::BitwiseAndAssembly => "andl",
            BinaryOperatorAssembly::BitwiseOrAssembly => "orl",
            BinaryOperatorAssembly::BitwiseXorAssembly => "xorl",
            BinaryOperatorAssembly::ShiftLeftAssembly => "sall",
            BinaryOperatorAssembly::ShiftRightAssembly => "sarl",
        };
        write!(f, "{}", s)
    }
}

// ===============================================
// Others
// ===============================================
#[derive(Debug, Clone)]
pub enum ConditionCodeAssembly {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
impl Display for ConditionCodeAssembly {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let s = match self {
            ConditionCodeAssembly::Equal => "e",
            ConditionCodeAssembly::NotEqual => "ne",
            ConditionCodeAssembly::LessThan => "l",
            ConditionCodeAssembly::LessThanOrEqual => "le",
            ConditionCodeAssembly::GreaterThan => "g",
            ConditionCodeAssembly::GreaterThanOrEqual => "ge",
        };
        write!(f, "{}", s)
    }
}
