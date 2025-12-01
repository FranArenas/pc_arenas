use std::fmt;

// Trait for AST nodes that can be formatted with indentation
pub trait IndentedDisplay {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result;
}

// Macro to automatically generate Display implementation for types that implement IndentedDisplay
macro_rules! impl_display {
    ($($type:ty),*) => {
        $(
            impl fmt::Display for $type {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    self.fmt_with_indent(f, "")
                }
            }
        )*
    };
}

// Program
#[derive(Debug, Clone)]
pub enum ProgramAst {
    Program(FunctionDefinition),
}

impl fmt::Display for ProgramAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProgramAst::Program(func_def) => {
                writeln!(f, "Program")?;
                func_def.fmt_with_indent(f, "  ")
            }
        }
    }
}

// FunctionDefinition
#[derive(Debug, Clone)]
pub enum FunctionDefinition {
    Function { identifier: String, body: Statement },
}

impl IndentedDisplay for FunctionDefinition {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            FunctionDefinition::Function { identifier, body } => {
                writeln!(f, "{}└── Function: {}", indent, identifier)?;
                body.fmt_with_indent(f, &format!("{}    ", indent))
            }
        }
    }
}

// Statement
#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
}

impl IndentedDisplay for Statement {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Statement::Return(expr) => {
                writeln!(f, "{}└── Return", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
        }
    }
}

// Expression
#[derive(Debug, Clone)]
pub enum Expression {
    Factor(Factor),
    BinaryOperator {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

impl IndentedDisplay for Expression {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Expression::Factor(factor) => {
                writeln!(f, "{}└── Factor", indent)?;
                factor.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::BinaryOperator {
                operator,
                left,
                right,
            } => {
                writeln!(f, "{}└── BinaryOperator: {}", indent, operator)?; // todo: Check if this looks good
                left.fmt_with_indent(f, &format!("{}    ", indent))?;
                right.fmt_with_indent(f, &format!("{}    ", indent))
            }
        }
    }
}

// Factor
#[derive(Debug, Clone)]
pub enum Factor {
    IntLiteral(i64),
    UnaryOp(UnaryOperator, Box<Factor>),
    Expression(Box<Expression>), // Boxed to avoid recursive size issues due to recursion with Expression
}

impl IndentedDisplay for Factor {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Factor::IntLiteral(value) => {
                writeln!(f, "{}└── IntLiteral: {}", indent, value)
            }
            Factor::UnaryOp(operator, factor) => {
                writeln!(f, "{}└── UnaryOp: {}", indent, operator)?;
                factor.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Factor::Expression(expr) => {
                writeln!(f, "{}└── Expression", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
        }
    }
}

// UnaryOperator
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    BitwiseComplement,
    Negation,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::BitwiseComplement => write!(f, "BitwiseComplement (~)"),
            UnaryOperator::Negation => write!(f, "Negation (-)"),
        }
    }
}

// BinaryOperator
#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Addition => write!(f, "Addition (+)"),
            BinaryOperator::Subtraction => write!(f, "Subtraction (-)"),
            BinaryOperator::Multiplication => write!(f, "Multiplication (*)"),
            BinaryOperator::Division => write!(f, "Division (/)"),
            BinaryOperator::Modulus => write!(f, "Modulus (%)"),
            BinaryOperator::BitwiseAnd => write!(f, "BitwiseAnd (&)"),
            BinaryOperator::BitwiseOr => write!(f, "BitwiseOr (|)"),
            BinaryOperator::BitwiseXor => write!(f, "BitwiseXor (^)"),
            BinaryOperator::ShiftLeft => write!(f, "ShiftLeft (<<)"),
            BinaryOperator::ShiftRight => write!(f, "ShiftRight (>>)"),
        }
    }
}

// Generate Display implementations for all types that implement IndentedDisplay
impl_display!(FunctionDefinition, Statement, Expression, Factor);
