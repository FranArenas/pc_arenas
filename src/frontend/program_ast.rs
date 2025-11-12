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
    Constant(i64),
    UnaryOp(UnaryOperator, Box<Expression>),
}

impl IndentedDisplay for Expression {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Expression::Constant(value) => {
                writeln!(f, "{}└── Constant: {}", indent, value)
            }
            Expression::UnaryOp(operator, expression) => {
                writeln!(f, "{}└── UnaryOp: {}", indent, operator)?;
                expression.fmt_with_indent(f, &format!("{}    ", indent))
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

// Generate Display implementations for all types that implement IndentedDisplay
impl_display!(FunctionDefinition, Statement, Expression);
