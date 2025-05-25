use std::fmt;
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

impl FunctionDefinition {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            FunctionDefinition::Function { identifier, body } => {
                writeln!(f, "{}└── Function: {}", indent, identifier)?;
                body.fmt_with_indent(f, &format!("{}    ", indent))
            }
        }
    }
}

impl fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

// Statement
#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
}

impl Statement {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Statement::Return(expr) => {
                writeln!(f, "{}└── Return", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}

// Expression
#[derive(Debug, Clone)]
pub enum Expression {
    Constant(i64),
}

impl Expression {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Expression::Constant(value) => {
                writeln!(f, "{}└── Constant: {}", indent, value)
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "")
    }
}
