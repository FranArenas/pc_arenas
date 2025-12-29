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
    Function {
        identifier: String,
        body: Vec<BlockItem>,
    },
}

impl IndentedDisplay for FunctionDefinition {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            FunctionDefinition::Function { identifier, body } => {
                writeln!(f, "{}└── Function: {}", indent, identifier)?;
                writeln!(f, "{}    └── Body", indent)?;
                for item in body {
                    item.fmt_with_indent(f, &format!("{}        ", indent))?;
                }
                Ok(())
            }
        }
    }
}

// Block Item. Used in function definitions to define a list of statements and declarations
#[derive(Debug, Clone)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}
impl IndentedDisplay for BlockItem {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            BlockItem::Declaration(decl) => {
                writeln!(f, "{}└── Declaration", indent)?;
                decl.fmt_with_indent(f, &format!("{}    ", indent))
            }
            BlockItem::Statement(stmt) => {
                writeln!(f, "{}└── Statement", indent)?;
                stmt.fmt_with_indent(f, &format!("{}    ", indent))
            }
        }
    }
}

// Declaration. Used to declare variables
#[derive(Debug, Clone)]
pub enum Declaration {
    VariableDeclaration {
        identifier: String,
        initial_value: Option<Expression>,
    },
}

impl IndentedDisplay for Declaration {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Declaration::VariableDeclaration {
                identifier,
                initial_value,
            } => {
                writeln!(f, "{}└── VariableDeclaration: {}", indent, identifier)?;
                if let Some(value) = initial_value {
                    writeln!(f, "{}    └── InitialValue", indent)?;
                    value.fmt_with_indent(f, &format!("{}        ", indent))
                } else {
                    Ok(())
                }
            }
        }
    }
}

// Statement
#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Expression(Expression), // Used for expression statements, used usually for side effects
    Null,
}

impl IndentedDisplay for Statement {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Statement::Return(expr) => {
                writeln!(f, "{}└── Return", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Statement::Expression(expr) => {
                writeln!(f, "{}└── Expression", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                writeln!(f, "{}└── If", indent)?;
                writeln!(f, "{}    └── Condition", indent)?;
                condition.fmt_with_indent(f, &format!("{}        ", indent))?;
                writeln!(f, "{}    └── ThenBranch", indent)?;
                then_branch.fmt_with_indent(f, &format!("{}        ", indent))?;
                if let Some(else_branch) = else_branch {
                    writeln!(f, "{}    └── ElseBranch", indent)?;
                    else_branch.fmt_with_indent(f, &format!("{}        ", indent))?;
                }
                Ok(())
            }
            Statement::Null => {
                writeln!(f, "{}└── Null", indent)
            }
        }
    }
}

// Expression
#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
    BinaryOperator {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Assignment {
        lvalue: Box<Expression>,
        value: Box<Expression>,
    },
    CompoundAssignment {
        operator: CompoundAssignmentOperator,
        lvalue: Box<Expression>,
        value: Box<Expression>,
    },
    PrefixIncrement(Box<Expression>),
    PrefixDecrement(Box<Expression>),
    PostfixIncrement(Box<Expression>),
    PostfixDecrement(Box<Expression>),
    Constant(i64),
    UnaryOp(UnaryOperator, Box<Expression>),
    Conditional {
        condition: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
}

impl IndentedDisplay for Expression {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str) -> fmt::Result {
        match self {
            Expression::BinaryOperator {
                operator,
                left,
                right,
            } => {
                writeln!(f, "{}└── BinaryOperator: {}", indent, operator)?;
                left.fmt_with_indent(f, &format!("{}    ", indent))?;
                right.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::Assignment { lvalue, value } => {
                writeln!(f, "{}└── Assignment", indent)?;
                lvalue.fmt_with_indent(f, &format!("{}    ", indent))?;
                value.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::CompoundAssignment {
                operator,
                lvalue,
                value,
            } => {
                writeln!(f, "{}└── CompoundAssignment: {}", indent, operator)?;
                lvalue.fmt_with_indent(f, &format!("{}    ", indent))?;
                value.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::PrefixIncrement(expr) => {
                writeln!(f, "{}└── PrefixIncrement (++)", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::PrefixDecrement(expr) => {
                writeln!(f, "{}└── PrefixDecrement (--)", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::PostfixIncrement(expr) => {
                writeln!(f, "{}└── PostfixIncrement (++)", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::PostfixDecrement(expr) => {
                writeln!(f, "{}└── PostfixDecrement (--)", indent)?;
                expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::Variable(identifier) => {
                writeln!(f, "{}└── Variable: {}", indent, identifier)
            }
            Expression::Constant(value) => {
                writeln!(f, "{}└── IntLiteral: {}", indent, value)
            }
            Expression::UnaryOp(operator, inner_expr) => {
                writeln!(f, "{}└── UnaryOp: {}", indent, operator)?;
                inner_expr.fmt_with_indent(f, &format!("{}    ", indent))
            }
            Expression::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                writeln!(f, "{}└── Conditional", indent)?;
                writeln!(f, "{}    └── Condition", indent)?;
                condition.fmt_with_indent(f, &format!("{}        ", indent))?;
                writeln!(f, "{}    └── ThenExpression", indent)?;
                then_expr.fmt_with_indent(f, &format!("{}        ", indent))?;
                writeln!(f, "{}    └── ElseExpression", indent)?;
                else_expr.fmt_with_indent(f, &format!("{}        ", indent))
            }
        }
    }
}

// UnaryOperator
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    BitwiseComplement,
    Negation,
    LogicalNot,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::BitwiseComplement => write!(f, "BitwiseComplement (~)"),
            UnaryOperator::Negation => write!(f, "Negation (-)"),
            UnaryOperator::LogicalNot => write!(f, "LogicalNot (!)"),
        }
    }
}

// CompoundAssignmentOperator
#[derive(Debug, Clone)]
pub enum CompoundAssignmentOperator {
    AddAssignment,        // +=
    SubtractAssignment,   // -=
    MultiplyAssignment,   // *=
    DivideAssignment,     // /=
    ModulusAssignment,    // %=
    BitwiseAndAssignment, // &=
    BitwiseOrAssignment,  // |=
    BitwiseXorAssignment, // ^=
    ShiftLeftAssignment,  // <<=
    ShiftRightAssignment, // >>=
}

impl fmt::Display for CompoundAssignmentOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompoundAssignmentOperator::AddAssignment => write!(f, "AddAssignment (+=)"),
            CompoundAssignmentOperator::SubtractAssignment => write!(f, "SubtractAssignment (-=)"),
            CompoundAssignmentOperator::MultiplyAssignment => write!(f, "MultiplyAssignment (*=)"),
            CompoundAssignmentOperator::DivideAssignment => write!(f, "DivideAssignment (/=)"),
            CompoundAssignmentOperator::ModulusAssignment => write!(f, "ModulusAssignment (%=)"),
            CompoundAssignmentOperator::BitwiseAndAssignment => {
                write!(f, "BitwiseAndAssignment (&=)")
            }
            CompoundAssignmentOperator::BitwiseOrAssignment => {
                write!(f, "BitwiseOrAssignment (|=)")
            }
            CompoundAssignmentOperator::BitwiseXorAssignment => {
                write!(f, "BitwiseXorAssignment (^=)")
            }
            CompoundAssignmentOperator::ShiftLeftAssignment => {
                write!(f, "ShiftLeftAssignment (<<=)")
            }
            CompoundAssignmentOperator::ShiftRightAssignment => {
                write!(f, "ShiftRightAssignment (>>=)")
            }
        }
    }
}

// BinaryOperator
#[derive(Debug, Clone)]
pub enum BinaryOperator {
    // Arithmetic operators
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    // Bitwise operators
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    // Shift operators
    ShiftLeft,
    ShiftRight,
    // Logical operators
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
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
            BinaryOperator::LogicalAnd => write!(f, "LogicalAnd (&&)"),
            BinaryOperator::LogicalOr => write!(f, "LogicalOr (||)"),
            BinaryOperator::Equal => write!(f, "Equal (==)"),
            BinaryOperator::NotEqual => write!(f, "NotEqual (!=)"),
            BinaryOperator::LessThan => write!(f, "LessThan (<)"),
            BinaryOperator::GreaterThan => write!(f, "GreaterThan (>)"),
            BinaryOperator::LessThanOrEqual => write!(f, "LessThanOrEqual (<=)"),
            BinaryOperator::GreaterThanOrEqual => write!(f, "GreaterThanOrEqual (>=)"),
        }
    }
}

// Generate Display implementations for all types that implement IndentedDisplay
impl_display!(
    FunctionDefinition,
    Statement,
    Expression,
    Declaration,
    BlockItem
);
