use crate::frontend::program_ast::{BlockItem, Declaration, ProgramAst};
use crate::frontend::semantic::symbol_table::SymbolTable;
use crate::{Expression, FunctionDefinition, Statement};

//todo: Implement panic mode for error handling?

pub fn run_semantic_analysis(program_ast: ProgramAst) -> Result<ProgramAst, String> {
    let mut symbol_table = SymbolTable::new();

    // Step 1: Variable resolution
    Ok(resolve_variables(program_ast, &mut symbol_table)?)
}

// #region Variable Resolution

fn resolve_variables(
    ast: ProgramAst,
    symbol_table: &mut SymbolTable,
) -> Result<ProgramAst, String> {
    match ast {
        ProgramAst::Program(FunctionDefinition::Function { identifier, body }) => {
            let updated_body = body
                .into_iter()
                .map(|block| resolve_block_item(block, symbol_table))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(ProgramAst::Program(FunctionDefinition::Function {
                identifier,
                body: updated_body,
            }))
        }
    }
}

fn resolve_block_item(
    item: BlockItem,
    symbol_table: &mut SymbolTable,
) -> Result<BlockItem, String> {
    match item {
        BlockItem::Declaration(decl) => Ok(BlockItem::Declaration(resolve_declaration(
            decl,
            symbol_table,
        )?)),
        BlockItem::Statement(stmt) => {
            Ok(BlockItem::Statement(resolve_statement(stmt, symbol_table)?))
        }
    }
}

fn resolve_declaration(
    decl: Declaration,
    symbol_table: &mut SymbolTable,
) -> Result<Declaration, String> {
    match decl {
        Declaration::VariableDeclaration {
            identifier,
            initial_value,
        } => {
            if symbol_table.contains(&identifier) {
                return Err(format!(
                    "Variable '{}' already declared in this scope",
                    identifier
                ));
            }

            symbol_table.insert(&identifier);

            let init_expression = initial_value
                .map(|expr| resolve_expression(expr, symbol_table))
                .transpose()?;

            let unique_id = symbol_table
                .get(&identifier)
                .ok_or_else(|| format!("Failed to retrieve symbol info for '{}'", identifier))?
                .id
                .clone();

            Ok(Declaration::VariableDeclaration {
                identifier: unique_id,
                initial_value: init_expression,
            })
        }
    }
}

fn resolve_statement(stmt: Statement, symbol_table: &mut SymbolTable) -> Result<Statement, String> {
    match stmt {
        Statement::Return(expr) => Ok(Statement::Return(resolve_expression(expr, symbol_table)?)),
        Statement::Expression(expression) => Ok(Statement::Expression(resolve_expression(
            expression,
            symbol_table,
        )?)),
        Statement::Null => Ok(Statement::Null),
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => Ok(Statement::If {
            condition: resolve_expression(condition, symbol_table)?,
            then_branch: Box::new(resolve_statement(*then_branch, symbol_table)?),
            else_branch: else_branch
                .map(|else_stmt| resolve_statement(*else_stmt, symbol_table).map(Box::new))
                .transpose()?,
        }),
    }
}

fn resolve_expression(
    expr: Expression,
    symbol_table: &mut SymbolTable,
) -> Result<Expression, String> {
    match expr {
        Expression::Constant(value) => Ok(Expression::Constant(value)),
        Expression::Variable(identifier) => symbol_table
            .get(&identifier)
            .ok_or_else(|| format!("Variable '{}' not declared", identifier))
            .map(|info| Expression::Variable(info.id.clone())),
        Expression::UnaryOp(operator, inner_expr) => Ok(Expression::UnaryOp(
            operator,
            Box::new(resolve_expression(*inner_expr, symbol_table)?),
        )),
        Expression::BinaryOperator {
            operator,
            left,
            right,
        } => Ok(Expression::BinaryOperator {
            operator,
            left: Box::new(resolve_expression(*left, symbol_table)?),
            right: Box::new(resolve_expression(*right, symbol_table)?),
        }),
        Expression::Assignment { lvalue, value } => {
            if !matches!(*lvalue, Expression::Variable(_)) {
                return Err(format!(
                    "Left-hand side of assignment must be a variable. Got {:?}",
                    lvalue
                ));
            }
            Ok(Expression::Assignment {
                lvalue: Box::new(resolve_expression(*lvalue, symbol_table)?),
                value: Box::new(resolve_expression(*value, symbol_table)?),
            })
        }
        Expression::Conditional {
            condition,
            then_expr,
            else_expr,
        } => Ok(Expression::Conditional {
            condition: Box::new(resolve_expression(*condition, symbol_table)?),
            then_expr: Box::new(resolve_expression(*then_expr, symbol_table)?),
            else_expr: Box::new(resolve_expression(*else_expr, symbol_table)?),
        }),
        Expression::CompoundAssignment {
            operator,
            lvalue,
            value,
        } => {
            if !matches!(*lvalue, Expression::Variable(_)) {
                return Err(format!(
                    "Left-hand side of compound assignment must be a variable. Got {:?}",
                    lvalue
                ));
            }
            Ok(Expression::CompoundAssignment {
                operator,
                lvalue: Box::new(resolve_expression(*lvalue, symbol_table)?),
                value: Box::new(resolve_expression(*value, symbol_table)?),
            })
        }
        Expression::PrefixIncrement(expr) => {
            if !matches!(*expr, Expression::Variable(_)) {
                return Err(format!(
                    "Operand of prefix increment must be a variable. Got {:?}",
                    expr
                ));
            }
            Ok(Expression::PrefixIncrement(Box::new(resolve_expression(
                *expr,
                symbol_table,
            )?)))
        }
        Expression::PrefixDecrement(expr) => {
            if !matches!(*expr, Expression::Variable(_)) {
                return Err(format!(
                    "Operand of prefix decrement must be a variable. Got {:?}",
                    expr
                ));
            }
            Ok(Expression::PrefixDecrement(Box::new(resolve_expression(
                *expr,
                symbol_table,
            )?)))
        }
        Expression::PostfixIncrement(expr) => {
            if !matches!(*expr, Expression::Variable(_)) {
                return Err(format!(
                    "Operand of postfix increment must be a variable. Got {:?}",
                    expr
                ));
            }
            Ok(Expression::PostfixIncrement(Box::new(resolve_expression(
                *expr,
                symbol_table,
            )?)))
        }
        Expression::PostfixDecrement(expr) => {
            if !matches!(*expr, Expression::Variable(_)) {
                return Err(format!(
                    "Operand of postfix decrement must be a variable. Got {:?}",
                    expr
                ));
            }
            Ok(Expression::PostfixDecrement(Box::new(resolve_expression(
                *expr,
                symbol_table,
            )?)))
        }
    }
}

// #endregion
