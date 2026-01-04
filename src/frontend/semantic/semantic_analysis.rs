use crate::frontend::program_ast::{Block, BlockItem, Declaration, ForInitialization, ProgramAst};
use crate::frontend::semantic::symbol_table::SymbolTable;
use crate::utils::tmp_var_counter::TMP_VAR_COUNT;
use crate::{Expression, FunctionDefinition, Statement};
use std::sync::atomic::Ordering::SeqCst;

//todo: Implement panic mode for error handling?

pub fn run_semantic_analysis(program_ast: ProgramAst) -> Result<ProgramAst, String> {
    let mut symbol_table = SymbolTable::new();

    // Step 1: Variable resolution
    let resolved_ast = resolve_variables(program_ast, &mut symbol_table)?;
    // Step 2: Loop labeling
    Ok(label_loops(resolved_ast)?)
}

// #region loop Labeling
fn create_loop_label() -> String {
    format!("loop_{}", TMP_VAR_COUNT.fetch_add(1, SeqCst))
}

fn label_loops(ast: ProgramAst) -> Result<ProgramAst, String> {
    let ProgramAst::Program(function) = ast;
    let FunctionDefinition::Function { identifier, body } = function;
    let Block { items } = body;
    let mut new_items = Vec::new();
    for item in items {
        match item {
            BlockItem::Statement(stmt) => {
                new_items.push(BlockItem::Statement(label_loops_in_statement(stmt, None)?));
            }
            _ => {
                new_items.push(item);
            }
        }
    }
    Ok(ProgramAst::Program(FunctionDefinition::Function {
        identifier,
        body: Block { items: new_items },
    }))
}

fn label_loops_in_statement(
    stmt: Statement,
    current_label: Option<String>,
) -> Result<Statement, String> {
    match stmt {
        Statement::DoWhile {
            body,
            condition,
            label: None,
        } => {
            let label = create_loop_label();
            let labeled_body = label_loops_in_statement(*body, Some(label.clone()))?;
            Ok(Statement::DoWhile {
                body: Box::new(labeled_body),
                condition,
                label: Some(label),
            })
        }
        Statement::While {
            condition,
            body,
            label: None,
        } => {
            let label = create_loop_label();
            let labeled_body = label_loops_in_statement(*body, Some(label.clone()))?;
            Ok(Statement::While {
                condition,
                body: Box::new(labeled_body),
                label: Some(label),
            })
        }
        Statement::For {
            initialization,
            condition,
            post,
            body,
            ..
        } => {
            let label = create_loop_label();
            let labeled_body = label_loops_in_statement(*body, Some(label.clone()))?;
            Ok(Statement::For {
                initialization,
                condition,
                post,
                body: Box::new(labeled_body),
                label: Some(label),
            })
        }
        Statement::CompoundStatement(block) => {
            let Block { items } = block;
            let mut new_items = Vec::new();
            for item in items {
                match item {
                    BlockItem::Statement(s) => {
                        new_items.push(BlockItem::Statement(label_loops_in_statement(
                            s,
                            current_label.clone(),
                        )?));
                    }
                    _ => {
                        new_items.push(item);
                    }
                }
            }
            Ok(Statement::CompoundStatement(Block { items: new_items }))
        }
        Statement::Break { label: None } => {
            if let Some(label) = current_label {
                Ok(Statement::Break {
                    label: Some(format!("break_{}", label)),
                })
            } else {
                Err("Break statement not within a loop".to_string())
            }
        }
        Statement::Break { label: Some(id) } => panic!(
            "Break statement should have been labeled already: {}. This is a compiler bug.",
            id
        ),
        Statement::Continue { label: None } => {
            if let Some(label) = current_label {
                Ok(Statement::Continue {
                    label: Some(format!("continue_{}", label)),
                })
            } else {
                Err("Continue statement not within a loop".to_string())
            }
        }
        Statement::Continue { label: Some(id) } => panic!(
            "Continue statement should have been labeled already: {}. This is a compiler bug.",
            id
        ),
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => Ok(Statement::If {
            condition,
            then_branch: Box::new(label_loops_in_statement(
                *then_branch,
                current_label.clone(),
            )?),
            else_branch: else_branch
                .map(|else_stmt| {
                    label_loops_in_statement(*else_stmt, current_label.clone()).map(Box::new)
                })
                .transpose()?,
        }),
        _ => Ok(stmt),
    }
}

// # endregion

// #region Variable Resolution

fn resolve_variables(
    ast: ProgramAst,
    symbol_table: &mut SymbolTable,
) -> Result<ProgramAst, String> {
    let ProgramAst::Program(function) = ast;
    Ok(ProgramAst::Program(resolve_function(
        function,
        symbol_table,
    )?))
}

fn resolve_function(
    func: FunctionDefinition,
    symbol_table: &mut SymbolTable,
) -> Result<FunctionDefinition, String> {
    let FunctionDefinition::Function { identifier, body } = func;
    Ok(FunctionDefinition::Function {
        identifier,
        body: resolve_block(body, symbol_table)?,
    })
}

fn resolve_block(
    block: crate::frontend::program_ast::Block,
    symbol_table: &mut SymbolTable,
) -> Result<crate::frontend::program_ast::Block, String> {
    let mut resolved_items = Vec::new();
    symbol_table.enter_scope();

    for item in block.items {
        resolved_items.push(resolve_block_item(item, symbol_table)?);
    }

    symbol_table.exit_scope();

    Ok(Block {
        items: resolved_items,
    })
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
            if symbol_table.is_in_current_scope(&identifier) {
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
        Statement::CompoundStatement(block) => Ok(Statement::CompoundStatement(resolve_block(
            block,
            symbol_table,
        )?)),
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Ok(Statement::DoWhile {
            body: Box::new(resolve_statement(*body, symbol_table)?),
            condition: resolve_expression(condition, symbol_table)?,
            label: label,
        }),
        Statement::While {
            condition,
            body,
            label,
        } => Ok(Statement::While {
            condition: resolve_expression(condition, symbol_table)?,
            body: Box::new(resolve_statement(*body, symbol_table)?),
            label: label,
        }),
        Statement::For {
            initialization,
            condition,
            post,
            body,
            label,
        } => {
            symbol_table.enter_scope(); // New scope for for-loop
            let resolved_initialization = initialization
                .map(|for_init| match for_init {
                    ForInitialization::InitExpression(expr) => expr
                        .map(|e| resolve_expression(e, symbol_table))
                        .transpose()
                        .map(ForInitialization::InitExpression),
                    ForInitialization::InitDeclaration(decl) => {
                        resolve_declaration(decl, symbol_table)
                            .map(ForInitialization::InitDeclaration)
                    }
                })
                .transpose()?;
            let resolved_condition = condition
                .map(|cond| resolve_expression(cond, symbol_table))
                .transpose()?;
            let resolved_post = post
                .map(|p| resolve_expression(p, symbol_table))
                .transpose()?;
            let resolved_body = Box::new(resolve_statement(*body, symbol_table)?);

            symbol_table.exit_scope();

            Ok(Statement::For {
                initialization: resolved_initialization,
                condition: resolved_condition,
                post: resolved_post,
                body: resolved_body,
                label: label,
            })
        }
        Statement::Break { label: identifier } => Ok(Statement::Break { label: identifier }),
        Statement::Continue { label: identifier } => Ok(Statement::Continue { label: identifier }),
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
