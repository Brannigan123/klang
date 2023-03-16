pub mod ast;

use pest::iterators::Pair;
use pest_derive::Parser;

use self::ast::{
    ArrayEntry, DictionaryEntry, Expression, Identifier, IdentifierList, PrefixOp, Program,
    Statement,
};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct KlangParser {}

pub fn gen_ast(parsed: Pair<Rule>) -> Program {
    match parsed.as_rule() {
        Rule::program_file => parsed
            .into_inner()
            .filter(|p| match p.as_rule() {
                Rule::EOI => false,
                _ => true,
            })
            .map(gen_stmt_ast)
            .into_iter()
            .collect(),
        _ => unreachable!(),
    }
}

fn gen_stmt_ast(parsed: Pair<Rule>) -> Statement {
    match parsed.as_rule() {
        Rule::if_statement => {
            let mut parts = parsed.into_inner();
            let mut if_clause = parts.next().unwrap().into_inner();
            let cond = gen_expr_ast(if_clause.nth(1).unwrap());
            let consequence: Vec<Statement> = if_clause.map(gen_stmt_ast).collect();
            let mut alt_conds = vec![];
            let mut alt_consequences = vec![];
            let mut fallback_consequnce = None;
            for part in parts {
                match part.as_rule() {
                    Rule::if_statement_elif_clause => {
                        let mut clause = part.into_inner();
                        let cond = gen_expr_ast(clause.nth(1).unwrap());
                        let consequence: Vec<Statement> = clause.map(gen_stmt_ast).collect();
                        alt_conds.push(cond);
                        alt_consequences.push(consequence);
                    }
                    Rule::if_statement_else_clause => {
                        let clause = part.into_inner();
                        let consequence: Vec<Statement> =
                            clause.skip(1).map(gen_stmt_ast).collect();
                        fallback_consequnce = Some(consequence);
                    }
                    _ => unreachable!(),
                }
            }
            Statement::IfStmt {
                cond,
                consequence: consequence,
                alt_conds,
                alt_consequences,
                fallback_consequnce,
            }
        }
        Rule::for_in_statement => {
            let mut parts = parsed.into_inner();
            let loop_vars = gen_identifier_nodes(parts.nth(1).unwrap());
            let iterable = gen_expr_ast(parts.nth(1).unwrap());
            let third = parts.next().unwrap();
            let mut filter = None;
            let mut body = vec![];
            match third.as_rule() {
                Rule::item_filter => {
                    filter = Some(gen_expr_ast(third.into_inner().nth(1).unwrap()))
                }
                _ => body.push(gen_stmt_ast(third)),
            };
            for stmt in parts {
                body.push(gen_stmt_ast(stmt));
            }
            Statement::ForInStmt {
                loop_vars,
                iterable,
                filter,
                body,
            }
        }
        Rule::while_statement => {
            let mut parts = parsed.into_inner();
            let cond = gen_expr_ast(parts.nth(1).unwrap());
            let third = parts.next().unwrap();
            let mut filter = None;
            let mut body = vec![];
            match third.as_rule() {
                Rule::item_filter => {
                    filter = Some(gen_expr_ast(third.into_inner().nth(1).unwrap()))
                }
                _ => body.push(gen_stmt_ast(third)),
            };
            for stmt in parts {
                body.push(gen_stmt_ast(stmt));
            }
            Statement::WhileStmt { cond, filter, body }
        }
        Rule::variable_declaration => {
            let mut parts = parsed.into_inner();
            let ids = gen_identifier_nodes(parts.next().unwrap());
            let expr = gen_expr_ast(parts.next().unwrap());
            Statement::LetStmt(ids, expr)
        }
        Rule::function_declaration => {
            let mut parts = parsed.into_inner();
            let id = gen_identifier_node(parts.next().unwrap());
            let func = gen_function_expr(parts.next().unwrap());
            Statement::LetStmt(vec![id], func)
        }
        Rule::expr_statement => {
            let expr = gen_expr_ast(parsed.into_inner().next().unwrap());
            Statement::ExpressionStmt(expr)
        }
        Rule::return_statement => {
            let expr = match parsed.into_inner().nth(1) {
                Some(e) => Some(gen_expr_ast(e)),
                None => None,
            };
            Statement::ReturnStmt(expr)
        }
        _ => unreachable!(),
    }
}

fn gen_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::if_expr => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_or_expr_ast(parts.next().unwrap());
            if let (Some(true_branch), Some(false_branch)) = (parts.next(), parts.next()) {
                expr = Expression::IfExpr {
                    cond: Box::new(expr),
                    consequence: Box::new(gen_expr_ast(true_branch)),
                    fallback_consequnce: Box::new(gen_expr_ast(false_branch)),
                }
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_or_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::logical_or => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_and_expr_ast(parts.next().unwrap());
            for condition in parts {
                expr = Expression::InfixExpr(
                    ast::InfixOp::Or,
                    Box::new(expr),
                    Box::new(gen_and_expr_ast(condition)),
                );
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_and_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::logical_and => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_equality_expr_ast(parts.next().unwrap());
            for condition in parts {
                expr = Expression::InfixExpr(
                    ast::InfixOp::And,
                    Box::new(expr),
                    Box::new(gen_equality_expr_ast(condition)),
                );
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_equality_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::equality => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_comparison_expr_ast(parts.next().unwrap());
            while let Some(op) = parts.next() {
                let op = match op.as_rule() {
                    Rule::eq => ast::InfixOp::Eq,
                    Rule::neq => ast::InfixOp::Neq,
                    _ => unreachable!(),
                };
                expr = Expression::InfixExpr(
                    op,
                    Box::new(expr),
                    Box::new(gen_comparison_expr_ast(parts.next().unwrap())),
                );
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_comparison_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::comparison => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_range_expr_ast(parts.next().unwrap());
            while let Some(op) = parts.next() {
                let op = match op.as_rule() {
                    Rule::ge => ast::InfixOp::Geq,
                    Rule::gt => ast::InfixOp::Gt,
                    Rule::le => ast::InfixOp::Leq,
                    Rule::lt => ast::InfixOp::Lt,
                    _ => unreachable!(),
                };
                expr = Expression::InfixExpr(
                    op,
                    Box::new(expr),
                    Box::new(gen_range_expr_ast(parts.next().unwrap())),
                );
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_range_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::range => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_term_expr_ast(parts.next().unwrap());
            if let Some(end) = parts.next() {
                expr = Expression::InfixExpr(
                    ast::InfixOp::Range,
                    Box::new(expr),
                    Box::new(gen_range_expr_ast(end)),
                );
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_term_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::term => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_factor_expr_ast(parts.next().unwrap());
            while let Some(op) = parts.next() {
                let op = match op.as_rule() {
                    Rule::add => ast::InfixOp::Plus,
                    Rule::minus => ast::InfixOp::Minus,
                    _ => unreachable!(),
                };
                expr = Expression::InfixExpr(
                    op,
                    Box::new(expr),
                    Box::new(gen_factor_expr_ast(parts.next().unwrap())),
                );
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_factor_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::factor => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_unary_expr_ast(parts.next().unwrap());
            while let Some(op) = parts.next() {
                let op = match op.as_rule() {
                    Rule::divide => ast::InfixOp::Divide,
                    Rule::multiply => ast::InfixOp::Times,
                    Rule::remainder => ast::InfixOp::Remainder,
                    _ => unreachable!(),
                };
                expr = Expression::InfixExpr(
                    op,
                    Box::new(expr),
                    Box::new(gen_unary_expr_ast(parts.next().unwrap())),
                );
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_unary_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::unary => {
            let mut parts = parsed.into_inner();
            let part1 = parts.next().unwrap();
            match part1.as_rule() {
                Rule::not => Expression::PrefixExpr(
                    PrefixOp::Not,
                    Box::new(gen_unary_expr_ast(parts.next().unwrap())),
                ),
                Rule::negate => Expression::PrefixExpr(
                    PrefixOp::Negate,
                    Box::new(gen_unary_expr_ast(parts.next().unwrap())),
                ),
                _ => gen_call_expr_ast(part1),
            }
        }
        _ => unreachable!(),
    }
}

fn gen_call_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::call => {
            let mut parts = parsed.into_inner();
            let mut expr = gen_primary_expr_ast(parts.next().unwrap());
            for access in parts {
                match access.as_rule() {
                    Rule::call_arguments => {
                        let args: Vec<Expression> = access.into_inner().map(gen_expr_ast).collect();
                        expr = Expression::InvocationExpr {
                            callable: Box::new(expr),
                            args,
                        }
                    }
                    Rule::index_arguments => {
                        let args: Vec<Expression> = access.into_inner().map(gen_expr_ast).collect();
                        expr = Expression::IndexAccessExpr {
                            indexed: Box::new(expr),
                            args,
                        }
                    }
                    Rule::named_member_access => {
                        expr = Expression::PropertyAccessExpr {
                            object: Box::new(expr),
                            name: gen_identifier_node(access.into_inner().next().unwrap()),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            expr
        }
        _ => unreachable!(),
    }
}

fn gen_primary_expr_ast(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::number => {
            Expression::NumberLiteral(parsed.as_span().as_str().parse::<f64>().unwrap())
        }
        Rule::false_literal => Expression::BooleanLiteral(false),
        Rule::true_literal => Expression::BooleanLiteral(true),
        Rule::identifier => Expression::IdentifierExpr(gen_identifier_node(parsed)),
        Rule::function => gen_function_expr(parsed),
        Rule::array_literal => {
            let mut entries = vec![];
            for entry in parsed.into_inner() {
                if entry.as_span().as_str().starts_with("..") {
                    let expr = gen_expr_ast(entry.into_inner().next().unwrap());
                    entries.push(ArrayEntry::Spread(expr));
                } else {
                    let expr = gen_expr_ast(entry.into_inner().next().unwrap());
                    entries.push(ArrayEntry::Single(expr));
                }
            }
            Expression::ArrayExpr(entries)
        }
        Rule::dictionary_literal => {
            let mut entries = vec![];
            for entry in parsed.into_inner() {
                if entry.as_span().as_str().starts_with("..") {
                    let expr = gen_expr_ast(entry.into_inner().next().unwrap());
                    entries.push(DictionaryEntry::Spread(expr));
                } else {
                    let mut parts = entry.into_inner();
                    let key = gen_expr_ast(parts.next().unwrap());
                    let value = gen_expr_ast(parts.next().unwrap());
                    entries.push(DictionaryEntry::Single(key, value));
                }
            }
            Expression::DictionaryExpr(entries)
        }
        Rule::tuple_literal => {
            let mut entries = vec![];
            for entry in parsed.into_inner() {
                entries.push(gen_expr_ast(entry));
            }
            Expression::TupleExpr(entries)
        }
        _ => unreachable!(),
    }
}

fn gen_function_expr(parsed: Pair<Rule>) -> Expression {
    match parsed.as_rule() {
        Rule::function => {
            let expr = parsed.into_inner().next().unwrap();
            match expr.as_rule() {
                Rule::simple_function => {
                    let mut parts = expr.into_inner();
                    let (params, filter) =
                        get_function_signature(parts.next().unwrap().into_inner());
                    let expr = gen_expr_ast(parts.nth(1).unwrap());
                    Expression::FunctionExpr {
                        params,
                        filter: filter.map(Box::new),
                        body: vec![Statement::ReturnStmt(Some(expr))],
                    }
                }
                Rule::compound_function => {
                    let mut parts = expr.into_inner();
                    let (params, filter) =
                        get_function_signature(parts.next().unwrap().into_inner());
                    let stmts: Vec<Statement> = parts.map(gen_stmt_ast).collect();
                    Expression::FunctionExpr {
                        params,
                        filter: filter.map(Box::new),
                        body: stmts,
                    }
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn get_function_signature(
    mut part1: pest::iterators::Pairs<Rule>,
) -> (Vec<Identifier>, Option<Expression>) {
    let params = match part1.next() {
        Some(p) => gen_identifier_nodes(p),
        None => vec![],
    };
    let filter = match part1.next() {
        Some(f) => Some(gen_expr_ast(f.into_inner().nth(1).unwrap())),
        None => None,
    };
    (params, filter)
}

fn gen_identifier_nodes(parsed: Pair<Rule>) -> IdentifierList {
    match parsed.as_rule() {
        Rule::identifier_list => parsed
            .into_inner()
            .map(gen_identifier_node)
            .into_iter()
            .collect(),
        _ => unreachable!(),
    }
}

fn gen_identifier_node(parsed: Pair<Rule>) -> Identifier {
    match parsed.as_rule() {
        Rule::identifier => Identifier(parsed.as_str().to_owned()),
        _ => unreachable!(),
    }
}
