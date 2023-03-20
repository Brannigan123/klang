pub mod builtins;
pub mod env;
pub mod obj;

use std::cell::RefCell;
use std::rc::Rc;

use crate::parser::ast::{
    ASTNodePosition, ArrayEntry, DictionaryEntry, Expression, Identifier, IdentifierList, InfixOp,
    PrefixOp, Program, Statement, Statements, StringPart,
};

use self::env::Environment;
use self::obj::{BuiltinFunction, ErrorEntry, Object};

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn register_ident(&mut self, ident: Identifier, object: Object) -> Object {
        let Identifier(name) = ident;
        self.env.borrow_mut().set(&name, object.clone());
        object
    }

    pub fn eval_ident(&mut self, ident: Identifier, pos: ASTNodePosition) -> Object {
        let Identifier(name) = ident;
        let borrow_env = self.env.borrow();
        let var = borrow_env.get(&name);
        match var {
            Some(o) => o,
            None => Object::Error(ErrorEntry {
                position: pos,
                message: format!("identifier not found: {}", name),
                parents: vec![],
            }),
        }
    }

    fn returned(&mut self, object: Object) -> Object {
        match object {
            Object::ReturnValue(v) => *v,
            o => o,
        }
    }

    pub fn eval_program(&mut self, prog: Program) -> Object {
        let return_data = self.eval_blockstmt(prog);
        self.returned(return_data)
    }

    pub fn eval_blockstmt(&mut self, mut prog: Program) -> Object {
        match prog.len() {
            0 => Object::Null,
            1 => self.eval_statement(prog.remove(0)),
            _ => {
                let s = prog.remove(0);
                let object = self.eval_statement(s);
                if object.is_returned() || object.is_error() {
                    object
                } else {
                    self.eval_blockstmt(prog)
                }
            }
        }
    }

    pub fn eval_statement(&mut self, stmt: Statement) -> Object {
        match stmt {
            Statement::ExpressionStmt(_, expression) => self.eval_expr(expression),
            Statement::ReturnStmt(_, expression) => Object::ReturnValue(Box::new(
                expression
                    .map(|e| self.eval_expr(e))
                    .unwrap_or(Object::Null),
            )),
            Statement::LetStmt(_, ident, expression) => self.eval_let_stmt(expression, ident),
            Statement::IfStmt {
                pos: _,
                cond,
                consequence,
                alt_conds,
                alt_consequences,
                fallback_consequnce,
            } => self.eval_if_stmt(
                cond,
                consequence,
                alt_conds,
                alt_consequences,
                fallback_consequnce,
            ),
            Statement::ForInStmt {
                pos: _,
                loop_vars,
                iterable,
                filter,
                body,
            } => self.eval_for_stmt(loop_vars, iterable, filter, body),
            Statement::WhileStmt {
                pos: _,
                cond,
                filter,
                body,
            } => self.eval_while_stmt(cond, filter, body),
        }
    }

    fn eval_let_stmt(&mut self, expression: Expression, ident: Vec<Identifier>) -> Object {
        let pos = expression.position();
        let object = self.eval_expr(expression);
        match ident.len() {
            1 => self.register_ident(ident.into_iter().next().unwrap(), object),
            _ => {
                let mut errors = vec![];
                match object.clone() {
                    Object::Array(elements) | Object::Tuple(elements) => {
                        for (id, val) in ident.into_iter().zip(elements.into_iter()) {
                            if let Object::Error(e) = self.register_ident(id, val) {
                                errors.push(e);
                            }
                        }
                    }
                    Object::Dictionary(entries) => {
                        for id in ident {
                            let value = self.eval_index(
                                pos,
                                Object::Dictionary(entries.clone()),
                                Object::String(id.clone().0),
                            );
                            if let Object::Error(e) = self.register_ident(id, value) {
                                errors.push(e);
                            }
                        }
                    }
                    _ => {
                        errors.push(ErrorEntry {
                            position: pos,
                            message: format!("Can not destructure: {}", object),
                            parents: vec![],
                        });
                    } //TODO error message
                }
                if errors.is_empty() {
                    object
                } else {
                    Object::Error(ErrorEntry {
                        position: pos,
                        message: format!("Can not assign value"),
                        parents: errors,
                    })
                }
            }
        }
    }

    pub fn eval_if_stmt(
        &mut self,
        cond: Expression,
        conse: Statements,
        alt_cond: Vec<Expression>,
        alt_conse: Vec<Statements>,
        fallback_conse: Option<Statements>,
    ) -> Object {
        let matched = self.eval_expr(cond);
        match Evaluator::otb(matched) {
            Ok(b) => {
                if b {
                    self.eval_blockstmt(conse)
                } else {
                    for (cond, conse) in alt_cond.into_iter().zip(alt_conse.into_iter()) {
                        let matched = self.eval_expr(cond);
                        match Evaluator::otb(matched) {
                            Ok(b) => {
                                if b {
                                    return self.eval_blockstmt(conse);
                                }
                            }
                            Err(err) => return err,
                        }
                    }
                    match fallback_conse {
                        Some(else_conse) => self.eval_blockstmt(else_conse),
                        _ => Object::Null,
                    }
                }
            }
            Err(err) => err,
        }
    }

    pub fn eval_for_stmt(
        &mut self,
        loop_vars: IdentifierList,
        iterable: Expression,
        filter: Option<Expression>,
        body: Statements,
    ) -> Object {
        let object = self.eval_expr(iterable.clone());
        let items: Vec<Vec<Object>> = match object.clone() {
            Object::Array(elements) | Object::Tuple(elements) => {
                elements.into_iter().map(|e| vec![e]).collect()
            }
            Object::Dictionary(entries) => entries
                .into_keys()
                .map(|k| {
                    let v = self.eval_index(iterable.position(), object.clone(), k.clone());
                    vec![k, v]
                })
                .collect(),
            Object::String(s) => s
                .chars()
                .into_iter()
                .map(|c| vec![Object::String(c.to_string())])
                .collect(),
            _ => {
                return Object::Error(ErrorEntry {
                    position: iterable.position(),
                    message: format!("Expected an iterable. Found: {}", object),
                    parents: vec![],
                })
            } //TODO error message
        };
        for item in items {
            for (id, val) in loop_vars.clone().into_iter().zip(item.into_iter()) {
                self.register_ident(id, val);
                if let Some(f) = filter.clone() {
                    let matched = self.eval_expr(f);
                    match Evaluator::otb(matched) {
                        Ok(b) => {
                            if !b {
                                continue;
                            }
                        }
                        Err(err) => return err,
                    }
                }
                let res = self.eval_blockstmt(body.clone());
                if res.is_returned() || res.is_error() {
                    return res;
                }
            }
        }
        Object::Null
    }

    pub fn eval_while_stmt(
        &mut self,
        cond: Expression,
        filter: Option<Expression>,
        body: Statements,
    ) -> Object {
        loop {
            let matched = self.eval_expr(cond.clone());
            match Evaluator::otb(matched) {
                Ok(b) => {
                    if !b {
                        break;
                    }
                }
                Err(err) => return err,
            }
            if let Some(f) = filter.clone() {
                let matched = self.eval_expr(f);
                match Evaluator::otb(matched) {
                    Ok(b) => {
                        if !b {
                            continue;
                        }
                    }
                    Err(err) => return err,
                }
            }
            let res = self.eval_blockstmt(body.clone());
            if res.is_returned() || res.is_error() {
                return res;
            }
        }

        Object::Null
    }

    pub fn eval_expr(&mut self, expr: Expression) -> Object {
        match expr {
            Expression::IdentifierExpr(pos, i) => self.eval_ident(i, pos),
            Expression::PrefixExpr(_, prefix, expression) => self.eval_prefix(&prefix, *expression),
            Expression::InfixExpr(_, infix, expr1, expr2) => {
                self.eval_infix(&infix, *expr1, *expr2)
            }
            Expression::IfExpr {
                cond,
                pos: _,
                consequence,
                fallback_consequnce,
            } => self.eval_if_expr(*cond, *consequence, *fallback_consequnce),
            Expression::FunctionExpr {
                pos: _,
                params,
                filter,
                body,
            } => self.eval_fn(params, filter, body),
            Expression::InvocationExpr {
                pos: _,
                callable,
                args,
            } => self.eval_call(*callable, args),
            Expression::ArrayExpr(_, exprs) => self.eval_array(exprs),
            Expression::DictionaryExpr(_, hash_exprs) => self.eval_hash(hash_exprs),
            Expression::IndexAccessExpr {
                pos: _,
                indexed,
                args,
            } => {
                let mut val = self.eval_expr(*indexed);
                for arg in args {
                    let argv = self.eval_expr(arg.clone());
                    val = self.eval_index(arg.position(), val, argv);
                }
                val
            }
            Expression::PropertyAccessExpr {
                pos: _,
                object,
                name,
            } => {
                let indexed = self.eval_expr(*object.clone());
                self.eval_index(object.position(), indexed, Object::String(name.0))
            }
            Expression::NumberLiteral(_, n) => Object::Number(n),
            Expression::BooleanLiteral(_, b) => Object::Boolean(b),
            Expression::StringExpr(_, parts) => {
                let mut string = "".to_owned();
                for part in parts {
                    match part {
                        StringPart::Literal(_, s) => string = format!("{}{}", string, s),
                        StringPart::Variable(pos, i) => {
                            string = format!("{}{}", string, self.eval_ident(i, pos))
                        }
                        StringPart::Expression(_, e) => {
                            string = format!("{}{}", string, self.eval_expr(e))
                        }
                    }
                }
                Object::String(string.to_owned())
            }
        }
    }

    pub fn eval_if_expr(
        &mut self,
        cond: Expression,
        conse: Expression,
        fallback_conse: Expression,
    ) -> Object {
        let matched = self.eval_expr(cond);
        match Evaluator::otb(matched) {
            Ok(b) => {
                if b {
                    self.eval_expr(conse)
                } else {
                    self.eval_expr(fallback_conse)
                }
            }
            Err(err) => err,
        }
    }

    pub fn eval_prefix(&mut self, prefix: &PrefixOp, expr: Expression) -> Object {
        let object = self.eval_expr(expr);
        match *prefix {
            PrefixOp::Not => match Evaluator::otb(object) {
                Ok(b) => Object::Boolean(!b),
                Err(err) => err,
            },
            PrefixOp::Negate => match Evaluator::otn(object) {
                Ok(i) => Object::Number(-i),
                Err(err) => err,
            },
        }
    }

    pub fn eval_infix(&mut self, infix: &InfixOp, expr1: Expression, expr2: Expression) -> Object {
        let object1 = self.eval_expr(expr1);
        let object2 = self.eval_expr(expr2.clone());
        match *infix {
            InfixOp::Plus => self.object_add(expr2.position(), object1, object2),
            InfixOp::Minus => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Number(n1 - n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Divide => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Number(n1 / n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Remainder => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Number(n1 % n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Times => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Number(n1 * n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Eq => Object::Boolean(object1 == object2),
            InfixOp::Neq => Object::Boolean(object1 != object2),
            InfixOp::Geq => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Boolean(n1 >= n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Gt => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Boolean(n1 > n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Leq => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Boolean(n1 <= n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Lt => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => Object::Boolean(n1 < n2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Range => {
                let n1 = Evaluator::otn(object1);
                let n2 = Evaluator::otn(object2);
                match (n1, n2) {
                    (Ok(n1), Ok(n2)) => {
                        let mut elements = vec![];
                        let (mut start, end, rev) = if n1 > n2 {
                            (n2, n1, true)
                        } else {
                            (n1, n2, false)
                        };
                        loop {
                            elements.push(Object::Number(start));
                            start += 1.0;
                            if start > end {
                                break;
                            }
                        }
                        if rev {
                            elements.reverse();
                        }
                        Object::Array(elements)
                    }
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::Or => {
                let b1 = Evaluator::otb(object1);
                let b2 = Evaluator::otb(object2);
                match (b1, b2) {
                    (Ok(b1), Ok(b2)) => Object::Boolean(b1 || b2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
            InfixOp::And => {
                let b1 = Evaluator::otb(object1);
                let b2 = Evaluator::otb(object2);
                match (b1, b2) {
                    (Ok(b1), Ok(b2)) => Object::Boolean(b1 && b2),
                    (Err(err), _) | (_, Err(err)) => err,
                }
            }
        }
    }

    pub fn eval_fn(
        &mut self,
        params: Vec<Identifier>,
        filter: Option<Box<Expression>>,
        body: Program,
    ) -> Object {
        Object::Function(params, filter.map(|e| *e), body, Rc::clone(&self.env))
    }

    pub fn eval_call(&mut self, fn_expr: Expression, args_expr: Vec<Expression>) -> Object {
        let fn_object = self.eval_expr(fn_expr.clone());
        let fn_ = Evaluator::otf(fn_object);
        match fn_ {
            Object::Function(params, filter, body, f_env) => {
                self.eval_fn_call(fn_expr.position(), args_expr, params, filter, body, &f_env)
            }
            Object::Builtin(_, num_params, b_fn) => {
                self.eval_builtin_call(fn_expr.position(), args_expr, num_params, b_fn)
            }
            o_err => o_err,
        }
    }

    fn eval_fn_call(
        &mut self,
        pos: ASTNodePosition,
        args_expr: Vec<Expression>,
        params: IdentifierList,
        filter: Option<Expression>,
        body: Program,
        f_env: &Rc<RefCell<Environment>>,
    ) -> Object {
        if args_expr.len() != params.len() {
            Object::Error(ErrorEntry {
                position: pos,
                message: format!(
                    "Wrong number of arguments: {} expected but {} given",
                    params.len(),
                    args_expr.len()
                ),
                parents: vec![],
            })
        } else {
            let args = args_expr
                .into_iter()
                .map(|e| self.eval_expr(e))
                .collect::<Vec<_>>();
            let old_env = Rc::clone(&self.env);
            let mut new_env = Environment::new_with_outer(Rc::clone(f_env));
            let zipped = params.into_iter().zip(args);
            for (_, (Identifier(name), o)) in zipped.enumerate() {
                new_env.set(&name, o);
            }
            self.env = Rc::new(RefCell::new(new_env));

            let object = match filter.clone() {
                Some(f) => {
                    let matched = self.eval_expr(f.clone());
                    match Evaluator::otb(matched) {
                        Ok(b) => {
                            if b {
                                self.eval_blockstmt(body)
                            } else {
                                Object::Error(ErrorEntry {
                                    position: f.position(),
                                    message: format!("Failed to match function filter"),
                                    parents: vec![],
                                })
                            }
                        }
                        Err(err) => err,
                    }
                }
                None => self.eval_blockstmt(body),
            };

            self.env = old_env;
            self.returned(object)
        }
    }

    fn eval_builtin_call(
        &mut self,
        pos: ASTNodePosition,
        args_expr: Vec<Expression>,
        num_params: usize,
        b_fn: BuiltinFunction,
    ) -> Object {
        if args_expr.len() != num_params {
            Object::Error(ErrorEntry {
                position: pos,
                message: format!(
                    "Wrong number of arguments: {} expected but {} given",
                    num_params,
                    args_expr.len()
                ),
                parents: vec![],
            })
        } else {
            let args = args_expr
                .into_iter()
                .map(|e| self.eval_expr(e))
                .collect::<Vec<_>>();
            b_fn(args).unwrap_or_else(|msg| {
                Object::Error(ErrorEntry {
                    position: pos,
                    message: msg,
                    parents: vec![],
                })
            })
        }
    }

    pub fn eval_array(&mut self, entries: Vec<ArrayEntry>) -> Object {
        let mut new_vec = vec![];
        for entry in entries {
            match entry {
                ArrayEntry::Single(e) => new_vec.push(self.eval_expr(e)),
                ArrayEntry::Spread(e) => match self.eval_expr(e.clone()) {
                    Object::Array(elements) | Object::Tuple(elements) => {
                        elements.into_iter().for_each(|e| new_vec.push(e))
                    }
                    Object::Dictionary(entries) => {
                        entries.into_values().for_each(|e| new_vec.push(e))
                    }
                    Object::String(s) => s
                        .chars()
                        .into_iter()
                        .map(|c| Object::String(c.to_string()))
                        .for_each(|e| new_vec.push(e)),
                    _ => new_vec.push(Object::Error(ErrorEntry {
                        position: e.position(),
                        message: format!("Expected an iterable"),
                        parents: vec![],
                    })),
                },
            };
        }
        maybe_extract_errors(new_vec, Object::Array, format!("Failed to create array"))
    }

    pub fn object_add(&mut self, pos: ASTNodePosition, object1: Object, object2: Object) -> Object {
        match (object1, object2) {
            (Object::Number(n1), Object::Number(n2)) => Object::Number(n1 + n2),
            (Object::String(s1), Object::String(s2)) => Object::String(s1 + &s2),
            (Object::Error(e1), Object::Error(e2)) => Object::Error(ErrorEntry {
                position: pos,
                message: format!("Operation failed."),
                parents: vec![e1, e2],
            }),
            (Object::Error(e), _) | (_, Object::Error(e)) => Object::Error(e),
            (x, y) => Object::Error(ErrorEntry {
                position: pos,
                message: format!("Operator not defined on arguments: '{}' and '{}'", x, y),
                parents: vec![],
            }),
        }
    }

    pub fn eval_hash(&mut self, entries: Vec<DictionaryEntry>) -> Object {
        let mut new_vec = vec![];
        for entry in entries {
            match entry {
                DictionaryEntry::Single(k, v) => new_vec.push(self.eval_pair((k, v))),
                DictionaryEntry::Spread(e) => {
                    let object = self.eval_expr(e.clone());
                    match object {
                        Object::Dictionary(addditional_entries) => addditional_entries
                            .into_iter()
                            .for_each(|e| new_vec.push(e)),
                        _ => {
                            return Object::Error(ErrorEntry {
                                position: e.position(),
                                message: format!("Expected an iterable. Found {}", object),
                                parents: vec![],
                            })
                        }
                    }
                }
            };
        }
        maybe_extract_pair_errors(
            new_vec,
            |p| Object::Dictionary(p.into_iter().collect()),
            format!("Failed to create dictionary"),
        )
    }

    fn eval_pair(&mut self, tuple: (Expression, Expression)) -> (Object, Object) {
        let (l, e) = tuple;
        let lo = self.eval_expr(l);
        let hash = Evaluator::oth(lo);
        let object = self.eval_expr(e);
        (hash, object)
    }

    pub fn eval_index(&mut self, pos: ASTNodePosition, target: Object, key: Object) -> Object {
        match target {
            Object::Array(arr) => match Evaluator::otn(key) {
                Ok(index_number) => arr
                    .into_iter()
                    .nth(index_number as usize)
                    .unwrap_or(Object::Null),
                Err(err) => err,
            },
            Object::Dictionary(mut hash) => {
                let name = Evaluator::oth(key);
                match name {
                    Object::Error(_) => name,
                    _ => hash.remove(&name).unwrap_or(Object::Null),
                }
            }
            o => Object::Error(ErrorEntry {
                position: pos,
                message: format!("Can not access index given {}", o),
                parents: vec![],
            }),
        }
    }

    pub fn otb(object: Object) -> Result<bool, Object> {
        match object {
            Object::Boolean(b) => Ok(b),
            Object::Number(n) => Ok(n > 0.0),
            Object::String(s) => Ok(!s.is_empty()),
            Object::Array(v) | Object::Tuple(v) => Ok(!v.is_empty()),
            Object::Dictionary(v) => Ok(!v.is_empty()),
            Object::ReturnValue(v) => Self::otb(*v),
            Object::Function(_, _, _, _) | Object::Builtin(_, _, _) => Ok(true),
            Object::Null => Ok(false),
            Object::Error(s) => Err(Object::Error(s)),
        }
    }

    pub fn otn(object: Object) -> Result<f64, Object> {
        match object {
            Object::Number(n) => Ok(n),
            Object::String(s) => s.parse().map_err(|_| {
                Object::Error(ErrorEntry {
                    position: (0, 0),
                    message: format!("Expected a number. Found {}", s),
                    parents: vec![],
                })
            }),
            Object::Error(s) => Err(Object::Error(s)),
            i => Err(Object::Error(ErrorEntry {
                position: (0, 0),
                message: format!("Expected a number. Found {}", i),
                parents: vec![],
            })),
        }
    }

    pub fn otf(object: Object) -> Object {
        match object {
            Object::Function(_, _, _, _) | Object::Builtin(_, _, _) => object,
            Object::Error(s) => Object::Error(s),
            f => Object::Error(ErrorEntry {
                position: (0, 0),
                message: format!("Expected a function. Found {}", f),
                parents: vec![],
            }),
        }
    }

    pub fn oth(object: Object) -> Object {
        match object {
            Object::Number(n) => Object::Number(n),
            Object::Boolean(b) => Object::Boolean(b),
            Object::String(s) => Object::String(s),
            Object::Error(s) => Object::Error(s),
            x => Object::Error(ErrorEntry {
                position: (0, 0),
                message: format!("Expected a hashable value. Found {}", x),
                parents: vec![],
            }),
        }
    }

    pub fn ots(object: Object) -> String {
        format!("{}", object)
    }
}

fn maybe_extract_errors<F>(objs: Vec<Object>, to_object: F, error_msg: String) -> Object
where
    F: Fn(Vec<Object>) -> Object,
{
    let errors: Vec<ErrorEntry> = objs
        .iter()
        .filter_map(|o| {
            if let Object::Error(e) = o {
                Some(e.clone())
            } else {
                None
            }
        })
        .collect();
    if errors.is_empty() {
        to_object(objs)
    } else {
        Object::Error(ErrorEntry {
            position: errors.first().unwrap().position,
            message: error_msg,
            parents: errors,
        })
    }
}

fn maybe_extract_pair_errors<F>(
    objs: Vec<(Object, Object)>,
    to_object: F,
    error_msg: String,
) -> Object
where
    F: Fn(Vec<(Object, Object)>) -> Object,
{
    let errors: Vec<ErrorEntry> = objs
        .iter()
        .filter_map(|o| match o {
            (Object::Error(e1), Object::Error(e2)) => Some(ErrorEntry {
                position: e1.position,
                message: format!("Failed to create pair"),
                parents: vec![e1.clone(), e2.clone()],
            }),
            (Object::Error(e), _) | (_, Object::Error(e)) => Some(e.clone()),
            _ => None,
        })
        .collect();
    if errors.is_empty() {
        to_object(objs)
    } else {
        Object::Error(ErrorEntry {
            position: errors.first().unwrap().position,
            message: error_msg,
            parents: errors,
        })
    }
}
