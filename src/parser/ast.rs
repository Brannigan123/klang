pub type ASTNodePosition = (usize, usize);

pub type Program = Statements;

pub type Statements = Vec<Statement>;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    IfStmt {
        pos: ASTNodePosition,
        cond: Expression,
        consequence: Statements,
        alt_conds: Vec<Expression>,
        alt_consequences: Vec<Statements>,
        fallback_consequnce: Option<Statements>,
    },
    ForInStmt {
        pos: ASTNodePosition,
        loop_vars: IdentifierList,
        iterable: Expression,
        filter: Option<Expression>,
        body: Statements,
    },
    WhileStmt {
        pos: ASTNodePosition,
        cond: Expression,
        filter: Option<Expression>,
        body: Statements,
    },
    ReturnStmt(ASTNodePosition, Option<Expression>),
    LetStmt(ASTNodePosition, IdentifierList, Expression),
    ExpressionStmt(ASTNodePosition, Expression),
}

pub type Expressions = Vec<Expression>;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    FunctionExpr {
        pos: ASTNodePosition,
        params: IdentifierList,
        filter: Option<Box<Expression>>,
        body: Statements,
    },
    IfExpr {
        pos: ASTNodePosition,
        cond: Box<Expression>,
        consequence: Box<Expression>,
        fallback_consequnce: Box<Expression>,
    },
    InfixExpr(ASTNodePosition, InfixOp, Box<Expression>, Box<Expression>),
    PrefixExpr(ASTNodePosition, PrefixOp, Box<Expression>),
    InvocationExpr {
        pos: ASTNodePosition,
        callable: Box<Expression>,
        args: Expressions,
    },
    IndexAccessExpr {
        pos: ASTNodePosition,
        indexed: Box<Expression>,
        args: Expressions,
    },
    PropertyAccessExpr {
        pos: ASTNodePosition,
        object: Box<Expression>,
        name: Identifier,
    },
    IdentifierExpr(ASTNodePosition, Identifier),
    NumberLiteral(ASTNodePosition, f64),
    BooleanLiteral(ASTNodePosition, bool),
    StringExpr(ASTNodePosition, Vec<StringPart>),
    ArrayExpr(ASTNodePosition, Vec<ArrayEntry>),
    DictionaryExpr(ASTNodePosition, Vec<DictionaryEntry>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum StringPart {
    Literal(ASTNodePosition, String),
    Variable(ASTNodePosition, Identifier),
    Expression(ASTNodePosition, Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub enum ArrayEntry {
    Single(Expression),
    Spread(Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub enum DictionaryEntry {
    Single(Expression, Expression),
    Spread(Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub enum PrefixOp {
    Not,
    Negate,
}

#[derive(PartialEq, Debug, Clone)]
pub enum InfixOp {
    Or,
    And,
    Eq,
    Neq,
    Leq,
    Lt,
    Geq,
    Gt,
    Range,
    Plus,
    Minus,
    Divide,
    Times,
    Remainder,
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Identifier(pub String);

pub type IdentifierList = Vec<Identifier>;

impl Statement {
    pub fn position(&self) -> ASTNodePosition {
        match self {
            Statement::IfStmt { pos, .. } => *pos,
            Statement::ForInStmt { pos, .. } => *pos,
            Statement::WhileStmt { pos, .. } => *pos,
            Statement::ReturnStmt(pos, _) => *pos,
            Statement::LetStmt(pos, _, _) => *pos,
            Statement::ExpressionStmt(pos, _) => *pos,
        }
    }
}

impl Expression {
    pub fn position(&self) -> ASTNodePosition {
        match self {
            Expression::FunctionExpr { pos, .. } => *pos,
            Expression::IfExpr { pos, .. } => *pos,
            Expression::InfixExpr(pos, _, _, _) => *pos,
            Expression::PrefixExpr(pos, _, _) => *pos,
            Expression::InvocationExpr { pos, .. } => *pos,
            Expression::IndexAccessExpr { pos, .. } => *pos,
            Expression::PropertyAccessExpr { pos, .. } => *pos,
            Expression::IdentifierExpr(pos, _) => *pos,
            Expression::NumberLiteral(pos, _) => *pos,
            Expression::BooleanLiteral(pos, _) => *pos,
            Expression::StringExpr(pos, _) => *pos,
            Expression::ArrayExpr(pos, _) => *pos,
            Expression::DictionaryExpr(pos, _) => *pos,
        }
    }
}
