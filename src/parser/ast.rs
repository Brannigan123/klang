pub type Program = Statements;

pub type Statements = Vec<Statement>;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    IfStmt {
        cond: Expression,
        consequence: Statements,
        alt_conds: Vec<Expression>,
        alt_consequences: Vec<Statements>,
        fallback_consequnce: Option<Statements>,
    },
    ForInStmt {
        loop_vars: IdentifierList,
        iterable: Expression,
        filter: Option<Expression>,
        body: Statements,
    },
    WhileStmt {
        cond: Expression,
        filter: Option<Expression>,
        body: Statements,
    },
    ReturnStmt(Option<Expression>),
    LetStmt(IdentifierList, Expression),
    ExpressionStmt(Expression),
}

pub type Expressions = Vec<Expression>;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    IfExpr {
        cond: Box<Expression>,
        consequence: Box<Expression>,
        alt_conds: Expressions,
        alt_consequencess: Expressions,
        fallback_consequnce: Box<Expression>,
    },
    InfixExpr(InfixOp, Box<Expression>, Box<Expression>),
    PrefixExpr(PrefixOp, Box<Expression>),
    InvocationExpr {
        callable: Box<Expression>,
        args: Expressions,
    },
    IndexAccessExpr {
        indexed: Box<Expression>,
        args: Expressions,
    },
    PropertyAccessExpr {
        object: Box<Expression>,
        name: Identifier,
    },
    IdentifierExpr(Identifier),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    ArrayExpr(Vec<ArrayEntry>),
    DictionaryExpr(Vec<DictionaryEntry>),
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
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Identifier(pub String);

pub type IdentifierList = Vec<Identifier>;
