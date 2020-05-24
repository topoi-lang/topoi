use super::*;

pub enum Expression<'ast> {
    PrimitiveExpression(Primitive<'ast>),

    // Testing
    // IsZeroExpression(IsZeroExpression<'ast>),
    // SuccExpression(SuccExpression<'ast>),
    // PredExpression(PredExpression<'ast>),
    IfExpression(IfExpression<'ast>),
}

pub enum Primitive<'ast> {
    Nat(&'ast str), // natural number
    Bool(bool),
    Integer(&'ast str),
    String_(&'ast str),
}

// pub struct IsZeroExpression {}
// pub struct SuccExpression {}
// pub struct PredExpression<'ast> {}

pub struct IfExpression<'ast> {
    pub predicate: Box<ExpressionNode<'ast>>,
    pub consequent: Box<ExpressionNode<'ast>>,
    pub alternate: Box<Expression<'ast>>,
}

pub type ExpressionNode<'ast> = Node<Expression<'ast>>;
