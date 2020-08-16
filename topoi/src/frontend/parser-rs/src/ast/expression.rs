use super::node::{Node, NodeList};

pub enum Expression<'ast> {
    IdentifierExpression(Identifier<'ast>),
    PrimitiveExpression(Primitive<'ast>),
}

pub type Identifier<'ast> = &'ast str;
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Primitive<'ast> {
    Bool(bool),
    IntegerNumber(&'ast str),
    String(&'ast str),
    Char(&'ast char),
}

pub type ExpressionNode<'ast> = Node<'ast, Expression<'ast>>;
pub type ExpressionList<'ast> = NodeList<'ast, Expression<'ast>>;

impl_from! {
    Identifier => Expression::IdentifierExpression,
    Primitive => Expression::PrimitiveExpression,
}
