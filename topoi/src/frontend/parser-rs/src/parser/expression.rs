use crate::ast::*;
use crate::parser::Parser;
use crate::ast::expression::ExpressionNode;
use crate::parser::nested::*;

use crate::lexer::{Token, Logos, lookup};

type HandlerFn = for<'ast> fn(&mut Parser<'ast>) -> Option<ExpressionNode<'ast>>;

static EXPRESSION_LUT: [HandlerFn; Token::SIZE] = lookup! {
    Token::KeywordThis         => |par| par.node_at_token(ThisExpression),
};

impl<'ast> Parser<'ast> {
    #[inline]
    pub fn expression(&mut self, precedence: Precedence) -> Option<ExpressionNode<'ast>> {
        let currentToken = self.currentToken.unwrap().token;
        EXPRESSION_LUT[currentToken as usize](self)
            .map(|expression| self.nested_expression(expression, precedence))
    }
}