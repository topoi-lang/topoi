// use toolshed::list::ListBuilder;
use Parser; 
use ast::*;
use lexer::{Token, Logos, lookup};

type HandlerFn = for<'ast> fn(&mut Parser<'ast>) -> Option<ExpressionNode<'ast>>;

static EXPRESSION_LUT: [HandlerFn; Token::SIZE] = lookup! {
    Token::Identifier => (|par| par.node_from_slice(|ident| ident)) as HandlerFn,
    Token::LiteralInteger => (|par| par.integer_number()) as HandlerFn,
    Token::LiteralString => (|par| par.node_from_slice(|slice| Primitive::Str(slice))) as HandlerFn,
    _ => (|_| None) as HandlerFn,
};

impl<'ast> Parser<'ast> {
    fn integer_number(&mut self) -> Option<ExpressionNode<'ast>> {
        let number = self.lexer.slice();
        let span = self.lexer.span();

        self.node_at(span.start as u32, span.end as u32, Primitive::IntegerNumber(number))
    }
}
