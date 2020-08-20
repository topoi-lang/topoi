extern crate toolshed;
extern crate topoi_lexer as lexer;
extern crate topoi_ast as ast;

use toolshed::Arena;

use ast::node::*;
use lexer::{Token, Lexer};

mod precedence;

pub struct Parser<'ast> {
    arena: &'ast Arena,
    lexer: Lexer<'ast, Token>,
    body: Vec<String>,
}

impl<'ast> Parser <'ast> {
    pub fn new(source: &str, arena: &'ast Arena) -> Self {
        let source = arena.alloc_str(source);

        Parser {
            arena,
            lexer: Lexer::new(source),
            body: Vec::new(),
        }
    }

    fn next(&mut self) -> Token {
        self.lexer.next().unwrap_or(Token::UnexpectedToken)
    }

    // This will allocate a new arena in the node.
    fn alloc<T>(&mut self, val: NodeInner<T>) -> Node<'ast, T>
    where
        T: Copy,
    {
        Node::new(self.arena.alloc(val))
    }

    fn node_at<T, I, R>(&mut self, start: u32, end: u32, item: I) -> R
    where
        T: 'ast + Copy,
        I: Into<T>,
        R: From<Node<'ast, T>>,
    {
        From::from(self.alloc(NodeInner::new(start, end, item.into())))
    }

    fn node_at_token<T, I, R>(&mut self, item: I) -> R
    where
        T: 'ast + Copy,
        I: Into<T>,
        R: From<Node<'ast, T>>,
    {
        let span = self.lexer.span();
        self.node_at(span.start as u32, span.end as u32, item)
    }

    fn node_from_slice<T, F, I, R>(&mut self, func: F) -> R
    where
        T: 'ast + Copy,
        F: FnOnce(&'ast str) -> I,
        I: Into<T>,
        R: From<Node<'ast, T>>,
    {
        let slice = self.lexer.slice();
        let span = self.lexer.span();

        self.node_at(span.start as u32, span.end as u32, func(slice))
    }
}

#[cfg(test)]
mod parser {
    use super::*;

    #[test]
    fn parser_next() {
        let source = "aaa = 1;";
        let arena = Arena::new();
        let mut parser = Parser::new(source, &arena);

        assert_eq!(parser.next(), Token::Identifier);
        assert_eq!(parser.next(), Token::EqualSign);
        assert_eq!(parser.next(), Token::LiteralInteger);
        assert_eq!(parser.next(), Token::Semicolon);
        assert_eq!(parser.next(), Token::UnexpectedToken);
    }
}