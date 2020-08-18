use toolshed::{Arena, list::GrowableList};
use crate::ast::node::{Node};
use std::ops::Range;
use std::iter::Peekable;

// mod expression;
// mod nested;
// mod statement;
mod source_unit;

use logos::{SpannedIter, Span, Lexer};
use crate::lexer::{Token};

#[derive(Clone)]
struct TokenSpan {
    token: Token,
    slice: Box<str>,
    span: Span,
}

pub struct Parser<'ast> {
    arena: &'ast Arena,
    lexer: Lexer<'ast, Token>,
    current_token: Option<TokenSpan>,
    errors: Vec<TokenSpan>,
}

impl<'ast> Parser<'ast> {
    pub fn new(source: &str, arena: &'ast Arena) -> Self {
        let source = arena.alloc_str(source);

        Parser {
            arena,
            lexer: logos::Lexer::new(source),
            current_token: None,
            errors: vec![],
        }
    }

    fn advance(&mut self) {
        match self.lexer.next() {
            Some(token) => {
                self.current_token = Some(TokenSpan{
                    token,
                    span: self.lexer.span(),
                    slice: self.lexer.slice().into(),
                })
            },
            None => unimplemented!("advance to the unknown horizon."),
        }
    }

    fn allow(&mut self, token: Token) -> bool {
        let current_token = self.current_token.as_ref().unwrap().token;
        if current_token == token {
            self.advance();
            true
        } else {
            false
        }
    }

    fn push_error(&mut self) {
        let current_token = self.current_token.as_ref().unwrap();
        self.errors.push(current_token.clone())
    }

    fn mark_start_span(&mut self) -> u32 {
        let start = self.lexer.span().start as u32;
        self.advance();
        start
    }

    fn mark_end_span(&mut self) -> u32 {
        let end = self.lexer.span().end as u32;
        self.advance();
        end
    }
}
