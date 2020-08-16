use toolshed::{Arena};
use crate::ast::node::{Node};
use std::ops::Range;
use std::iter::Peekable;

mod expression;
mod nested;

use logos::{SpannedIter, Span};
use crate::lexer::{Token};

struct TokenSpan {
    token: Token,
    span: Span,
}

pub struct Parser<'ast> {
    arena: &'ast Arena,
    lexerIter: Peekable<SpannedIter<'ast, Token>>,
    currentToken: Option<TokenSpan>,
    errors: Vec<TokenSpan>,
}

impl<'ast> Parser<'ast> {
    pub fn new(source: &str, arena: &'ast Arena) -> Self {
        let source = arena.alloc_str(source);

        Parser {
            arena,
            lexerIter: logos::Lexer::new(source).spanned().peekable(),
            currentToken: None,
            errors: vec![],
        }
    }

    fn advance(&mut self) {
        match self.lexerIter.peek() {
            Some(&(token, span)) => {
                self.currentToken = Some(TokenSpan { token, span });
            },
            None => unimplemented!("advance to the unknown horizon."),
        }
    }

    fn allow(&mut self, token: Token) -> bool {
        if self.currentToken.unwrap().token == token {
            self.advance();
            true
        } else {
            false
        }
    }

    fn push_error(&mut self) {
        let currentToken = self.currentToken.unwrap();
        self.errors.push(currentToken)
    }

}