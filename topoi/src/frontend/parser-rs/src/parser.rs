use toolshed::{Arena};

use crate::lexer::{Token};

pub struct Parser<'ast> {
    arena: &'ast Arena,
    lexer: logos::Lexer<'ast, Token>,
    errors: Vec<String>,
}

impl<'ast> Parser<'ast> {
    pub fn new(source: &str, arena: &'ast Arena) -> Self {
        let source = arena.alloc_str(source);

        Parser {
            arena,
            lexer: logos::Lexer::new(source),
            errors: Vec::new()
        }
    }
}