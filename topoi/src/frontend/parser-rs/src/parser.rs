use toolshed::{Arena};

use crate::lexer::{Token};

pub struct Parser<'ast> {
    arena: Arena,
    lexer: logos::Lexer<'ast, Token>,
    errors: Vec<String>,
}

impl<'ast> Parser<'ast> {
    pub fn new(source: &str, arena: Arena) -> Self {
        let source = arena.alloc_nul_term_str(val: &str)
    }
}