extern crate bumpalo;
extern crate logos;
extern crate topoi_ast as ast;
extern crate topoi_lexer as lexer;

use std::ops::Range;

type TokenSpan = (lexer::Token, Range<usize>);

pub struct TopoiParser {
    tokens: Vec<TokenSpan>,
}

impl TopoiParser {
    pub fn new(tokens: Vec<TokenSpan>) -> Self {
        TopoiParser { tokens }
    }

    pub fn tokens(self) -> Vec<TokenSpan> {
        self.tokens
    }
}
