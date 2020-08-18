use crate::parser::Parser;
use crate::lexer::Token;
use crate::ast::source_unit::*;

impl<'ast> Parser<'ast> {

    // Start from this function
    fn source_body(&mut self) -> Option<SourceBody<'ast>> {
        let current_token = self.current_token.as_ref().unwrap().token;
        match current_token {
            Token::DataKeyword => unimplemented!(),
            Token::Identifier => unimplemented!(),
            _ => unimplemented!(),
        }
    }
}
