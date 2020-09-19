use ast::*;
use Parser;
use lexer::Token;

impl<'ast> Parser<'ast> {
    pub fn source_unit(&mut self) -> Option<SourceUnitNode<'ast>> {
        match self.lexer.next().unwrap_or(Token::UnexpectedToken) {
            Token::ModuleKeyword => self.module_directive(),
            _ => None, //self.source_unit_body(),
        }
    }

    pub fn module_directive(&mut self) -> Option<SourceUnitNode<'ast>> {
        let start = self.mark_start();

        let next_token = self.next();
        if next_token != Token::Identifier {
            eprint!("error occured after the keyword module");
        }

        let module_name = self.lexer.slice();
        let end = self.mark_end();

        self.node_at(start, end, ModuleDirective { module_name })
    }

    // TODO: Import directive
}
