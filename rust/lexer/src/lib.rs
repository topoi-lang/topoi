extern crate logos;
use logos::Logos;
use std::ops::Range;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    EndOfProgram,

    #[token(",")]
    Comma,
    #[token(".")]
    Accessor,

    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,

    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,

    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,

    #[regex("[a-zA-Z_$][a-zA-Z0-9_'$]*", |lex| lex.slice().to_owned())]
    Identifier(String),

    #[token("true")]
    LiteralTrue,

    #[token("false")]
    LiteralFalse,

    #[token("->")]
    LeftArrow,

    #[regex("[0-9]+")]
    LiteralInteger,

    #[regex("succ|pred|zero")]
    ReservedKeyword,

    #[token("if")]
    KwIf,
    #[token("then")]
    KwThen,
    #[token("else")]
    KwElse,

    #[token("\n")]
    Newline,

    #[regex("--[^\r\n]*(\r\n|\n)?", |lex| lex.slice().to_owned())]
    Comment(String),

    #[error]
    #[regex("[ ]", logos::skip)]
    UnexpectedToken,
}

#[derive(Debug, PartialEq)]
pub struct TopoiLexer<'source> {
    source: &'source str,

    /// Result tokens
    tokens: Vec<(Token, Range<usize>)>,

    errors: Vec<(Token, Range<usize>)>,
}

impl<'source> TopoiLexer<'source> {
    pub fn new(source: &'source str) -> Self {
        TopoiLexer {
            source,
            tokens: vec![],
            errors: vec![],
        }
    }

    pub fn lex(mut self) -> Self {
        self.tokens = Token::lexer(self.source)
            .spanned()
            .map(|token_with_span| match &token_with_span {
                (Token::UnexpectedToken, span) => {
                    self.errors.push((Token::UnexpectedToken, span.clone()));
                    token_with_span
                }
                _ => token_with_span,
            })
            .collect();
        self
    }

    pub fn tokens(self) -> Vec<(Token, Range<usize>)> {
        self.tokens
    }
}

#[cfg(test)]
mod lexer {
    use super::*;
    use logos::Logos;

    #[test]
    fn empty_input() {
        let input = "";
        let tokens: Vec<_> = Token::lexer(input).spanned().collect();
        assert_eq!(tokens, &[])
    }

    #[test]
    fn ident_lex() {
        let input = "succ 1 apple";
        let mut lex = Token::lexer(input);

        assert_eq!(lex.next(), Some(Token::ReservedKeyword));
        assert_eq!(lex.slice(), "succ");

        assert_eq!(lex.next(), Some(Token::LiteralInteger));
        assert_eq!(lex.slice(), "1");

        assert_eq!(lex.next(), Some(Token::Identifier("apple".to_string())));
        assert_eq!(lex.slice(), "apple");
    }

    #[test]
    fn comment_lex() {
        let input = "succ\n-- aaaaa";
        let mut lex = Token::lexer(input);

        assert_eq!(lex.next(), Some(Token::ReservedKeyword));
        assert_eq!(lex.slice(), "succ");
        assert_eq!(lex.next(), Some(Token::Newline));
        assert_eq!(lex.next(), Some(Token::Comment("-- aaaaa".to_owned())));
        assert_eq!(lex.next(), None);
    }

    #[test]
    fn lex_topoi() {
        let input = "succ 1 \\";
        let topoi_lexer = TopoiLexer::new(input).lex();
        assert_eq!(
            topoi_lexer,
            TopoiLexer {
                source: input,
                tokens: vec![
                    (Token::ReservedKeyword, 0..4),
                    (Token::LiteralInteger, 5..6),
                    (Token::UnexpectedToken, 7..8),
                ],
                errors: vec![(Token::UnexpectedToken, 7..8)],
            }
        );
    }
}
