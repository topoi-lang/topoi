extern crate logos;
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
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

    #[regex("[a-zA-Z_$][a-zA-Z0-9_'$]*")]
    Identifier,

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

    #[regex("if")]
    KwIf,
    #[regex("then")]
    KwThen,
    #[regex("else")]
    KwElse,

    #[error]
    #[regex(r"[ ]", logos::skip)] // skip whitespace
    UnexpectedToken,
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
        let input = "succ 1";
        let mut lex = Token::lexer(input);

        assert_eq!(lex.next(), Some(Token::ReservedKeyword));
        assert_eq!(lex.slice(), "succ");

        assert_eq!(lex.next(), Some(Token::LiteralInteger));
        assert_eq!(lex.slice(), "1");
    }
}
