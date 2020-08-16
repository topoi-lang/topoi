pub use logos::{Logos, lookup};

#[derive(PartialEq, Debug, Copy, Clone, Logos)]
pub enum Token {
    #[token(";")]
    Semicolon,

    #[regex("[a-zA-Z_$][a-zA-Z0-9_$]*")]
    Identifier,

    #[regex("[0-9]+")]
    LiteralInteger,

    #[regex("\"([^\"\\\\]|\\\\.)*\"")]
    LiteralString,

    #[error]
    UnexpectedToken,
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_lex<T>(source: &str, tokens: T)
    where
        T: AsRef<[(Token, &'static str)]>
    {
        let mut lex = Token::lexer(source);

        for &(ref token, slice) in tokens.as_ref() {
            let lexed_token = lex.next();
            assert!(
                lexed_token == Some(*token) && lex.slice() == slice,
                "\n\n\n\tExpected {:?}({:?}), found {:?}({:?}) instead!\n\n\n",
                token, slice, lexed_token, lex.slice()
            )
        }
    }

    #[test]
    fn empty_input() {
        assert_lex(" ", [])
    }
}