pub use logos::{Logos, lookup};

#[derive(PartialEq, Debug, Copy, Clone, Logos)]
pub enum Token {
    #[token(";")]
    Semicolon,

    #[regex("[a-zA-Z_$][a-zA-Z0-9_$]*")]
    Identifier,

    #[regex("[0-9]+")]
    LiteralInteger,

    // #[regex("\"([^\"\\\\]|\\\\.)*\"")]
    // LiteralString,

    #[error]
    UnexpectedToken,
}

#[cfg(test)]
mod test {
    use super::*;

}