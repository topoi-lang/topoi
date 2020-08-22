pub use logos::Logos;

#[derive(PartialEq, Debug, Copy, Clone, Logos)]
pub enum Token {
    #[token(";")]
    Semicolon,

    #[token(".")]
    Accessor,

    #[token("=")]
    Assign,

    #[token("\n")]
    Newline,

    #[token("data")]
    DataKeyword,

    #[regex("[a-zA-Z_$][a-zA-Z0-9_$]*")]
    Identifier,

    #[regex("[0-9]+")]
    LiteralInteger,

    #[regex("\"([^\"\\\\]|\\\\.)*\"")]
    LiteralString,

    #[token("module")]
    ModuleKeyword,

    #[token("where")]
    WhereKeyword,

    #[regex(r"[ \f\t\r]", logos::skip)]
    #[error]
    UnexpectedToken,
}
