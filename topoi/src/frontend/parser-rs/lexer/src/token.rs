pub use logos::Logos;

#[derive(PartialEq, Debug, Copy, Clone, Logos)]
pub enum Token {
    #[token(";")]
    Semicolon,

    #[token("=")]
    EqualSign,

    #[token("\n")]
    Newline,

    #[token("data")]
    DataKeyword,

    #[regex("[a-zA-Z_$][a-zA-Z0-9_$]*")]
    Identifier,

    #[regex("[0-9]+")]
    LiteralInteger,

    // #[regex("\"([^\"\\\\]|\\\\.)*\"")]
    // LiteralString,

    #[token("module")]
    ModuleKeyword,

    #[regex(r"[ \f\t\r]", logos::skip)]
    #[error]
    UnexpectedToken,
}
