module Topoi.Parser.Tokenizer where

data Tok
    = TokNewline
    | TokWhitespace
    | TokEOF
    | TokComment Text