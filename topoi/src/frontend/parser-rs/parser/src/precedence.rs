use Parser;
use ast::ExpressionNode;
use lexer::{Token, Logos, lookup};

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Precedence(u8);

type HandlerFn = for<'ast> fn(&mut Parser<'ast>, ExpressionNode<'ast>) -> Option<ExpressionNode<'ast>>;

#[derive(Clone, Copy)]
struct NestedHandler(Precedence, HandlerFn);

const INVALID: Precedence = Precedence(100);

pub const TOP: Precedence = Precedence(15);
pub const P14: Precedence = Precedence(14);
pub const P13: Precedence = Precedence(13);
pub const P12: Precedence = Precedence(12);
pub const P11: Precedence = Precedence(11);
pub const P10: Precedence = Precedence(10);
pub const P9: Precedence = Precedence(9);
pub const P8: Precedence = Precedence(8);
pub const P7: Precedence = Precedence(7);
pub const P6: Precedence = Precedence(6);
pub const P5: Precedence = Precedence(5);
pub const P4: Precedence = Precedence(4);
pub const P3: Precedence = Precedence(3);
pub const P2: Precedence = Precedence(2);

// [TODO]
static PRECEDENCE_LUT: [NestedHandler; Token::SIZE] = lookup! {
    Token::Assign => NestedHandler(INVALID, |_, _| None),
    _ => NestedHandler(INVALID, |_, _| None),
};
