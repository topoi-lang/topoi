extern crate bumpalo;

pub mod expression;
mod node;

use node::*;

use bumpalo::{collections as bump, Bump};

pub type Identifier<'a> = &'a str;
pub type StringLiteral = str;
pub type DocLiteral = str;

pub type IdentifierNode<'ast> = Node<Identifier<'ast>>;

pub struct Program<'a> {
    body: bump::Vec<'a, Identifier<'a>>,
    arena: Bump,
}

impl<'a> Program<'a> {
    pub fn new(body: bump::Vec<'a, Identifier<'a>>, arena: Bump) -> Self {
        Program { body, arena }
    }

    pub fn arena(&'a self) -> &'a Bump {
        &self.arena
    }
}
