extern crate toolshed;

use toolshed::list::List;

pub mod node;
use node::*;

pub type NodeList<'ast, T> = List<'ast, Node<'ast, T>>;
pub type Identifier<'ast> = &'ast str;
pub type IdentifierNode<'ast> = Node<'ast, Identifier<'ast>>;

mod impl_from;
pub mod source_unit;
pub mod expression;