extern crate toolshed;

use toolshed::list::List;

mod impl_from;
mod node;
pub use node::*;

pub type NodeList<'ast, T> = List<'ast, Node<'ast, T>>;
pub type Identifier<'ast> = &'ast str;
pub type IdentifierNode<'ast> = Node<'ast, Identifier<'ast>>;

mod source_unit;
pub use source_unit::*;

pub type SourceUnitNode<'ast> = Node<'ast, SourceUnit<'ast>>;
pub type SourceUnitList<'ast> = NodeList<'ast, SourceUnit<'ast>>;

mod expression;
pub use expression::*;