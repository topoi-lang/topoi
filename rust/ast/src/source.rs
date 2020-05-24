use super::*;

/// SourceUnit is the compilation unit, aka a source file.
/// It also served as a module, every piece of code is wrapped into a module.
/// And a module itself can be a submodule of another module, as long as it
/// reexport.
pub enum SourceUnit<'ast> {
    ModuleDirective(ModuleDefinition<'ast>),
    // ImportDirective(ImportDirective<'ast>),
    SourceBody(SourceBodyList<'ast>),
}

/// `module A.B.C`
pub struct ModuleDefinition<'ast> {
    pub name: IdentifierNode<'ast>,
}

// pub struct Import<'ast> {
//     pub symbol: IdentifierNode<'ast>,
//     pub alias: Option<IdentifierNode<'ast>>,
// }

pub enum SourceBodyPart<'ast> {
    ValueDefinition(ValueDefinition<'ast>),
    DataDefinition(DataDefinition<'ast>),
}

pub type SourceBodyPartList<'ast> = NodeList<'ast, SourceBodyPart>;
