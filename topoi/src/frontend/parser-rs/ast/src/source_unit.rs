use {*};

/// `SourceUnit` is the module of topoi code. Every code in topoi is wrapped in
/// module.
#[derive(Copy, Clone, Debug)]
pub enum SourceUnit<'ast> {
    ModuleDirective(ModuleDirective<'ast>),
    // pub imports: ImportDirective<'ast>,
    // pub body: SourceUnitPartList<'ast>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ModuleDirective<'ast> {
    pub module_name: &'ast str,
}

#[derive(Copy, Clone, Debug)]
pub enum SourceUnitPart<'ast> {
    FunctionDefinition(FunctionDefinition<'ast>),
}

#[derive(Copy, Clone, Debug)]
pub struct FunctionDefinition<'ast> {
    pub name: IdentifierNode<'ast>,
    pub params: IdentifierNode<'ast>,
    pub body: ExpressionNode<'ast>,
}

pub type SourceUnitPartList<'ast> = NodeList<'ast, SourceUnitPart<'ast>>;

impl_from! {
    ModuleDirective => SourceUnit::ModuleDirective,
    // FunctionDefinition => SourceUnitPart::FunctionDefinition,
}
