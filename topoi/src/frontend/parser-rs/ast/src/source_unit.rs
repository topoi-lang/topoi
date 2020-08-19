
/// `SourceUnit` is the module of topoi code. Every code in topoi is wrapped in
/// module.
pub enum SourceUnit<'ast> {
    ModuleDirective(ModuleDirective<'ast>),
}

pub struct ModuleDirective<'ast> {
    pub module_name: &'ast str,
}

// #[derive(Clone, Copy, Debug)]
// pub enum SourceUnit<'ast> {
//     ModuleDirective(ModuleDirective<'ast>),
//     SourceBody(SourceBody<'ast>),
// }

// #[derive(Clone, Copy, Debug)]
// pub struct ModuleDirective<'ast> {
//     pub module_name: &'ast str,
// }

// #[derive(Clone, Copy, Debug)]
// pub enum SourceBody<'ast> {
//     DataTypeDefinition(DataTypeDefinition<'ast>),
//     ValueDefinition(ValueDefinition<'ast>),
// }

// #[derive(Clone, Copy, Debug)]
// pub struct DataTypeDefinition<'ast> {
//     pub data_name: IdentifierNode<'ast>,
// }

// #[derive(Clone, Copy, Debug)]
// pub struct ValueDefinition<'ast> {
//     pub name: IdentifierNode<'ast>,
//     pub expression: IdentifierNode<'ast>,
// }
