use {*};

/// <bindingNode> where List<expressionNode>
#[derive(Copy, Clone, Debug)]
pub struct WhereBinding<'ast, T> {
    pub binding_node: &'ast T,
    pub expression: Option<ExpressionList<'ast>>,
}

/// let <expressionNode> in <bindingNode>
#[derive(Copy, Clone, Debug)]
pub struct LetInBinding<'ast, T> {
    pub expression: ExpressionNode<'ast>,
    pub binding_node: &'ast T,
}
