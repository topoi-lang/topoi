use toolshed::Arena;
use toolshed::list::UnsafeList;
use std::marker::PhantomData;

use ast::*;

pub struct Program<'ast> {
    body: UnsafeList,
    arena: Arena,

    /// Lifetime safety, a place holder for lifetime `'ast`
    _phantom: PhantomData<SourceUnitList<'ast>>
}

impl<'ast> Program<'ast> {
    pub fn new(body: UnsafeList, arena: Arena) -> Self {
        Program {
            body,
            arena,
            _phantom: PhantomData,
        }
    }

    pub fn body(&self) -> SourceUnitList<'ast> {
        unsafe { self.body.into_list() }
    }

    pub fn arena(&'ast self) -> &'ast Arena {
        &self.arena
    }
}
