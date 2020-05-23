use std::cell::Cell;
use std::ops::Range;

pub struct Node<T> {
    inner: Cell<NodeInner<T>>,
}

pub struct NodeInner<T> {
    pub span: Range<u32>,
    pub value: T,
}

impl<T> NodeInner<T> {
    pub fn new(start: u32, end: u32, value: T) -> Self {
        NodeInner {
            span: (start..end),
            value,
        }
    }
}

impl<'ast, T: 'ast> Node<T> {
    pub fn new(ptr: NodeInner<T>) -> Self {
        Node {
            inner: Cell::new(ptr),
        }
    }

    pub fn set(&self, ptr: NodeInner<T>) {
        self.inner.set(ptr)
    }

    pub fn get_mut(&mut self) -> &mut NodeInner<T> {
        self.inner.get_mut()
    }
}
