//! Vel concrete syntax tree.

/// Contents of a whole file.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Whole<'a> {
    /// Top-level items.
    pub top_levels: Vec<TopLevel<'a>>,
}

/// Top-level item.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TopLevel<'a> {
    /// Function definition.
    FnDef(FnDef<'a>),
}

pub use TopLevel::*;

/// Function definition.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FnDef<'a> {
    /// Function name.
    pub name: &'a str,
    /// Static inputs.
    pub stat_ins: Vec<Entry<'a>>,
    /// Dynamic inputs.
    pub dyn_ins: Vec<Entry<'a>>,
    /// Outputs.
    pub outs: Vec<Entry<'a>>,
    /// Body.
    pub body: Stmt,
}

/// Entry.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Entry<'a> {
    /// Name of the argument.
    pub name: &'a str,
    /// Type of the argument.
    pub ty: Ty<'a>,
}

/// Type.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ty<'a> {
    /// Name.
    NameTy(&'a str),
}
pub use Ty::*;

/// Statement.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Stmt {
    /// Empty
    Empty,
}
