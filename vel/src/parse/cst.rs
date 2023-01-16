//! Vel concrete syntax tree.

/// Contents of a whole file.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Whole {
    /// Top-level items.
    pub top_levels: Vec<TopLevel>,
}

/// Top-level item.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TopLevel {
    /// Function definition.
    FnDef(FnDef),
}
pub use TopLevel::*;

/// Function definition.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FnDef {
    /// Function name.
    pub name: String,
    /// Static inputs.
    pub stat_ins: Vec<Entry>,
    /// Dynamic inputs.
    pub dyn_ins: Vec<Entry>,
    /// Outputs.
    pub outs: Vec<Entry>,
    /// Body.
    pub body: Stmt,
}

/// Entry.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Entry {
    /// Name of the argument.
    pub name: String,
    /// Type of the argument.
    pub ty: Ty,
}

/// Type.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ty {
    /// Name.
    NameTy(String),
}
pub use Ty::*;

/// Statement.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Stmt {
    /// Empty
    Empty,
}
