use std::rc::Rc;

pub struct Loc {
    row: usize,
    col: usize,
}

pub struct MetaData {
    attr: Box<Sexp>,
    loc: Loc,
}

pub struct Sexp {
    kind: SexpKind,
    metadata: MetaData,
}

pub mod data {
    use std::rc::Rc;

    use super::Sexp;

    pub struct TaggedSexp {
        // if this will be imutable then
        // Rc will be better for coping
        tag: Rc<str>,
        data: Sexp,
    }

    pub struct Complex {
        real: f64,
        imaginary: f64,
    }

    pub type List = Vec<TaggedSexp>;
}

pub mod lang {
    use std::rc::Rc;

    pub struct Sym {
        data: Rc<str>,
    }

    pub enum Target {}

    pub struct Lang {
        target: Target,
        args: super::data::List,
    }
}

// SXP
pub enum SexpKind {
    Sym(lang::Sym),
    List(data::List),

    // language contructs
    Closure,
    Environment,
    Promise,
    Lang(lang::Lang),

    // vecs
    Char(Vec<char>),
    Logic(Vec<bool>),
    Real(Vec<f64>),
    Int(Vec<i32>),
    Complex(Vec<data::Complex>),
    Str(Vec<Rc<str>>),
}
