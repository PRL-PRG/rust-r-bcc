use std::rc::Rc;

pub struct Loc {
    row: usize,
    col: usize,
}

#[derive(Debug, PartialEq)]
pub struct MetaData {
    attr: Option<Box<Sexp>>,
}

#[derive(Debug, PartialEq)]
pub struct Sexp {
    pub kind: SexpKind,
    metadata: MetaData,
}

impl From<SexpKind> for Sexp {
    fn from(kind: SexpKind) -> Self {
        Sexp {
            kind,
            metadata: MetaData { attr: None },
        }
    }
}

impl Sexp {
    pub fn set_attr(&mut self, attr: Sexp) {
        self.metadata.attr = Some(Box::new(attr))
    }
}

pub mod data {
    use std::rc::Rc;

    use super::{Sexp, SexpKind};

    #[derive(Debug, PartialEq)]
    pub struct TaggedSexp {
        // if this will be imutable then
        // Rc will be better for coping
        tag: Option<String>,
        data: Sexp,
    }

    impl TaggedSexp {
        pub fn new(data: Sexp) -> Self {
            Self { tag: None, data }
        }

        pub fn new_with_tag(data: Sexp, tag: String) -> Self {
            let tag = Some(tag);
            Self { tag, data }
        }
    }

    impl From<Sexp> for TaggedSexp {
        fn from(value: Sexp) -> Self {
            Self::new(value)
        }
    }

    impl From<SexpKind> for TaggedSexp {
        fn from(value: SexpKind) -> Self {
            Self::new(value.into())
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Complex {
        real: f64,
        imaginary: f64,
    }

    pub type List = Vec<TaggedSexp>;
}

pub mod lang {
    use std::rc::Rc;

    #[derive(Debug, PartialEq)]
    pub struct Sym {
        pub data: String,
    }

    impl Sym {
        pub fn new(data: String) -> Self {
            Self { data }
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Target {
        Lang(Box<Lang>), // expression
        Sym(Sym),        // named
    }

    #[derive(Debug, PartialEq)]
    pub struct Lang {
        target: Target,
        args: super::data::List,
    }

    impl Lang {
        pub fn new(target: Target, args: super::data::List) -> Self {
            Self { target, args }
        }
    }
}

// SXP
#[derive(Debug, PartialEq)]
pub enum SexpKind {
    Sym(lang::Sym),
    List(data::List),
    Nil,

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
    Str(Vec<String>),
    Vec(Vec<Sexp>),
}
