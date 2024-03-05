use super::bc::Bc;

pub struct Loc {
    row: usize,
    col: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MetaData {
    pub attr: Option<Box<Sexp>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Sexp {
    pub kind: SexpKind,
    pub metadata: MetaData,
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
    use super::{Sexp, SexpKind};

    #[derive(Debug, PartialEq, Clone)]
    pub struct TaggedSexp {
        pub tag: Option<String>,
        pub data: Sexp,
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

    #[derive(Debug, PartialEq, Clone)]
    pub struct Complex {
        real: f64,
        imaginary: f64,
    }

    pub type List = Vec<TaggedSexp>;
}

pub mod lang {

    #[derive(Debug, PartialEq, Clone)]
    pub struct Sym {
        pub(crate) data: String,
    }

    impl Sym {
        pub fn new(data: String) -> Self {
            Self { data }
        }
    }

    impl From<&str> for Sym {
        fn from(value: &str) -> Self {
            Sym {
                data: value.to_string(),
            }
        }
    }

    impl Into<super::Sexp> for Sym {
        fn into(self) -> super::Sexp {
            super::SexpKind::Sym(self).into()
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Target {
        Lang(Box<Lang>), // expression
        Sym(Sym),        // named
    }

    impl Into<super::Sexp> for Target {
        fn into(self) -> super::Sexp {
            match self {
                Target::Lang(x) => super::SexpKind::Lang(*x).into(),
                Target::Sym(x) => super::SexpKind::Sym(x).into(),
            }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Lang {
        pub(in crate::sexp) target: Target,
        pub(in crate::sexp) args: super::data::List,
    }

    impl Into<super::Sexp> for Lang {
        fn into(self) -> super::Sexp {
            super::SexpKind::Lang(self).into()
        }
    }

    impl Lang {
        pub fn new(target: Target, args: super::data::List) -> Self {
            Self { target, args }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Formal {
        name: Sym,
        value: Box<super::Sexp>,
    }

    impl Formal {
        pub fn new(name: Sym, value: super::Sexp) -> Self {
            Self {
                name,
                value: Box::new(value),
            }
        }
    }

    impl TryInto<Formal> for super::data::TaggedSexp {
        type Error = crate::rds::rds_reader::RDSReaderError;

        fn try_into(self) -> Result<Formal, Self::Error> {
            match self.tag {
                Some(name) => Ok(Formal::new(Sym::new(name), self.data)),
                None => Err(crate::rds::rds_reader::RDSReaderError::DataError(
                    "Formal must have name as a tag".to_string(),
                )),
            }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Closure {
        formals: Vec<Formal>,
        pub(crate) body: Box<super::Sexp>,
        environment: Environment,
    }

    impl Closure {
        pub fn new(formals: Vec<Formal>, body: super::Sexp, environment: Environment) -> Self {
            Self {
                formals,
                body: Box::new(body),
                environment,
            }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Environment {
        Global,
        Base,
        Empty,
        Normal(NormalEnv),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct NormalEnv {
        parent: Box<Environment>,
        locked: bool,
        frame: Frame,
        hash_frame: Frame,
    }

    impl NormalEnv {
        pub fn new(parent: Box<Environment>, locked: bool) -> Self {
            Self {
                parent,
                locked,
                frame: Frame,
                hash_frame: Frame,
            }
        }
    }

    impl Into<Environment> for NormalEnv {
        fn into(self) -> Environment {
            Environment::Normal(self)
        }
    }

    // TODO
    #[derive(Debug, PartialEq, Clone)]
    struct Frame;

    impl Into<super::SexpKind> for Environment {
        fn into(self) -> super::SexpKind {
            super::SexpKind::Environment(self)
        }
    }
}

// SXP
#[derive(Debug, PartialEq, Clone)]
pub enum SexpKind {
    Sym(lang::Sym),
    List(data::List),
    Nil,

    // language contructs
    Closure(lang::Closure),
    Environment(lang::Environment),
    Promise,
    Lang(lang::Lang),
    Bc(Bc),

    // vecs
    Char(Vec<char>),
    Logic(Vec<bool>),
    Real(Vec<f64>),
    Int(Vec<i32>),
    Complex(Vec<data::Complex>),
    Str(Vec<String>),
    Vec(Vec<Sexp>),

    MissingArg,
}
