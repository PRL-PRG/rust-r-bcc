use super::bc::Bc;

pub struct Loc {
    row: usize,
    col: usize,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct MetaData {
    pub attr: Option<Box<Sexp>>,
}

impl MetaData {
    pub fn is_obj(&self) -> bool {
        if self.attr.is_some() {
            let attr = self.attr.clone().unwrap();
            if let Sexp {
                kind: SexpKind::List(list),
                ..
            } = *attr
            {
                list.into_iter().any(|x| {
                    let x = match x.tag {
                        Some(x) if x.as_str() == "class" => true,
                        _ => false,
                    };
                    x
                })
            } else {
                false
            }
        } else {
            false
        }
    }
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
        pub real: f64,
        pub imaginary: f64,
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

    impl From<Sym> for Target {
        fn from(value: Sym) -> Self {
            Target::Sym(value)
        }
    }

    impl From<Lang> for Target {
        fn from(value: Lang) -> Self {
            Target::Lang(Box::new(value))
        }
    }

    impl Into<super::Sexp> for Target {
        fn into(self) -> super::Sexp {
            match self {
                Target::Lang(x) => super::SexpKind::Lang(*x).into(),
                Target::Sym(x) => super::SexpKind::Sym(x).into(),
            }
        }
    }

    impl Into<super::Sexp> for &Target {
        fn into(self) -> super::Sexp {
            match self {
                Target::Lang(x) => super::SexpKind::Lang(*x.clone()).into(),
                Target::Sym(x) => super::SexpKind::Sym(x.clone()).into(),
            }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Lang {
        pub(crate) target: Target,
        pub(crate) args: super::data::List,
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
        pub name: Sym,
        pub value: Box<super::Sexp>,
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
        pub(crate) formals: Vec<Formal>,
        pub(crate) body: Box<super::Sexp>,
        pub(crate) environment: Environment,
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

    impl Into<super::Sexp> for Closure {
        fn into(self) -> super::Sexp {
            super::SexpKind::Closure(self).into()
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
        pub parent: Box<Environment>,
        pub locked: bool,
        pub frame: ListFrame,
        pub hash_frame: HashFrame,
    }

    impl NormalEnv {
        pub fn new(
            parent: Box<Environment>,
            locked: bool,
            frame: ListFrame,
            hash_frame: HashFrame,
        ) -> Self {
            Self {
                parent,
                locked,
                frame,
                hash_frame,
            }
        }
    }

    impl Into<Environment> for NormalEnv {
        fn into(self) -> Environment {
            Environment::Normal(self)
        }
    }

    impl Into<super::Sexp> for Environment {
        fn into(self) -> super::Sexp {
            super::SexpKind::Environment(self).into()
        }
    }

    #[derive(Debug, PartialEq, Clone, Default)]
    pub struct ListFrame {
        pub data: Option<super::data::List>,
    }

    impl ListFrame {
        pub fn new(data: super::data::List) -> Self {
            Self { data: Some(data) }
        }
    }

    #[derive(Debug, PartialEq, Clone, Default)]
    pub struct HashFrame {
        pub data: Option<Vec<super::Sexp>>,
    }

    impl HashFrame {
        pub fn new(data: Vec<super::Sexp>) -> Self {
            Self { data: Some(data) }
        }
    }

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
