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
    use std::collections::HashMap;

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

    impl Environment {
        pub fn find_local_var(&self, name: &str) -> Option<&super::Sexp> {
            match self {
                Environment::Global | Environment::Base | Environment::Empty => None,
                Environment::Normal(env) => env.find_local_var(name),
            }
        }
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

        pub fn find_local_var(&self, name: &str) -> Option<&super::Sexp> {
            match self.frame.get(name) {
                Some(res) => Some(res),
                None => match self.hash_frame.get(name) {
                    Some(res) => Some(res),
                    None => self.parent.find_local_var(name),
                },
            }
        }

        pub fn get(&self, name: &str) -> Option<&super::Sexp> {
            todo!()
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

    #[derive(PartialEq, Clone, Default)]
    pub struct ListFrame {
        pub data: Option<super::data::List>,
        pub env: HashMap<String, usize>,
    }

    impl std::fmt::Debug for ListFrame {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.data)
        }
    }

    impl ListFrame {
        pub fn new(data: super::data::List) -> Self {
            let mut env = HashMap::default();
            for (index, item) in data.iter().enumerate() {
                let Some(name) = item.tag.clone() else {
                    unreachable!()
                };
                env.insert(name, index);
            }
            Self {
                data: Some(data),
                env,
            }
        }

        pub fn get(&self, name: &str) -> Option<&super::Sexp> {
            let index = self.env.get(name)?.clone();
            match &self.data {
                Some(data) => Some(&data[index].data),
                None => None,
            }
        }
    }

    #[derive(PartialEq, Clone, Default)]
    pub struct HashFrame {
        pub data: Option<Vec<super::Sexp>>,
        pub env: HashMap<String, (usize, usize)>,
    }

    impl std::fmt::Debug for HashFrame {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.data)
        }
    }

    impl HashFrame {
        pub fn new(data: Vec<super::Sexp>) -> Self {
            let mut env = HashMap::new();

            for (block, item) in data.iter().enumerate() {
                match &item.kind {
                    super::SexpKind::List(list) => {
                        for (idx, item) in list.into_iter().enumerate() {
                            let Some(name) = item.tag.clone() else {
                                unreachable!()
                            };
                            env.insert(name, (block, idx));
                        }
                    }
                    super::SexpKind::Nil => (),
                    _ => unreachable!(),
                }
            }

            Self {
                data: Some(data),
                env,
            }
        }

        pub fn get(&self, name: &str) -> Option<&super::Sexp> {
            let (block, idx) = self.env.get(name)?.clone();

            match &self.data {
                Some(data) => match &data[block].kind {
                    super::SexpKind::List(list) => Some(&list[idx].data),
                    _ if idx == 0 => Some(&data[block]),
                    _ => None,
                },
                None => None,
            }
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
    Buildin(lang::Sym),

    // vecs
    Char(Vec<char>),
    Logic(Vec<bool>),
    Real(Vec<f64>),
    Int(Vec<i32>),
    Complex(Vec<data::Complex>),
    Str(Vec<String>),
    Vec(Vec<Sexp>),

    MissingArg,

    BaseNamespace, // as in GnuR fake namespace
}
