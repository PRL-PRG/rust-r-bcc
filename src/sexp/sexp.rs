use super::bc::Bc;

pub struct Loc {
    row: usize,
    col: usize,
}

#[derive(Debug, PartialEq, Default)]
pub struct MetaData<'a> {
    pub attr: Option<&'a mut Sexp<'a>>,
}

impl MetaData<'_> {
    pub fn is_obj(&self) -> bool {
        if self.attr.is_some() {
            let attr = self.attr.unwrap();
            if let Sexp {
                kind: SexpKind::List(list),
                ..
            } = *attr
            {
                list.into_iter().any(|x| {
                    let x = match x.tag {
                        Some(x) if x == "class" => true,
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

#[derive(Debug, PartialEq)]
pub struct Sexp<'a> {
    pub kind: SexpKind<'a>,
    pub metadata: MetaData<'a>,
}

impl<'a> From<SexpKind<'a>> for Sexp<'a> {
    fn from(kind: SexpKind) -> Self {
        Sexp {
            kind,
            metadata: MetaData { attr: None },
        }
    }
}

impl Sexp<'_> {
    pub fn set_attr(&mut self, attr: &mut Sexp) {
        self.metadata.attr = Some(attr);
    }
}

pub mod data {
    use std::ops::Deref;

    use super::{Sexp, SexpKind};

    #[derive(Debug, PartialEq)]
    pub struct TaggedSexp<'a> {
        pub tag: Option<&'a str>,
        pub data: Sexp<'a>,
    }

    impl TaggedSexp<'_> {
        pub fn new(data: Sexp) -> Self {
            Self { tag: None, data }
        }

        pub fn new_with_tag<'a>(data: Sexp, tag: &'a str) -> Self {
            let tag = Some(tag);
            Self { tag, data }
        }
    }

    impl<'a> From<Sexp<'a>> for TaggedSexp<'a> {
        fn from(value: Sexp) -> Self {
            Self::new(value)
        }
    }

    impl<'a> From<SexpKind<'a>> for TaggedSexp<'a> {
        fn from(value: SexpKind) -> Self {
            Self::new(value.into())
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Complex {
        pub real: f64,
        pub imaginary: f64,
    }

    #[derive(PartialEq)]
    pub struct List<'a> {
        data: &'a [TaggedSexp<'a>],
    }

    impl std::fmt::Debug for List<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.data)
        }
    }

    impl<'a> Deref for List<'a> {
        type Target = [TaggedSexp<'a>];

        fn deref(&self) -> &Self::Target {
            &self.data
        }
    }

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum Logic {
        True,
        False,
        NA,
    }

    impl Into<i32> for &Logic {
        fn into(self) -> i32 {
            match self {
                Logic::True => 1,
                Logic::False => 0,
                Logic::NA => i32::MIN,
            }
        }
    }

    impl From<i32> for Logic {
        fn from(value: i32) -> Self {
            match value {
                0 => Logic::False,
                1 => Logic::True,
                i32::MIN => Logic::NA,
                val => {
                    println!("{val}");
                    unreachable!()
                }
            }
        }
    }
}

pub mod lang {
    use std::{collections::HashMap, rc::Rc};

    #[derive(Debug, PartialEq)]
    pub struct Sym<'a> {
        pub(crate) data: &'a str,
    }

    impl<'a> Sym<'a> {
        pub fn new(data: &'a str) -> Self {
            Self { data }
        }
    }

    impl<'a> Into<super::Sexp<'a>> for Sym<'a> {
        fn into(self) -> super::Sexp<'a> {
            super::SexpKind::Sym(self).into()
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Target<'a> {
        Lang(&'a mut Lang<'a>), // expression
        Sym(Sym<'a>),           // named
    }

    impl<'a> From<Sym<'a>> for Target<'a> {
        fn from(value: Sym) -> Self {
            Target::Sym(value)
        }
    }

    impl<'a> From<&'a mut Lang<'a>> for Target<'a> {
        fn from(value: &'a mut Lang) -> Self {
            Target::Lang(value)
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Lang<'a> {
        pub(crate) target: Target<'a>,
        pub(crate) args: super::data::List<'a>,
    }

    impl<'a> Lang<'a> {
        pub fn new(target: Target<'a>, args: super::data::List<'a>) -> Self {
            Self { target, args }
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Formal<'a> {
        pub name: Sym<'a>,
        pub value: super::Sexp<'a>,
    }

    impl<'a> Formal<'a> {
        pub fn new(name: Sym<'a>, value: super::Sexp<'a>) -> Self {
            Self { name, value }
        }
    }

    impl<'a> TryInto<Formal<'a>> for super::data::TaggedSexp<'a> {
        type Error = crate::rds::rds_reader::RDSReaderError;

        fn try_into(self) -> Result<Formal<'a>, Self::Error> {
            match self.tag {
                Some(name) => Ok(Formal::new(Sym::new(name), self.data)),
                None => Err(crate::rds::rds_reader::RDSReaderError::DataError(
                    "Formal must have name as a tag".to_string(),
                )),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Closure<'a> {
        pub(crate) formals: &'a [Formal<'a>],
        pub(crate) body: Box<super::Sexp<'a>>,
        pub(crate) environment: Environment<'a>,
    }

    impl<'a> Closure<'a> {
        pub fn new(
            formals: &'a [Formal<'a>],
            body: super::Sexp<'a>,
            environment: Environment,
        ) -> Self {
            Self {
                formals,
                body: Box::new(body),
                environment,
            }
        }
    }

    impl<'a> Into<super::Sexp<'a>> for Closure<'a> {
        fn into(self) -> super::Sexp<'a> {
            super::SexpKind::Closure(self).into()
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Environment<'a> {
        Global,
        Base,
        Empty,
        Normal(NormalEnv<'a>),
        Namespace(Vec<super::Sexp<'a>>),
    }

    impl<'a> Environment<'a> {
        pub fn find_local_var(&self, name: &str) -> Option<&super::Sexp> {
            match self {
                Environment::Global
                | Environment::Base
                | Environment::Empty
                | Environment::Namespace(_) => None,
                Environment::Normal(env) => env.find_local_var(name),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct NormalEnv<'a> {
        pub parent: &'a Environment<'a>,
        pub locked: bool,
        pub frame: ListFrame<'a>,
        pub hash_frame: HashFrame<'a>,
    }

    impl<'a> NormalEnv<'a> {
        pub fn new(
            parent: &'a Environment,
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

    impl<'a> Into<Environment<'a>> for NormalEnv<'a> {
        fn into(self) -> Environment<'a> {
            Environment::Normal(self)
        }
    }

    #[derive(PartialEq, Default)]
    pub struct ListFrame<'a> {
        pub data: Option<super::data::List<'a>>,
        pub env: HashMap<&'a str, usize>,
    }

    impl std::fmt::Debug for ListFrame<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.data)
        }
    }

    impl ListFrame<'_> {
        pub fn new(data: super::data::List) -> Self {
            let mut env = HashMap::default();
            for (index, item) in data.iter().enumerate() {
                let Some(name) = item.tag.clone() else {
                    println!("{item}");
                    continue;
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

    #[derive(PartialEq, Default)]
    pub struct HashFrame<'a> {
        pub data: Option<Vec<super::Sexp<'a>>>,
        pub env: HashMap<&'a str, (usize, usize)>,
    }

    impl std::fmt::Debug for HashFrame<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.data)
        }
    }

    impl HashFrame<'_> {
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

    impl<'a> Into<super::SexpKind<'a>> for Environment<'a> {
        fn into(self) -> super::SexpKind<'a> {
            super::SexpKind::Environment(self)
        }
    }
}

// SXP
#[derive(Debug, PartialEq)]
pub enum SexpKind<'a> {
    Sym(lang::Sym<'a>),
    List(data::List<'a>),
    Nil,

    // language contructs
    Closure(lang::Closure<'a>),
    Environment(lang::Environment<'a>),
    Promise {
        environment: lang::Environment<'a>,
        expr: &'a mut Sexp<'a>,
        value: &'a mut Sexp<'a>,
    },
    Lang(lang::Lang<'a>),
    Bc(Bc),
    Buildin(lang::Sym<'a>),

    // vecs
    Char(&'a [char]),
    NAString,
    Logic(&'a [data::Logic]),
    Real(&'a [f64]),
    Int(&'a [i32]),
    Complex(&'a [data::Complex]),
    Str(&'a [String]),
    Vec(&'a [Sexp<'a>]),

    MissingArg,

    BaseNamespace, // as in GnuR fake namespace
}
