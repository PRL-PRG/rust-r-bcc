use std::cell::UnsafeCell;

use super::bc::Bc;

#[derive(Default)]
pub struct MetaData<'a> {
    pub attr: UnsafeCell<Option<&'a Sexp<'a>>>,
}

impl<'a> PartialEq for MetaData<'a> {
    fn eq(&self, other: &Self) -> bool {
        let self_data = unsafe { self.attr.get().as_ref().unwrap() };
        let other_data = unsafe { other.attr.get().as_ref().unwrap() };
        self_data == other_data
    }
}

impl<'a> std::fmt::Debug for MetaData<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner = unsafe { self.attr.get().as_ref().unwrap() };
        if f.alternate() {
            write!(f, "MetaData {{ attr: {:#?} }}", inner)
        } else {
            write!(f, "MetaData {{ attr: {:?} }}", inner)
        }
    }
}

impl<'a> MetaData<'a> {
    pub fn is_obj(&self) -> bool {
        if self.get_attr().is_some() {
            let attr = self.get_attr().unwrap();
            if let Sexp {
                kind: SexpKind::List(list),
                ..
            } = attr
            {
                list.into_iter().any(|x| {
                    let x = match x.tag {
                        Some(x) if x.data == "class" => true,
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

    pub fn get_attr(&self) -> &Option<&'a Sexp<'a>> {
        unsafe { self.attr.get().as_ref().unwrap() }
    }

    pub fn get_attr_mut(&self) -> &mut Option<&'a Sexp<'a>> {
        unsafe { self.attr.get().as_mut().unwrap() }
    }
}

#[derive(Debug)]
pub struct Sexp<'a> {
    pub kind: SexpKind<'a>,
    pub metadata: MetaData<'a>,
}

impl<'a> PartialEq for Sexp<'a> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other) || (self.kind == other.kind && self.metadata == other.metadata)
    }
}

impl<'a> From<SexpKind<'a>> for Sexp<'a> {
    fn from(kind: SexpKind<'a>) -> Self {
        Sexp {
            kind,
            metadata: MetaData {
                attr: UnsafeCell::new(None),
            },
        }
    }
}

impl<'a> Sexp<'a> {
    pub fn set_attr(&'a self, attr: &'a Sexp<'a>) {
        *self.metadata.get_attr_mut() = Some(attr);
    }
}

pub mod data {
    use std::ops::{Deref, DerefMut};

    use super::Sexp;

    #[derive(Debug, Clone)]
    pub struct TaggedSexp<'a> {
        pub tag: Option<&'a super::lang::Sym<'a>>,
        pub data: &'a Sexp<'a>,
    }

    impl<'a> PartialEq for TaggedSexp<'a> {
        fn eq(&self, other: &Self) -> bool {
            match (self.tag, other.tag) {
                (None, Some(_)) | (Some(_), None) => return false,
                (Some(a), Some(b)) if !(std::ptr::eq(a.data, b.data) || a.data == b.data) => {
                    return false
                }
                _ => (),
            }
            std::ptr::eq(self.data, other.data) || self.data == other.data
        }
    }

    impl<'a> TaggedSexp<'a> {
        pub fn new(data: &'a Sexp<'a>) -> Self {
            Self { tag: None, data }
        }

        pub fn new_with_tag(data: &'a Sexp<'a>, tag: &'a super::lang::Sym<'a>) -> Self {
            let tag = Some(tag);
            Self { tag, data }
        }

        pub fn strip_tag(&self) -> Self {
            Self::new(self.data)
        }
    }

    impl<'a> From<&'a Sexp<'a>> for TaggedSexp<'a> {
        fn from(value: &'a Sexp<'a>) -> Self {
            Self::new(value)
        }
    }

    #[derive(Debug, PartialEq, Default)]
    pub struct Complex {
        pub real: Double,
        pub imaginary: Double,
    }

    #[derive(Clone, Copy)]
    pub struct List<'a> {
        pub data: &'a [TaggedSexp<'a>],
    }

    impl<'a> PartialEq for List<'a> {
        fn eq(&self, other: &Self) -> bool {
            std::ptr::eq(self.data, other.data) || self.data == other.data
        }
    }

    impl std::fmt::Debug for List<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if f.alternate() {
                write!(f, "{:#?}", self.data)
            } else {
                write!(f, "{:?}", self.data)
            }
        }
    }

    impl<'a> Deref for List<'a> {
        type Target = [TaggedSexp<'a>];

        fn deref(&self) -> &Self::Target {
            &self.data
        }
    }

    #[derive(Clone, Copy)]
    pub struct RVec<'a, T>
    where
        T: PartialEq,
    {
        data: &'a [T],
    }

    impl<'a, T> RVec<'a, T>
    where
        T: PartialEq,
    {
        pub fn new(data: &'a [T]) -> Self {
            Self { data }
        }
    }

    impl<'a, T> PartialEq for RVec<'a, T>
    where
        T: PartialEq,
    {
        fn eq(&self, other: &Self) -> bool {
            std::ptr::eq(self.data, other.data) || self.data == other.data
        }
    }

    impl<'a, T> Deref for RVec<'a, T>
    where
        T: PartialEq,
    {
        type Target = [T];

        fn deref(&self) -> &Self::Target {
            self.data
        }
    }

    impl<'a, T> std::fmt::Debug for RVec<'a, T>
    where
        T: PartialEq + std::fmt::Debug,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if f.alternate() {
                write!(f, "{:#?}", self.data)
            } else {
                write!(f, "{:?}", self.data)
            }
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

    // This is needed because of the NA equality
    #[derive(Clone, Copy, Default)]
    pub struct Double {
        pub data: f64,
    }

    impl std::fmt::Debug for Double {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if f.alternate() {
                write!(f, "{:#?}", self.data)
            } else {
                write!(f, "{:?}", self.data)
            }
        }
    }

    impl Into<f64> for Double {
        fn into(self) -> f64 {
            self.data
        }
    }

    impl From<f64> for Double {
        fn from(value: f64) -> Self {
            Double { data: value }
        }
    }

    impl Deref for Double {
        type Target = f64;

        fn deref(&self) -> &Self::Target {
            &self.data
        }
    }

    impl PartialEq for Double {
        fn eq(&self, other: &Self) -> bool {
            (self.data.is_nan() && other.data.is_nan()) || self.data == other.data
        }
    }
}

pub mod lang {
    use std::collections::HashMap;

    use crate::sexp::{bc::ConstPoolItem, sexp_alloc::Alloc};

    #[derive(Debug, Clone)]
    pub struct Sym<'a> {
        pub(crate) data: &'a str,
    }

    impl<'a> PartialEq for Sym<'a> {
        fn eq(&self, other: &Self) -> bool {
            std::ptr::eq(self.data, other.data) || self.data == other.data
        }
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

    #[derive(Debug, Clone)]
    pub enum Target<'a> {
        Lang(&'a Lang<'a>), // expression
        Sym(Sym<'a>),       // named
    }

    impl<'a> PartialEq for Target<'a> {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (Target::Lang(a), Target::Lang(b)) => std::ptr::eq(*a, *b) || *a == *b,
                (Target::Sym(a), Target::Sym(b)) => std::ptr::eq(a.data, b.data) || *a == *b,
                _ => false,
            }
        }
    }

    impl<'a> From<Sym<'a>> for Target<'a> {
        fn from(value: Sym<'a>) -> Self {
            Target::Sym(value)
        }
    }

    impl<'a> From<&'a mut Lang<'a>> for Target<'a> {
        fn from(value: &'a mut Lang<'a>) -> Self {
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

    #[derive(Debug, PartialEq, Clone)]
    pub struct Formal<'a> {
        pub name: Sym<'a>,
        pub value: &'a super::Sexp<'a>,
    }

    impl<'a> Formal<'a> {
        pub fn new(name: Sym<'a>, value: &'a super::Sexp<'a>) -> Self {
            Self { name, value }
        }
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Closure<'a> {
        pub(crate) formals: &'a [Formal<'a>],
        pub(crate) body: &'a super::Sexp<'a>,
        pub(crate) environment: &'a Environment<'a>,
    }

    impl<'a> Closure<'a> {
        pub fn new(
            formals: &'a [Formal<'a>],
            body: &'a super::Sexp<'a>,
            environment: &'a Environment<'a>,
        ) -> Self {
            Self {
                formals,
                body,
                environment,
            }
        }

        pub fn body(&self) -> Option<&super::Sexp<'a>> {
            match &self.body.kind {
                super::SexpKind::Bc(bc) => {
                    if bc.constpool.is_empty() {
                        None
                    } else {
                        let ConstPoolItem::Sexp(res) = &bc.constpool[0] else {
                            return None;
                        };
                        Some(res)
                    }
                }
                _ => Some(self.body),
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
        Normal(&'a NormalEnv<'a>),
        Namespace(&'a [&'a super::Sexp<'a>]),
    }

    impl<'a> Environment<'a> {
        pub fn find_local_var(&'a self, name: &str) -> Option<&super::Sexp> {
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
            parent: &'a Environment<'a>,
            locked: bool,
            frame: ListFrame<'a>,
            hash_frame: HashFrame<'a>,
        ) -> Self {
            Self {
                parent,
                locked,
                frame,
                hash_frame,
            }
        }

        pub fn find_local_var(&'a self, name: &str) -> Option<&super::Sexp> {
            match self.frame.get(name) {
                Some(res) => Some(res),
                None => match self.hash_frame.get(name) {
                    Some(res) => Some(res),
                    None => self.parent.find_local_var(name),
                },
            }
        }
    }

    pub struct ListFrame<'a> {
        pub data: Option<super::data::List<'a>>,
        // this is boxed to allow droping this with the arena death
        pub env: bumpalo::boxed::Box<'a, HashMap<&'a str, usize>>,
    }

    impl<'a> PartialEq for ListFrame<'a> {
        fn eq(&self, other: &Self) -> bool {
            self.data == other.data
        }
    }

    impl std::fmt::Debug for ListFrame<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.data)
        }
    }

    impl<'a> ListFrame<'a> {
        pub fn new(data: super::data::List<'a>, arena: &'a Alloc<'a>) -> Self {
            let mut env = bumpalo::boxed::Box::new_in(HashMap::default(), arena);
            for (index, item) in data.iter().enumerate() {
                let Some(name) = item.tag.clone() else {
                    println!("{item}");
                    continue;
                };
                env.insert(name.data, index);
            }
            Self {
                data: if std::ptr::eq(arena.nil_list.data, data.data) {
                    None
                } else {
                    Some(data)
                },
                env,
            }
        }

        pub fn get(&'a self, name: &str) -> Option<&super::Sexp> {
            let index = self.env.get(name)?.clone();
            match &self.data {
                Some(data) => Some(&data[index].data),
                None => None,
            }
        }
    }

    pub struct HashFrame<'a> {
        pub data: Option<&'a [&'a super::Sexp<'a>]>,
        // this is boxed to allow droping this with the arena death
        pub env: bumpalo::boxed::Box<'a, HashMap<&'a str, (usize, usize)>>,
    }

    impl<'a> PartialEq for HashFrame<'a> {
        fn eq(&self, other: &Self) -> bool {
            self.data == other.data
        }
    }

    impl std::fmt::Debug for HashFrame<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.data)
        }
    }

    impl<'a> HashFrame<'a> {
        pub fn new(data: &'a [&'a super::Sexp<'a>], arena: &'a Alloc<'a>) -> Self {
            let mut env = bumpalo::boxed::Box::new_in(HashMap::new(), arena);

            for (block, item) in data.iter().enumerate() {
                match &item.kind {
                    super::SexpKind::List(list) => {
                        for (idx, item) in list.into_iter().enumerate() {
                            let Some(name) = item.tag.clone() else {
                                unreachable!()
                            };
                            env.insert(name.data, (block, idx));
                        }
                    }
                    super::SexpKind::Nil => (),
                    _ => unreachable!(),
                }
            }

            Self {
                data: if std::ptr::eq(arena.nil_vec, data) {
                    None
                } else {
                    Some(data)
                },
                env,
            }
        }

        pub fn get(&'a self, name: &str) -> Option<&super::Sexp> {
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
#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum SexpKind<'a> {
    Sym(lang::Sym<'a>),
    List(data::List<'a>),
    Nil,

    // language contructs
    Closure(lang::Closure<'a>),
    Environment(lang::Environment<'a>),
    Promise {
        environment: &'a lang::Environment<'a>,
        expr: &'a Sexp<'a>,
        value: &'a Sexp<'a>,
    },
    Lang(lang::Lang<'a>),
    Bc(Bc<'a>),
    Buildin(lang::Sym<'a>),

    // vecs
    Char(&'a [char]),
    NAString,
    Logic(&'a [data::Logic]),
    Real(data::RVec<'a, data::Double>),
    Int(data::RVec<'a, i32>),
    Complex(data::RVec<'a, data::Complex>),
    Str(data::RVec<'a, &'a str>),
    Vec(&'a [&'a Sexp<'a>]),

    MissingArg,
    UnboundVal,

    BaseNamespace, // as in GnuR fake namespace
}
