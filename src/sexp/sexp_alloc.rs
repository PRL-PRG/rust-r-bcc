use std::ops::Deref;

use bumpalo::Bump;

use super::sexp::{data, lang, MetaData, Sexp, SexpKind};

pub struct Alloc<'a> {
    allocator: &'a Bump,
    pub nil: &'a Sexp<'a>,
    pub nil_list: data::List<'a>,    // this is done to dedupe empty list
    pub nil_vec: &'a [&'a Sexp<'a>], // this is done to dedupe empty vecs
    pub missing: &'a Sexp<'a>,
    pub base_env: &'a Sexp<'a>,
    pub global_env: &'a Sexp<'a>,
    pub empty_env: &'a Sexp<'a>,
    pub na_string: &'a str,
    pub unbound: &'a Sexp<'a>,

    pub empty_metadata: &'a MetaData<'a>,
}

impl<'a> Alloc<'a> {
    pub fn new(allocator: &'a Bump) -> Self {
        let nil = allocator.alloc(SexpKind::Nil.into()) as &'a Sexp;
        Self {
            allocator,
            nil,
            nil_list: data::List {
                data: allocator.alloc_slice_fill_with(0, |_| data::TaggedSexp::new(nil)),
            },
            nil_vec: allocator.alloc_slice_fill_copy(0, nil as &'a Sexp),
            missing: allocator.alloc(SexpKind::MissingArg.into()),
            base_env: allocator.alloc(SexpKind::Environment(lang::Environment::Base).into()),
            global_env: allocator.alloc(SexpKind::Environment(lang::Environment::Global).into()),
            empty_env: allocator.alloc(SexpKind::Environment(lang::Environment::Empty).into()),
            na_string: allocator.alloc_str("__NA__"),
            unbound: allocator.alloc(SexpKind::UnboundVal.into()),
            empty_metadata: allocator.alloc(MetaData::default()),
        }
    }

    // this is only for environments that are stored in
    // Alloc structure (ie. base_env)
    fn get_inner_env(env: &'a Sexp<'a>) -> &'a lang::Environment<'a> {
        let SexpKind::Environment(env) = &env.kind else {
            unreachable!()
        };

        env
    }

    pub fn get_base(&self) -> &'a lang::Environment<'a> {
        Self::get_inner_env(self.base_env)
    }

    pub fn get_global(&self) -> &'a lang::Environment<'a> {
        Self::get_inner_env(self.global_env)
    }

    pub fn get_empty(&self) -> &'a lang::Environment<'a> {
        Self::get_inner_env(self.empty_env)
    }
}

impl<'a> Deref for Alloc<'a> {
    type Target = Bump;

    fn deref(&self) -> &Self::Target {
        &self.allocator
    }
}
