use std::ops::{Deref, DerefMut};

use bumpalo::Bump;

use super::sexp::{data, lang, Sexp, SexpKind};

pub struct Alloc<'a> {
    allocator: &'a mut Bump,
    pub nil: &'a mut Sexp<'a>,
    pub nil_list: data::List<'a>, // this is done to dedupe empty list
    pub nil_vec: &'a mut [&'a Sexp<'a>], // this is done to dedupe empty vecs
    pub missing: &'a mut Sexp<'a>,
    pub base_env: &'a mut Sexp<'a>,
    pub global_env: &'a mut Sexp<'a>,
    pub empty_env: &'a mut Sexp<'a>,
    pub na_string: &'a str,
}

impl<'a> Alloc<'a> {
    pub fn new(allocator: &'a mut Bump) -> Self {
        let nil = allocator.alloc(SexpKind::Nil.into());
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
        }
    }

    // this is only for environments that are stored in
    // Alloc structure (ie. base_env)
    fn get_inner_env(env: &'a mut Sexp) -> &'a mut lang::Environment<'a> {
        let SexpKind::Environment(env) = &mut env.kind else {
            unreachable!()
        };

        env
    }

    pub fn get_base(&mut self) -> &'a mut lang::Environment<'a> {
        Self::get_inner_env(self.base_env)
    }

    pub fn get_global(&mut self) -> &'a mut lang::Environment<'a> {
        Self::get_inner_env(self.global_env)
    }

    pub fn get_empty(&mut self) -> &'a mut lang::Environment<'a> {
        Self::get_inner_env(self.empty_env)
    }
}

impl<'a> Deref for Alloc<'a> {
    type Target = Bump;

    fn deref(&self) -> &Self::Target {
        &self.allocator
    }
}

impl<'a> DerefMut for Alloc<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.allocator
    }
}
