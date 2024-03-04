use std::fmt::Display;

use super::sexp::{data, lang, Sexp, SexpKind};

impl Display for Sexp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for SexpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SexpKind::Sym(sym) => write!(f, "{}", sym.data),
            SexpKind::List(_) => todo!(),
            SexpKind::Nil => todo!(),
            SexpKind::Closure(_) => todo!(),
            SexpKind::Environment(_) => todo!(),
            SexpKind::Promise => todo!(),
            SexpKind::Lang(lang) => todo!(),
            SexpKind::Char(_) => todo!(),
            SexpKind::Logic(_) => todo!(),
            SexpKind::Real(_) => todo!(),
            SexpKind::Int(_) => todo!(),
            SexpKind::Complex(_) => todo!(),
            SexpKind::Str(_) => todo!(),
            SexpKind::Vec(_) => todo!(),
            SexpKind::MissingArg => todo!(),
            SexpKind::Bc(_) => todo!(),
        }
    }
}

impl Display for lang::Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for data::TaggedSexp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.tag {
            Some(tag) => todo!(),
            None => write!(f, "{}", self.data),
        }
    }
}
