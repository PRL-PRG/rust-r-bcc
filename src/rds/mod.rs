use crate::sexp::sexp::{lang, Sexp, SexpKind};

pub mod rds_reader;
pub mod rds_writer;

#[derive(Clone, Copy, Debug)]
pub struct Flag {
    sexp_type: u8,
    level: i32,
    has_attributes: bool,
    has_tag: bool,
    orig: i32,
}

// these constants are taken directly from GNUR
// and all the comments with them
#[allow(dead_code)]
mod sexptype {
    pub const NILSXP: u8 = 0; /* nil = NULL */
    pub const SYMSXP: u8 = 1; /* symbols */
    pub const LISTSXP: u8 = 2; /* lists of dotted pairs */
    pub const CLOSXP: u8 = 3; /* closures */
    pub const ENVSXP: u8 = 4; /* environments */
    pub const PROMSXP: u8 = 5; /* promises: [un]evaluated closure arguments */
    pub const LANGSXP: u8 = 6; /* language constructs (special lists) */
    pub const SPECIALSXP: u8 = 7; /* special forms */
    pub const BUILTINSXP: u8 = 8; /* builtin non-special forms */
    pub const CHARSXP: u8 = 9; /* "scalar" string type (internal only)*/
    pub const LGLSXP: u8 = 10; /* logical vectors */
    /* 11 and 12 were factors and ordered factors in the 1990s */
    pub const INTSXP: u8 = 13; /* integer vectors */
    pub const REALSXP: u8 = 14; /* real variables */
    pub const CPLXSXP: u8 = 15; /* complex variables */
    pub const STRSXP: u8 = 16; /* string vectors */
    pub const DOTSXP: u8 = 17; /* dot-dot-dot object */
    pub const ANYSXP: u8 = 18; /* make "any" args work.
                               Used in specifying types for symbol
                               registration to mean anything is okay  */
    pub const VECSXP: u8 = 19; /* generic vectors */
    pub const EXPRSXP: u8 = 20; /* expressions vectors */
    pub const BCODESXP: u8 = 21; /* byte code */
    pub const EXTPTRSXP: u8 = 22; /* external pointer */
    pub const WEAKREFSXP: u8 = 23; /* weak reference */
    pub const RAWSXP: u8 = 24; /* raw bytes */
    pub const OBJSXP: u8 = 25; /* object, non-vector  */
    pub const S4SXP: u8 = 25; /* same as OBJSXP, retained for back compatability */

    /* used for detecting PROTECT issues in memory.c */
    pub const NEWSXP: u8 = 30; /* fresh node created in new page */
    pub const FREESXP: u8 = 31; /* node released by GC */

    pub const FUNSXP: u8 = 99; /* Closure or Builtin or Special */

    pub const REFSXP: u8 = 255;
    pub const NILVALUE_SXP: u8 = 254;
    pub const GLOBALENV_SXP: u8 = 253;
    pub const UNBOUNDVALUE_SXP: u8 = 252;
    pub const MISSINGARG_SXP: u8 = 251;
    pub const BASENAMESPACE_SXP: u8 = 250;
    pub const NAMESPACESXP: u8 = 249;
    pub const PACKAGESXP: u8 = 248;
    pub const PERSISTSXP: u8 = 247;

    pub const CLASSREFSXP: u8 = 246;
    pub const GENERICREFSXP: u8 = 245;
    pub const BCREPDEF: u8 = 244;
    pub const BCREPREF: u8 = 243;
    pub const EMPTYENV_SXP: u8 = 242;
    pub const BASEENV_SXP: u8 = 241;

    /* the following are needed to preserve attribute information on
    expressions in the constant pool of byte code objects. this is
    mainly for preserving source references attributes.  the original
    implementation of the sharing-preserving writing and reading of byte
    code objects did not account for the need to preserve attributes,
    so there is now a work-around using these sxp types to flag when
    the attrib field has been written out. object bits and s4 bits are
    still not preserved.  in the long run it might be better to change
    to a scheme in which all sharing is preserved and byte code objects
    don't need to be handled as a special case.  lt */
    pub const ATTRLANGSXP: u8 = 240;
    pub const ATTRLISTSXP: u8 = 239;
}

impl From<&Sexp> for Flag {
    fn from(value: &Sexp) -> Self {
        let sexp_type = match value.kind {
            SexpKind::Sym(_) => sexptype::SYMSXP,
            SexpKind::List(_) => sexptype::LISTSXP,
            SexpKind::Nil => sexptype::NILSXP,
            SexpKind::Closure(_) => sexptype::CLOSXP,
            SexpKind::Environment(lang::Environment::Global) => sexptype::GLOBALENV_SXP,
            SexpKind::Environment(lang::Environment::Base) => sexptype::BASEENV_SXP,
            SexpKind::Environment(lang::Environment::Empty) => sexptype::EMPTYENV_SXP,
            SexpKind::Environment(lang::Environment::Normal(_)) => sexptype::ENVSXP,
            SexpKind::Promise => sexptype::PROMSXP,
            SexpKind::Lang(_) => sexptype::LANGSXP,
            SexpKind::Bc(_) => sexptype::BCODESXP,
            SexpKind::Char(_) => sexptype::CHARSXP,
            SexpKind::Logic(_) => sexptype::LGLSXP,
            SexpKind::Real(_) => sexptype::REALSXP,
            SexpKind::Int(_) => sexptype::INTSXP,
            SexpKind::Complex(_) => sexptype::CPLXSXP,
            SexpKind::Str(_) => sexptype::STRSXP,
            SexpKind::Vec(_) => sexptype::VECSXP,
            SexpKind::MissingArg => sexptype::MISSINGARG_SXP,
        };

        if sexp_type == sexptype::REFSXP {
            panic!()
        } else {
            Flag {
                sexp_type,
                level: 1 << 3, // utf-8
                has_attributes: value.metadata.attr.is_some(),
                has_tag: false,
                orig: 0,
            }
        }
    }
}
