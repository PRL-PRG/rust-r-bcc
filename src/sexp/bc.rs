use super::sexp::{lang, Sexp, SexpKind};

#[derive(Clone, Copy)]
pub enum ConstPoolItem<'a> {
    Sexp(&'a Sexp<'a>),
    Sym(&'a lang::Sym<'a>),
    Lang(&'a lang::Lang<'a>),
}

impl std::fmt::Debug for ConstPoolItem<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            match self {
                ConstPoolItem::Sexp(data) => write!(f, "{data:#?}"),
                ConstPoolItem::Sym(data) => write!(f, "{data:#?}"),
                ConstPoolItem::Lang(data) => write!(f, "{data:#?}"),
            }
        } else {
            match self {
                ConstPoolItem::Sexp(data) => write!(f, "{data:?}"),
                ConstPoolItem::Sym(data) => write!(f, "{data:?}"),
                ConstPoolItem::Lang(data) => write!(f, "{data:?}"),
            }
        }
    }
}

impl<'a> PartialEq<lang::Lang<'a>> for ConstPoolItem<'a> {
    fn eq(&self, other: &lang::Lang<'a>) -> bool {
        match self {
            ConstPoolItem::Sexp(sexp) => match &sexp.kind {
                SexpKind::Lang(lang) => std::ptr::eq(lang, other) || lang == other,
                _ => false,
            },
            ConstPoolItem::Sym(_) => false,
            ConstPoolItem::Lang(lang) => std::ptr::eq(*lang, other) || *lang == other,
        }
    }
}

impl<'a> PartialEq<lang::Sym<'a>> for ConstPoolItem<'a> {
    fn eq(&self, other: &lang::Sym<'a>) -> bool {
        match self {
            ConstPoolItem::Sexp(sexp) => match &sexp.kind {
                SexpKind::Sym(sym) if sexp.metadata.get_attr().is_none() => {
                    std::ptr::eq(sym, other) || sym == other
                }
                _ => false,
            },
            ConstPoolItem::Lang(_) => false,
            ConstPoolItem::Sym(sym) => std::ptr::eq(*sym, other) || *sym == other,
        }
    }
}

impl<'a> PartialEq<Sexp<'a>> for ConstPoolItem<'a> {
    fn eq(&self, other: &Sexp<'a>) -> bool {
        match self {
            ConstPoolItem::Sym(sym) => match &other.kind {
                SexpKind::Sym(other_sym) => std::ptr::eq(*sym, other_sym) || *sym == other_sym,
                _ => false,
            },
            ConstPoolItem::Lang(lang) => match &other.kind {
                SexpKind::Lang(other_lang) => {
                    std::ptr::eq(*lang, other_lang) || *lang == other_lang
                }
                _ => false,
            },
            ConstPoolItem::Sexp(sexp) => std::ptr::eq(*sexp, other) || *sexp == other,
        }
    }
}

impl<'a> PartialEq<ConstPoolItem<'a>> for ConstPoolItem<'a> {
    fn eq(&self, other: &ConstPoolItem<'a>) -> bool {
        match other {
            ConstPoolItem::Sexp(sexp) => self == *sexp,
            ConstPoolItem::Sym(sym) => self == *sym,
            ConstPoolItem::Lang(lang) => self == *lang,
        }
    }
}

impl<'a> From<&'a Sexp<'a>> for ConstPoolItem<'a> {
    fn from(value: &'a Sexp<'a>) -> Self {
        Self::Sexp(value)
    }
}

impl<'a> From<&'a lang::Lang<'a>> for ConstPoolItem<'a> {
    fn from(value: &'a lang::Lang<'a>) -> Self {
        Self::Lang(value)
    }
}

impl<'a> From<&'a lang::Target<'a>> for ConstPoolItem<'a> {
    fn from(value: &'a lang::Target<'a>) -> Self {
        match value {
            lang::Target::Lang(lang) => (*lang).into(),
            lang::Target::Sym(sym) => sym.into(),
        }
    }
}

impl<'a> From<&'a lang::Sym<'a>> for ConstPoolItem<'a> {
    fn from(value: &'a lang::Sym<'a>) -> Self {
        Self::Sym(value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Bc<'a> {
    pub instructions: &'a [i32],
    pub constpool: &'a [ConstPoolItem<'a>],
}

impl<'a> Into<Sexp<'a>> for Bc<'a> {
    fn into(self) -> Sexp<'a> {
        SexpKind::Bc(self).into()
    }
}

impl<'a> Bc<'a> {
    pub fn new(instructions: &'a [i32], constpool: &'a [ConstPoolItem<'a>]) -> Self {
        Self {
            instructions,
            constpool,
        }
    }

    // this is needed as a first instuction in bc
    pub fn version() -> i32 {
        12
    }
}

// allowed no camel case types to have same
// names as int C implementation
#[repr(u8)]
#[allow(non_camel_case_types)]
#[allow(unused)]
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum BcOp {
    BCMISMATCH_OP,
    RETURN_OP,
    GOTO_OP,
    BRIFNOT_OP,
    POP_OP,
    DUP_OP,
    PRINTVALUE_OP,
    STARTLOOPCNTXT_OP,
    ENDLOOPCNTXT_OP,
    DOLOOPNEXT_OP,
    DOLOOPBREAK_OP,
    STARTFOR_OP,
    STEPFOR_OP,
    ENDFOR_OP,
    SETLOOPVAL_OP,
    INVISIBLE_OP,
    LDCONST_OP,
    LDNULL_OP,
    LDTRUE_OP,
    LDFALSE_OP,
    GETVAR_OP,
    DDVAL_OP,
    SETVAR_OP,
    GETFUN_OP,
    GETGLOBFUN_OP,
    GETSYMFUN_OP,
    GETBUILTIN_OP,
    GETINTLBUILTIN_OP,
    CHECKFUN_OP,
    MAKEPROM_OP,
    DOMISSING_OP,
    SETTAG_OP,
    DODOTS_OP,
    PUSHARG_OP,
    PUSHCONSTARG_OP,
    PUSHNULLARG_OP,
    PUSHTRUEARG_OP,
    PUSHFALSEARG_OP,
    CALL_OP,
    CALLBUILTIN_OP,
    CALLSPECIAL_OP,
    MAKECLOSURE_OP,
    UMINUS_OP,
    UPLUS_OP,
    ADD_OP,
    SUB_OP,
    MUL_OP,
    DIV_OP,
    EXPT_OP,
    SQRT_OP,
    EXP_OP,
    EQ_OP,
    NE_OP,
    LT_OP,
    LE_OP,
    GE_OP,
    GT_OP,
    AND_OP,
    OR_OP,
    NOT_OP,
    DOTSERR_OP,
    STARTASSIGN_OP,
    ENDASSIGN_OP,
    STARTSUBSET_OP,
    DFLTSUBSET_OP,
    STARTSUBASSIGN_OP,
    DFLTSUBASSIGN_OP,
    STARTC_OP,
    DFLTC_OP,
    STARTSUBSET2_OP,
    DFLTSUBSET2_OP,
    STARTSUBASSIGN2_OP,
    DFLTSUBASSIGN2_OP,
    DOLLAR_OP,
    DOLLARGETS_OP,
    ISNULL_OP,
    ISLOGICAL_OP,
    ISINTEGER_OP,
    ISDOUBLE_OP,
    ISCOMPLEX_OP,
    ISCHARACTER_OP,
    ISSYMBOL_OP,
    ISOBJECT_OP,
    ISNUMERIC_OP,
    VECSUBSET_OP,
    MATSUBSET_OP,
    VECSUBASSIGN_OP,
    MATSUBASSIGN_OP,
    AND1ST_OP,
    AND2ND_OP,
    OR1ST_OP,
    OR2ND_OP,
    GETVAR_MISSOK_OP,
    DDVAL_MISSOK_OP,
    VISIBLE_OP,
    SETVAR2_OP,
    STARTASSIGN2_OP,
    ENDASSIGN2_OP,
    SETTER_CALL_OP,
    GETTER_CALL_OP,
    SWAP_OP,
    DUP2ND_OP,
    SWITCH_OP,
    RETURNJMP_OP,
    STARTSUBSET_N_OP,
    STARTSUBASSIGN_N_OP,
    VECSUBSET2_OP,
    MATSUBSET2_OP,
    VECSUBASSIGN2_OP,
    MATSUBASSIGN2_OP,
    STARTSUBSET2_N_OP,
    STARTSUBASSIGN2_N_OP,
    SUBSET_N_OP,
    SUBSET2_N_OP,
    SUBASSIGN_N_OP,
    SUBASSIGN2_N_OP,
    LOG_OP,
    LOGBASE_OP,
    MATH1_OP,
    DOTCALL_OP,
    COLON_OP,
    SEQALONG_OP,
    SEQLEN_OP,
    BASEGUARD_OP,
    INCLNK_OP,
    DECLNK_OP,
    DECLNK_N_OP,
    INCLNKSTK_OP,
    DECLNKSTK_OP,
    OPCOUNT,
}

// TODO you could implement this with proc macro
// but this should work for now
impl From<u8> for BcOp {
    fn from(value: u8) -> Self {
        if value <= BcOp::OPCOUNT as u8 {
            unsafe { std::mem::transmute(value) }
        } else {
            panic!()
        }
    }
}

impl Into<i32> for BcOp {
    fn into(self) -> i32 {
        self as i32
    }
}

impl BcOp {
    pub fn arity(&self) -> usize {
        match self {
            BcOp::RETURN_OP => 0,
            BcOp::GOTO_OP => 1,
            BcOp::BRIFNOT_OP => 2,
            BcOp::POP_OP => 0,
            BcOp::DUP_OP => 0,
            BcOp::PRINTVALUE_OP => 0,
            BcOp::STARTLOOPCNTXT_OP => 2,
            BcOp::ENDLOOPCNTXT_OP => 1,
            BcOp::DOLOOPNEXT_OP => 0,
            BcOp::DOLOOPBREAK_OP => 0,
            BcOp::STARTFOR_OP => 3,
            BcOp::STEPFOR_OP => 1,
            BcOp::ENDFOR_OP => 0,
            BcOp::SETLOOPVAL_OP => 0,
            BcOp::INVISIBLE_OP => 0,
            BcOp::LDCONST_OP => 1,
            BcOp::LDNULL_OP => 0,
            BcOp::LDTRUE_OP => 0,
            BcOp::LDFALSE_OP => 0,
            BcOp::GETVAR_OP => 1,
            BcOp::DDVAL_OP => 1,
            BcOp::SETVAR_OP => 1,
            BcOp::GETFUN_OP => 1,
            BcOp::GETGLOBFUN_OP => 1,
            BcOp::GETSYMFUN_OP => 1,
            BcOp::GETBUILTIN_OP => 1,
            BcOp::GETINTLBUILTIN_OP => 1,
            BcOp::CHECKFUN_OP => 0,
            BcOp::MAKEPROM_OP => 1,
            BcOp::DOMISSING_OP => 0,
            BcOp::SETTAG_OP => 1,
            BcOp::DODOTS_OP => 0,
            BcOp::PUSHARG_OP => 0,
            BcOp::PUSHCONSTARG_OP => 1,
            BcOp::PUSHNULLARG_OP => 0,
            BcOp::PUSHTRUEARG_OP => 0,
            BcOp::PUSHFALSEARG_OP => 0,
            BcOp::CALL_OP => 1,
            BcOp::CALLBUILTIN_OP => 1,
            BcOp::CALLSPECIAL_OP => 1,
            BcOp::MAKECLOSURE_OP => 1,
            BcOp::UMINUS_OP => 1,
            BcOp::UPLUS_OP => 1,
            BcOp::ADD_OP => 1,
            BcOp::SUB_OP => 1,
            BcOp::MUL_OP => 1,
            BcOp::DIV_OP => 1,
            BcOp::EXPT_OP => 1,
            BcOp::SQRT_OP => 1,
            BcOp::EXP_OP => 1,
            BcOp::EQ_OP => 1,
            BcOp::NE_OP => 1,
            BcOp::LT_OP => 1,
            BcOp::LE_OP => 1,
            BcOp::GE_OP => 1,
            BcOp::GT_OP => 1,
            BcOp::AND_OP => 1,
            BcOp::OR_OP => 1,
            BcOp::NOT_OP => 1,
            BcOp::DOTSERR_OP => 0,
            BcOp::STARTASSIGN_OP => 1,
            BcOp::ENDASSIGN_OP => 1,
            BcOp::STARTSUBSET_OP => 2,
            BcOp::DFLTSUBSET_OP => 0,
            BcOp::STARTSUBASSIGN_OP => 2,
            BcOp::DFLTSUBASSIGN_OP => 0,
            BcOp::STARTC_OP => 2,
            BcOp::DFLTC_OP => 0,
            BcOp::STARTSUBSET2_OP => 2,
            BcOp::DFLTSUBSET2_OP => 0,
            BcOp::STARTSUBASSIGN2_OP => 2,
            BcOp::DFLTSUBASSIGN2_OP => 0,
            BcOp::DOLLAR_OP => 2,
            BcOp::DOLLARGETS_OP => 2,
            BcOp::ISNULL_OP => 0,
            BcOp::ISLOGICAL_OP => 0,
            BcOp::ISINTEGER_OP => 0,
            BcOp::ISDOUBLE_OP => 0,
            BcOp::ISCOMPLEX_OP => 0,
            BcOp::ISCHARACTER_OP => 0,
            BcOp::ISSYMBOL_OP => 0,
            BcOp::ISOBJECT_OP => 0,
            BcOp::ISNUMERIC_OP => 0,
            BcOp::VECSUBSET_OP => 1,
            BcOp::MATSUBSET_OP => 1,
            BcOp::VECSUBASSIGN_OP => 1,
            BcOp::MATSUBASSIGN_OP => 1,
            BcOp::AND1ST_OP => 2,
            BcOp::AND2ND_OP => 1,
            BcOp::OR1ST_OP => 2,
            BcOp::OR2ND_OP => 1,
            BcOp::GETVAR_MISSOK_OP => 1,
            BcOp::DDVAL_MISSOK_OP => 1,
            BcOp::VISIBLE_OP => 0,
            BcOp::SETVAR2_OP => 1,
            BcOp::STARTASSIGN2_OP => 1,
            BcOp::ENDASSIGN2_OP => 1,
            BcOp::SETTER_CALL_OP => 2,
            BcOp::GETTER_CALL_OP => 1,
            BcOp::SWAP_OP => 0,
            BcOp::DUP2ND_OP => 0,
            BcOp::SWITCH_OP => 4,
            BcOp::RETURNJMP_OP => 0,
            BcOp::STARTSUBSET_N_OP => 2,
            BcOp::STARTSUBASSIGN_N_OP => 2,
            BcOp::VECSUBSET2_OP => 1,
            BcOp::MATSUBSET2_OP => 1,
            BcOp::VECSUBASSIGN2_OP => 1,
            BcOp::MATSUBASSIGN2_OP => 1,
            BcOp::STARTSUBSET2_N_OP => 2,
            BcOp::STARTSUBASSIGN2_N_OP => 2,
            BcOp::SUBSET_N_OP => 2,
            BcOp::SUBSET2_N_OP => 2,
            BcOp::SUBASSIGN_N_OP => 2,
            BcOp::SUBASSIGN2_N_OP => 2,
            BcOp::LOG_OP => 1,
            BcOp::LOGBASE_OP => 1,
            BcOp::MATH1_OP => 2,
            BcOp::DOTCALL_OP => 2,
            BcOp::COLON_OP => 1,
            BcOp::SEQALONG_OP => 1,
            BcOp::SEQLEN_OP => 1,
            BcOp::BASEGUARD_OP => 2,
            BcOp::INCLNK_OP => 0,
            BcOp::DECLNK_OP => 0,
            BcOp::DECLNK_N_OP => 1,
            BcOp::INCLNKSTK_OP => 0,
            BcOp::DECLNKSTK_OP => 0,
            BcOp::BCMISMATCH_OP => todo!(),
            BcOp::OPCOUNT => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // this is done as sanity check
    #[test]
    fn test_correct_bc_instr_vals() {
        use BcOp::*;
        let all_instrs_ordered = vec![
            BCMISMATCH_OP,
            RETURN_OP,
            GOTO_OP,
            BRIFNOT_OP,
            POP_OP,
            DUP_OP,
            PRINTVALUE_OP,
            STARTLOOPCNTXT_OP,
            ENDLOOPCNTXT_OP,
            DOLOOPNEXT_OP,
            DOLOOPBREAK_OP,
            STARTFOR_OP,
            STEPFOR_OP,
            ENDFOR_OP,
            SETLOOPVAL_OP,
            INVISIBLE_OP,
            LDCONST_OP,
            LDNULL_OP,
            LDTRUE_OP,
            LDFALSE_OP,
            GETVAR_OP,
            DDVAL_OP,
            SETVAR_OP,
            GETFUN_OP,
            GETGLOBFUN_OP,
            GETSYMFUN_OP,
            GETBUILTIN_OP,
            GETINTLBUILTIN_OP,
            CHECKFUN_OP,
            MAKEPROM_OP,
            DOMISSING_OP,
            SETTAG_OP,
            DODOTS_OP,
            PUSHARG_OP,
            PUSHCONSTARG_OP,
            PUSHNULLARG_OP,
            PUSHTRUEARG_OP,
            PUSHFALSEARG_OP,
            CALL_OP,
            CALLBUILTIN_OP,
            CALLSPECIAL_OP,
            MAKECLOSURE_OP,
            UMINUS_OP,
            UPLUS_OP,
            ADD_OP,
            SUB_OP,
            MUL_OP,
            DIV_OP,
            EXPT_OP,
            SQRT_OP,
            EXP_OP,
            EQ_OP,
            NE_OP,
            LT_OP,
            LE_OP,
            GE_OP,
            GT_OP,
            AND_OP,
            OR_OP,
            NOT_OP,
            DOTSERR_OP,
            STARTASSIGN_OP,
            ENDASSIGN_OP,
            STARTSUBSET_OP,
            DFLTSUBSET_OP,
            STARTSUBASSIGN_OP,
            DFLTSUBASSIGN_OP,
            STARTC_OP,
            DFLTC_OP,
            STARTSUBSET2_OP,
            DFLTSUBSET2_OP,
            STARTSUBASSIGN2_OP,
            DFLTSUBASSIGN2_OP,
            DOLLAR_OP,
            DOLLARGETS_OP,
            ISNULL_OP,
            ISLOGICAL_OP,
            ISINTEGER_OP,
            ISDOUBLE_OP,
            ISCOMPLEX_OP,
            ISCHARACTER_OP,
            ISSYMBOL_OP,
            ISOBJECT_OP,
            ISNUMERIC_OP,
            VECSUBSET_OP,
            MATSUBSET_OP,
            VECSUBASSIGN_OP,
            MATSUBASSIGN_OP,
            AND1ST_OP,
            AND2ND_OP,
            OR1ST_OP,
            OR2ND_OP,
            GETVAR_MISSOK_OP,
            DDVAL_MISSOK_OP,
            VISIBLE_OP,
            SETVAR2_OP,
            STARTASSIGN2_OP,
            ENDASSIGN2_OP,
            SETTER_CALL_OP,
            GETTER_CALL_OP,
            SWAP_OP,
            DUP2ND_OP,
            SWITCH_OP,
            RETURNJMP_OP,
            STARTSUBSET_N_OP,
            STARTSUBASSIGN_N_OP,
            VECSUBSET2_OP,
            MATSUBSET2_OP,
            VECSUBASSIGN2_OP,
            MATSUBASSIGN2_OP,
            STARTSUBSET2_N_OP,
            STARTSUBASSIGN2_N_OP,
            SUBSET_N_OP,
            SUBSET2_N_OP,
            SUBASSIGN_N_OP,
            SUBASSIGN2_N_OP,
            LOG_OP,
            LOGBASE_OP,
            MATH1_OP,
            DOTCALL_OP,
            COLON_OP,
            SEQALONG_OP,
            SEQLEN_OP,
            BASEGUARD_OP,
            INCLNK_OP,
            DECLNK_OP,
            DECLNK_N_OP,
            INCLNKSTK_OP,
            DECLNKSTK_OP,
            OPCOUNT,
        ];

        for (inst, byte) in all_instrs_ordered.into_iter().zip(0..) {
            assert_eq!(byte, inst as u8);
            assert_eq!(inst, byte.into());
        }
    }
}
