use std::fmt::Display;

use crate::sexp::sexp_display::join_string;

use super::bc::{Bc, BcOp, ConstPoolItem};

impl<'a> Display for Bc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // instructions
        write!(f, "instruction : \n")?;
        let mut index: usize = 1;
        while index < self.instructions.len() {
            let inst = self.instructions[index];
            let bc: BcOp = (inst as u8).into();
            write!(f, "\t{bc}")?;
            index += 1;
            for val in &self.instructions[index..(index + bc.arity())] {
                write!(f, " {val}")?;
                index += 1;
            }
            write!(f, "\n")?
        }

        // const pool
        write!(
            f,
            "constant pool : [{}]\n",
            join_string(&self.constpool, ", ")
        )?;

        Ok(())
    }
}

impl Display for ConstPoolItem<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstPoolItem::Sexp(data) => write!(f, "{data}"),
            ConstPoolItem::Sym(data) => write!(f, "{data}"),
            ConstPoolItem::Lang(data) => write!(f, "{data}"),
        }
    }
}

impl Display for BcOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BcOp::BCMISMATCH_OP => write!(f, "BCMISMATCH_OP"),
            BcOp::RETURN_OP => write!(f, "RETURN_OP"),
            BcOp::GOTO_OP => write!(f, "GOTO_OP"),
            BcOp::BRIFNOT_OP => write!(f, "BRIFNOT_OP"),
            BcOp::POP_OP => write!(f, "POP_OP"),
            BcOp::DUP_OP => write!(f, "DUP_OP"),
            BcOp::PRINTVALUE_OP => write!(f, "PRINTVALUE_OP"),
            BcOp::STARTLOOPCNTXT_OP => write!(f, "STARTLOOPCNTXT_OP"),
            BcOp::ENDLOOPCNTXT_OP => write!(f, "ENDLOOPCNTXT_OP"),
            BcOp::DOLOOPNEXT_OP => write!(f, "DOLOOPNEXT_OP"),
            BcOp::DOLOOPBREAK_OP => write!(f, "DOLOOPBREAK_OP"),
            BcOp::STARTFOR_OP => write!(f, "STARTFOR_OP"),
            BcOp::STEPFOR_OP => write!(f, "STEPFOR_OP"),
            BcOp::ENDFOR_OP => write!(f, "ENDFOR_OP"),
            BcOp::SETLOOPVAL_OP => write!(f, "SETLOOPVAL_OP"),
            BcOp::INVISIBLE_OP => write!(f, "INVISIBLE_OP"),
            BcOp::LDCONST_OP => write!(f, "LDCONST_OP"),
            BcOp::LDNULL_OP => write!(f, "LDNULL_OP"),
            BcOp::LDTRUE_OP => write!(f, "LDTRUE_OP"),
            BcOp::LDFALSE_OP => write!(f, "LDFALSE_OP"),
            BcOp::GETVAR_OP => write!(f, "GETVAR_OP"),
            BcOp::DDVAL_OP => write!(f, "DDVAL_OP"),
            BcOp::SETVAR_OP => write!(f, "SETVAR_OP"),
            BcOp::GETFUN_OP => write!(f, "GETFUN_OP"),
            BcOp::GETGLOBFUN_OP => write!(f, "GETGLOBFUN_OP"),
            BcOp::GETSYMFUN_OP => write!(f, "GETSYMFUN_OP"),
            BcOp::GETBUILTIN_OP => write!(f, "GETBUILTIN_OP"),
            BcOp::GETINTLBUILTIN_OP => write!(f, "GETINTLBUILTIN_OP"),
            BcOp::CHECKFUN_OP => write!(f, "CHECKFUN_OP"),
            BcOp::MAKEPROM_OP => write!(f, "MAKEPROM_OP"),
            BcOp::DOMISSING_OP => write!(f, "DOMISSING_OP"),
            BcOp::SETTAG_OP => write!(f, "SETTAG_OP"),
            BcOp::DODOTS_OP => write!(f, "DODOTS_OP"),
            BcOp::PUSHARG_OP => write!(f, "PUSHARG_OP"),
            BcOp::PUSHCONSTARG_OP => write!(f, "PUSHCONSTARG_OP"),
            BcOp::PUSHNULLARG_OP => write!(f, "PUSHNULLARG_OP"),
            BcOp::PUSHTRUEARG_OP => write!(f, "PUSHTRUEARG_OP"),
            BcOp::PUSHFALSEARG_OP => write!(f, "PUSHFALSEARG_OP"),
            BcOp::CALL_OP => write!(f, "CALL_OP"),
            BcOp::CALLBUILTIN_OP => write!(f, "CALLBUILTIN_OP"),
            BcOp::CALLSPECIAL_OP => write!(f, "CALLSPECIAL_OP"),
            BcOp::MAKECLOSURE_OP => write!(f, "MAKECLOSURE_OP"),
            BcOp::UMINUS_OP => write!(f, "UMINUS_OP"),
            BcOp::UPLUS_OP => write!(f, "UPLUS_OP"),
            BcOp::ADD_OP => write!(f, "ADD_OP"),
            BcOp::SUB_OP => write!(f, "SUB_OP"),
            BcOp::MUL_OP => write!(f, "MUL_OP"),
            BcOp::DIV_OP => write!(f, "DIV_OP"),
            BcOp::EXPT_OP => write!(f, "EXPT_OP"),
            BcOp::SQRT_OP => write!(f, "SQRT_OP"),
            BcOp::EXP_OP => write!(f, "EXP_OP"),
            BcOp::EQ_OP => write!(f, "EQ_OP"),
            BcOp::NE_OP => write!(f, "NE_OP"),
            BcOp::LT_OP => write!(f, "LT_OP"),
            BcOp::LE_OP => write!(f, "LE_OP"),
            BcOp::GE_OP => write!(f, "GE_OP"),
            BcOp::GT_OP => write!(f, "GT_OP"),
            BcOp::AND_OP => write!(f, "AND_OP"),
            BcOp::OR_OP => write!(f, "OR_OP"),
            BcOp::NOT_OP => write!(f, "NOT_OP"),
            BcOp::DOTSERR_OP => write!(f, "DOTSERR_OP"),
            BcOp::STARTASSIGN_OP => write!(f, "STARTASSIGN_OP"),
            BcOp::ENDASSIGN_OP => write!(f, "ENDASSIGN_OP"),
            BcOp::STARTSUBSET_OP => write!(f, "STARTSUBSET_OP"),
            BcOp::DFLTSUBSET_OP => write!(f, "DFLTSUBSET_OP"),
            BcOp::STARTSUBASSIGN_OP => write!(f, "STARTSUBASSIGN_OP"),
            BcOp::DFLTSUBASSIGN_OP => write!(f, "DFLTSUBASSIGN_OP"),
            BcOp::STARTC_OP => write!(f, "STARTC_OP"),
            BcOp::DFLTC_OP => write!(f, "DFLTC_OP"),
            BcOp::STARTSUBSET2_OP => write!(f, "STARTSUBSET2_OP"),
            BcOp::DFLTSUBSET2_OP => write!(f, "DFLTSUBSET2_OP"),
            BcOp::STARTSUBASSIGN2_OP => write!(f, "STARTSUBASSIGN2_OP"),
            BcOp::DFLTSUBASSIGN2_OP => write!(f, "DFLTSUBASSIGN2_OP"),
            BcOp::DOLLAR_OP => write!(f, "DOLLAR_OP"),
            BcOp::DOLLARGETS_OP => write!(f, "DOLLARGETS_OP"),
            BcOp::ISNULL_OP => write!(f, "ISNULL_OP"),
            BcOp::ISLOGICAL_OP => write!(f, "ISLOGICAL_OP"),
            BcOp::ISINTEGER_OP => write!(f, "ISINTEGER_OP"),
            BcOp::ISDOUBLE_OP => write!(f, "ISDOUBLE_OP"),
            BcOp::ISCOMPLEX_OP => write!(f, "ISCOMPLEX_OP"),
            BcOp::ISCHARACTER_OP => write!(f, "ISCHARACTER_OP"),
            BcOp::ISSYMBOL_OP => write!(f, "ISSYMBOL_OP"),
            BcOp::ISOBJECT_OP => write!(f, "ISOBJECT_OP"),
            BcOp::ISNUMERIC_OP => write!(f, "ISNUMERIC_OP"),
            BcOp::VECSUBSET_OP => write!(f, "VECSUBSET_OP"),
            BcOp::MATSUBSET_OP => write!(f, "MATSUBSET_OP"),
            BcOp::VECSUBASSIGN_OP => write!(f, "VECSUBASSIGN_OP"),
            BcOp::MATSUBASSIGN_OP => write!(f, "MATSUBASSIGN_OP"),
            BcOp::AND1ST_OP => write!(f, "AND1ST_OP"),
            BcOp::AND2ND_OP => write!(f, "AND2ND_OP"),
            BcOp::OR1ST_OP => write!(f, "OR1ST_OP"),
            BcOp::OR2ND_OP => write!(f, "OR2ND_OP"),
            BcOp::GETVAR_MISSOK_OP => write!(f, "GETVAR_MISSOK_OP"),
            BcOp::DDVAL_MISSOK_OP => write!(f, "DDVAL_MISSOK_OP"),
            BcOp::VISIBLE_OP => write!(f, "VISIBLE_OP"),
            BcOp::SETVAR2_OP => write!(f, "SETVAR2_OP"),
            BcOp::STARTASSIGN2_OP => write!(f, "STARTASSIGN2_OP"),
            BcOp::ENDASSIGN2_OP => write!(f, "ENDASSIGN2_OP"),
            BcOp::SETTER_CALL_OP => write!(f, "SETTER_CALL_OP"),
            BcOp::GETTER_CALL_OP => write!(f, "GETTER_CALL_OP"),
            BcOp::SWAP_OP => write!(f, "SWAP_OP"),
            BcOp::DUP2ND_OP => write!(f, "DUP2ND_OP"),
            BcOp::SWITCH_OP => write!(f, "SWITCH_OP"),
            BcOp::RETURNJMP_OP => write!(f, "RETURNJMP_OP"),
            BcOp::STARTSUBSET_N_OP => write!(f, "STARTSUBSET_N_OP"),
            BcOp::STARTSUBASSIGN_N_OP => write!(f, "STARTSUBASSIGN_N_OP"),
            BcOp::VECSUBSET2_OP => write!(f, "VECSUBSET2_OP"),
            BcOp::MATSUBSET2_OP => write!(f, "MATSUBSET2_OP"),
            BcOp::VECSUBASSIGN2_OP => write!(f, "VECSUBASSIGN2_OP"),
            BcOp::MATSUBASSIGN2_OP => write!(f, "MATSUBASSIGN2_OP"),
            BcOp::STARTSUBSET2_N_OP => write!(f, "STARTSUBSET2_N_OP"),
            BcOp::STARTSUBASSIGN2_N_OP => write!(f, "STARTSUBASSIGN2_N_OP"),
            BcOp::SUBSET_N_OP => write!(f, "SUBSET_N_OP"),
            BcOp::SUBSET2_N_OP => write!(f, "SUBSET2_N_OP"),
            BcOp::SUBASSIGN_N_OP => write!(f, "SUBASSIGN_N_OP"),
            BcOp::SUBASSIGN2_N_OP => write!(f, "SUBASSIGN2_N_OP"),
            BcOp::LOG_OP => write!(f, "LOG_OP"),
            BcOp::LOGBASE_OP => write!(f, "LOGBASE_OP"),
            BcOp::MATH1_OP => write!(f, "MATH1_OP"),
            BcOp::DOTCALL_OP => write!(f, "DOTCALL_OP"),
            BcOp::COLON_OP => write!(f, "COLON_OP"),
            BcOp::SEQALONG_OP => write!(f, "SEQALONG_OP"),
            BcOp::SEQLEN_OP => write!(f, "SEQLEN_OP"),
            BcOp::BASEGUARD_OP => write!(f, "BASEGUARD_OP"),
            BcOp::INCLNK_OP => write!(f, "INCLNK_OP"),
            BcOp::DECLNK_OP => write!(f, "DECLNK_OP"),
            BcOp::DECLNK_N_OP => write!(f, "DECLNK_N_OP"),
            BcOp::INCLNKSTK_OP => write!(f, "INCLNKSTK_OP"),
            BcOp::DECLNKSTK_OP => write!(f, "DECLNKSTK_OP"),
            BcOp::OPCOUNT => write!(f, "OPCOUNT"),
        }
    }
}
