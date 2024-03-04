use super::sexp::Sexp;

pub struct ConstantPoolIdx(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Bc {
    instructions: Vec<i32>,
    constpool: Vec<Sexp>,
}

impl Bc {
    pub fn new() -> Self {
        Self {
            instructions: vec![Bc::version()],
            constpool: vec![],
        }
    }

    pub fn new_init(instructions: Vec<i32>, constpool: Vec<Sexp>) -> Self {
        Self {
            instructions,
            constpool,
        }
    }

    pub fn add_instr(&mut self, op: BcOp) {
        self.instructions.push(op.into());
    }

    pub fn add_instr2(&mut self, op: BcOp, idx: i32) {
        self.instructions.push(op.into());
        self.instructions.push(idx);
    }

    pub fn add_instr_n(&mut self, op: BcOp, idxs: &[i32]) {
        self.instructions.push(op.into());
        self.instructions.extend_from_slice(idxs);
    }

    pub fn add_const(&mut self, val: Sexp) -> i32 {
        match self.constpool.iter().position(|x| x == &val) {
            Some(idx) => idx as i32,
            None => {
                self.constpool.push(val);
                (self.constpool.len() - 1) as i32
            }
        }
    }

    // this is needed as a first instuction in bc
    fn version() -> i32 {
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
