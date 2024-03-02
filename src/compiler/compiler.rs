use crate::sexp::{
    bc::{Bc, BcOp},
    sexp::{lang, Sexp, SexpKind},
};

struct CompilerContext {
    tailcall: bool,
}

impl CompilerContext {
    fn new_top() -> Self {
        Self { tailcall: true }
    }
}

pub struct Compiler {
    options: CompilerOptions,
    context: CompilerContext,
    code_buffer: Bc,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            options: CompilerOptions,
            context: CompilerContext::new_top(),
            code_buffer: Bc::new(),
        }
    }

    pub fn cmpfun(self, closure: lang::Closure) -> Bc {
        self.gen_code(*closure.body)
    }

    fn gen_code(self, target: Sexp) -> Bc {
        let mut tmp = self;
        tmp.code_buffer.add_const(target.clone());
        tmp.cmp(target);
        tmp.code_buffer
    }

    fn cmp(&mut self, sexp: Sexp) {
        match sexp.kind {
            SexpKind::Sym(_) => todo!(),
            SexpKind::Nil => self.code_buffer.add_instr(BcOp::LDNULL_OP),
            SexpKind::Environment(_) => todo!(),
            SexpKind::Promise => todo!(),
            SexpKind::Lang(_) => todo!(),
            _ => {
                let ci = self.code_buffer.add_const(sexp);
                self.code_buffer.add_instr2(BcOp::LDCONST_OP, ci);
            }
        };

        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }
    }
}

pub struct CompilerOptions;
