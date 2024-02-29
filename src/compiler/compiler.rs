use crate::sexp::{
    bc::{Bc, BcInstr},
    sexp::lang,
};

pub struct Compiler {
    options: CompilerOptions,
    env: CompilerEnvironment,
    code_buffer: CodeBuffer,
}

pub struct CompilerOptions;

pub struct CompilerEnvironment;

type CodeBuffer = Bc;

pub fn cmpfun(closure: lang::Closure, options: CompilerOptions) -> Bc {
    todo!()
}

fn gen_code(target: lang::Target, options: CompilerOptions) -> Vec<BcInstr> {
    todo!()
}
