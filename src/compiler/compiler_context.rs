use crate::sexp::sexp::{data, lang, Sexp};

use super::code_buf::LabelIdx;

#[derive(Default, Clone)]
pub struct LoopContext {
    pub loop_label: LabelIdx,
    pub end_label: LabelIdx,
    pub goto_ok: bool,
}

#[derive(Default, Clone)]
pub struct CompilerContext<'a> {
    pub top_level: bool,
    pub need_returnjmp: bool,
    pub tailcall: bool,
    pub loop_ctx: Option<LoopContext>,
    pub call: Option<&'a lang::Lang<'a>>,
}

impl<'a> CompilerContext<'a> {
    pub fn new_top(ctxt: &CompilerContext) -> Self {
        Self {
            top_level: true,
            tailcall: true,
            call: None,
            ..ctxt.clone()
        }
    }

    pub fn new_promise(ctxt: &CompilerContext) -> Self {
        Self {
            top_level: false,
            tailcall: true,
            ..ctxt.clone()
        }
    }

    pub fn new_call(ctxt: &CompilerContext, call: &'a lang::Lang<'a>) -> Self {
        Self {
            call: Some(call),
            ..ctxt.clone()
        }
    }

    pub fn new_arg(ctxt: &CompilerContext) -> Self {
        Self {
            tailcall: false,
            top_level: false,
            loop_ctx: match &ctxt.loop_ctx {
                Some(ctx) => Some(LoopContext {
                    goto_ok: false,
                    ..ctx.clone()
                }),
                None => None,
            },
            ..ctxt.clone()
        }
    }

    pub fn new_function(ctxt : &CompilerContext, formals : &data::List, body : &Sexp) -> Self {
        let tmp = CompilerContext::new_top(ctxt);
        tmp
    }

    pub fn new_loop(ctxt: &CompilerContext, loop_label: LabelIdx, end_label: LabelIdx) -> Self {
        let mut res = Self {
            loop_ctx: Some(LoopContext {
                loop_label,
                end_label,
                goto_ok: true,
            }),
            ..ctxt.clone()
        };
        res.tailcall = false;
        res
    }
}
