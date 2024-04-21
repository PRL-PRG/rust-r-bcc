use crate::sexp::sexp::Sexp;

use super::code_buf::LabelIdx;

#[derive(Default, Clone)]
pub struct LoopContext {
    pub loop_label: LabelIdx,
    pub end_label: LabelIdx,
    pub goto_ok: bool,
}

#[derive(Default, Clone)]
pub struct CompilerContext {
    pub top_level: bool,
    pub need_returnjmp: bool,
    pub tailcall: bool,
    pub loop_ctx: Option<LoopContext>,
    pub call: Option<Sexp>,
}

impl CompilerContext {
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

    pub fn new_call(ctxt: &CompilerContext, call: Sexp) -> Self {
        Self {
            call: Some(call.clone()),
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
