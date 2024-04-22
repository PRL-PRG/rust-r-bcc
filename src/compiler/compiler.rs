use std::collections::HashSet;

use crate::sexp::{
    bc::{Bc, BcOp},
    sexp::{data, lang, MetaData, Sexp, SexpKind},
};

use super::{
    code_buf::{CodeBuffer, DEFLABEL},
    compiler_context::CompilerContext,
};

#[derive(Debug)]
pub enum Warning {
    VariableDoesNotExist(String),
    NoLoopContext,
}

struct InlineInfo {
    guard: bool,
    base_var: bool,
}

pub struct Compiler {
    options: CompilerOptions,
    context: CompilerContext,
    code_buffer: CodeBuffer,

    pub warnings: Vec<Warning>,

    env: lang::Environment,
    localenv: HashSet<String>,
    baseenv: Option<lang::NormalEnv>,
    namespacebase: Option<lang::NormalEnv>,

    specials: HashSet<String>,
    builtins: HashSet<String>,
}

const LANG_FUNCS: [&str; 46] = [
    "^", "~", "<", "<<-", "<=", "<-", "=", "==", ">", ">=", "|", "||", "-", ":", "!", "!=", "/",
    "(", "[", "[<-", "[[", "[[<-", "{", "@", "$", "$<-", "*", "&", "&&", "%/%", "%*%", "%%", "+",
    "::", ":::", "@<-", "break", "for", "function", "if", "next", "repeat", "while", "local",
    "return", "switch",
];

impl Compiler {
    pub fn new() -> Self {
        Self {
            options: CompilerOptions::default(),
            context: CompilerContext::new_top(&CompilerContext::default()),
            code_buffer: CodeBuffer::new(),

            env: lang::Environment::Global,
            localenv: HashSet::new(),
            baseenv: None,
            namespacebase: None,

            warnings: vec![],

            specials: HashSet::new(),
            builtins: HashSet::new(),
        }
    }

    pub fn new_options(inline_level: usize) -> Self {
        Self {
            options: CompilerOptions { inline_level },
            context: CompilerContext::new_top(&CompilerContext::default()),
            code_buffer: CodeBuffer::new(),

            env: lang::Environment::Global,
            localenv: HashSet::new(),
            baseenv: None,
            namespacebase: None,

            warnings: vec![],

            specials: HashSet::new(),
            builtins: HashSet::new(),
        }
    }

    pub fn set_baseenv(&mut self, env: lang::NormalEnv) {
        self.baseenv = Some(env);
    }

    pub fn set_namespacebase(&mut self, env: lang::NormalEnv) {
        self.namespacebase = Some(env);
    }

    pub fn cmpfun(&mut self, closure: lang::Closure) -> lang::Closure {
        let mut closure = closure;
        self.env = lang::NormalEnv::new(
            Box::new(closure.environment),
            false,
            lang::ListFrame::new(
                closure
                    .formals
                    .iter()
                    .map(|x| {
                        data::TaggedSexp::new_with_tag(
                            x.value.as_ref().clone(),
                            x.name.data.clone(),
                        )
                    })
                    .collect(),
            ),
            lang::HashFrame::new(vec![]),
        )
        .into();
        self.find_locals(&closure.body);
        println!("locals : {:?}", self.localenv);
        let body =
            SexpKind::Bc(self.gen_code(closure.body.as_ref(), Some(closure.body.as_ref()))).into();
        closure.body = Box::new(body);
        let lang::Environment::Normal(env) = &mut self.env else {
            unreachable!()
        };
        closure.environment = std::mem::replace(env.parent.as_mut(), lang::Environment::Global);
        closure
    }

    fn gen_code(&mut self, target: &Sexp, loc: Option<&Sexp>) -> Bc {
        let tmp = if let Some(loc) = loc {
            CodeBuffer::new_with_expr(loc.clone())
        } else {
            CodeBuffer::new()
        };
        let orig = std::mem::replace(&mut self.code_buffer, tmp);
        self.code_buffer.add_const(target.clone());
        self.cmp(&target, false, false);
        let locs = self.create_expression_loc();
        self.code_buffer.add_const(locs);
        self.code_buffer.patch_labels();
        std::mem::replace(&mut self.code_buffer, orig).bc
    }

    /// Default for missing_ok and set_loc in original compiler
    /// is missing_ok = FALSE and set_loc = TRUE
    fn cmp(&mut self, sexp: &Sexp, missing_ok: bool, set_loc: bool) {
        let orig = if set_loc {
            self.code_buffer.set_current_expr(sexp.clone())
        } else {
            None
        };
        match &sexp.kind {
            SexpKind::Sym(sym) => self.cmp_sym(sym, missing_ok),
            SexpKind::Nil => {
                self.code_buffer.add_instr(BcOp::LDNULL_OP);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
            }
            SexpKind::Environment(_) => todo!(),
            SexpKind::Promise {
                environment: _,
                expr: _,
                value: _,
            } => {
                todo!()
            }
            SexpKind::Lang(lang) => self.cmp_call(lang, true),
            _ => {
                let ci = self.code_buffer.add_const(sexp.clone());
                self.code_buffer.add_instr2(BcOp::LDCONST_OP, ci);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
            }
        };
        self.code_buffer.restore_current_expr(orig);
    }

    fn cmp_sym(&mut self, sym: &lang::Sym, missing_ok: bool) {
        match sym.data.as_str() {
            ".." => todo!(),
            name if name.starts_with("..") => todo!(),
            name => {
                if !self.var_exist(name) {
                    self.warnings
                        .push(Warning::VariableDoesNotExist(name.to_string()))
                }
                let index = self.code_buffer.add_const(sym.clone().into());
                if missing_ok {
                    self.code_buffer.add_instr2(BcOp::GETVAR_MISSOK_OP, index);
                } else {
                    self.code_buffer.add_instr2(BcOp::GETVAR_OP, index);
                }
            }
        }
        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }
    }

    fn cmp_call(&mut self, call: &lang::Lang, inline_ok: bool) {
        // set up state
        let mut sexp_tmp = std::mem::take(&mut self.code_buffer.current_expr);
        let orig = self.code_buffer.set_current_expr(call.clone().into());
        let tmp = CompilerContext::new_call(&self.context, call.clone().into());
        let mut orig_context = std::mem::replace(&mut self.context, tmp);

        if inline_ok && self.try_inline(call) {
            return;
        }

        match &call.target {
            lang::Target::Lang(lang) => {
                let orig_tailcall = self.context.tailcall;
                self.context.tailcall = false;

                self.cmp(&lang.as_ref().clone().into(), false, true);
                self.code_buffer.add_instr(BcOp::CHECKFUN_OP);
                self.cmp_args(&call.args);
                let index = self.code_buffer.add_const(call.clone().into());
                self.code_buffer.add_instr2(BcOp::CALL_OP, index);

                self.context.tailcall = orig_tailcall;
                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
            }
            lang::Target::Sym(sym) => {
                let index = self.code_buffer.add_const(sym.clone().into());
                self.code_buffer.add_instr2(BcOp::GETFUN_OP, index);
                self.cmp_args(&call.args);
                let index = self.code_buffer.add_const(call.clone().into());
                self.code_buffer.add_instr2(BcOp::CALL_OP, index);
                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
            }
        };

        // restore state
        self.code_buffer.restore_current_expr(orig);
        std::mem::swap(&mut self.context, &mut orig_context);
        std::mem::swap(&mut self.code_buffer.current_expr, &mut sexp_tmp);
    }

    fn cmp_args(&mut self, args: &data::List) {
        let tmp = CompilerContext::new_promise(&self.context);
        let mut orig_context = std::mem::replace(&mut self.context, tmp);

        for arg in args {
            match &arg.data.kind {
                SexpKind::MissingArg => todo!(),
                SexpKind::Sym(sym) if sym.data.as_str() == ".." => todo!(),
                SexpKind::Bc(_) => todo!(),
                SexpKind::Promise {
                    environment: _,
                    expr: _,
                    value: _,
                } => todo!(),
                SexpKind::Sym(_) | SexpKind::Lang(_) => {
                    let curr = self.code_buffer.current_expr.clone();
                    let code = self.gen_code(&arg.data, curr.as_ref());
                    let index = self.code_buffer.add_const(code.into());
                    self.code_buffer.add_instr2(BcOp::MAKEPROM_OP, index);
                }
                SexpKind::Nil => {
                    self.code_buffer.add_instr(BcOp::PUSHNULLARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Logic(logs) if logs.len() == 1 && logs[0] => {
                    self.code_buffer.add_instr(BcOp::PUSHTRUEARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Logic(logs) if logs.len() == 1 && !logs[0] => {
                    self.code_buffer.add_instr(BcOp::PUSHFALSEARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                _ => {
                    let index = self.code_buffer.add_const(arg.data.clone());
                    self.code_buffer.add_instr2(BcOp::PUSHCONSTARG_OP, index);
                    self.cmp_tag(&arg.tag);
                }
            }
        }

        std::mem::swap(&mut self.context, &mut orig_context);
    }

    fn cmp_tag(&mut self, tag: &Option<String>) {
        if let Some(tag) = tag {
            let tag: lang::Sym = tag.as_str().into();
            let tag: Sexp = tag.into();
            let index = self.code_buffer.add_const(tag);
            self.code_buffer.add_instr2(BcOp::SETTAG_OP, index);
        }
    }

    fn var_exist(&self, name: &str) -> bool {
        self.env.find_local_var(name).is_some()
    }

    fn create_expression_loc(&mut self) -> Sexp {
        Sexp {
            kind: SexpKind::Int(std::mem::take(&mut self.code_buffer.expression_buffer)),
            metadata: MetaData {
                attr: Some(Box::new(Sexp {
                    kind: SexpKind::List(vec![data::TaggedSexp {
                        tag: Some("class".into()),
                        data: Sexp {
                            kind: SexpKind::Str(vec!["expressionsIndex".into()]),
                            metadata: MetaData { attr: None },
                        },
                    }]),
                    metadata: MetaData { attr: None },
                })),
            },
        }
    }

    fn cmp_prim2(&mut self, first: &Sexp, second: &Sexp, full: &lang::Lang, op: BcOp) {
        let taicall = self.context.tailcall;
        self.context.tailcall = false;
        self.cmp(first, false, true);
        self.context.tailcall = taicall;

        let tmp = CompilerContext::new_arg(&self.context);
        let mut orig = std::mem::replace(&mut self.context, tmp);

        self.cmp(second, false, true);

        let index = self.code_buffer.add_const(full.clone().into());
        self.code_buffer.add_instr2(op, index);

        std::mem::swap(&mut self.context, &mut orig);

        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }
    }

    fn cmp_dispatch(&mut self) {
        todo!()
    }

    fn cmp_subset_dispatch(&mut self) {
        todo!()
    }

    fn try_inline(&mut self, expr: &lang::Lang) -> bool {
        let sym = match &expr.target {
            lang::Target::Lang(_) => return false,
            lang::Target::Sym(s) => s.data.as_str(),
        };

        let info = self.get_inlineinfo(sym);

        if info.is_none() {
            return false;
        }

        let info = info.unwrap();

        if info.guard {
            let tailcall = self.context.tailcall;
            self.context.tailcall = false;
            let expr_index = self.code_buffer.add_const(expr.clone().into());
            let end_label = self.code_buffer.make_label();
            self.code_buffer
                .add_instr_n(BcOp::BASEGUARD_OP, &[expr_index, DEFLABEL]);
            self.code_buffer.set_label(end_label);
            if !self.handle_inline(sym, expr, info) {
                self.cmp_call(expr, false);
            }

            self.code_buffer.put_label(end_label);
            if tailcall {
                self.code_buffer.add_instr(BcOp::RETURN_OP);
            }
            self.context.tailcall = tailcall;

            true
        } else {
            self.handle_inline(sym, expr, info)
        }
    }

    fn handle_inline(&mut self, sym: &str, expr: &lang::Lang, info: InlineInfo) -> bool {
        match sym {
            "if" => {
                let cond = &expr.args[0].data;
                let then_block = &expr.args[1].data;
                let else_block = expr.args.get(2).map(|x| &x.data);

                // condition is compiled without tailcall
                let tailcall = self.context.tailcall;
                self.context.tailcall = false;
                self.cmp(cond, false, true);
                self.context.tailcall = tailcall;

                let call_idx = self.code_buffer.add_const(expr.clone().into());
                let else_label = self.code_buffer.make_label();

                self.code_buffer
                    .add_instr_n(BcOp::BRIFNOT_OP, &[call_idx, DEFLABEL]);
                self.code_buffer.set_label(else_label);
                self.cmp(then_block, false, true);

                if self.context.tailcall {
                    self.code_buffer.put_label(else_label);
                    match else_block {
                        Some(block) => {
                            self.cmp(block, false, true);
                        }
                        None => {
                            self.code_buffer.add_instr(BcOp::LDNULL_OP);
                            self.code_buffer.add_instr(BcOp::INVISIBLE_OP);
                            self.code_buffer.add_instr(BcOp::RETURN_OP);
                        }
                    }
                } else {
                    let end_label = self.code_buffer.make_label();
                    self.code_buffer.add_instr2(BcOp::GOTO_OP, DEFLABEL);
                    self.code_buffer.set_label(end_label);
                    self.code_buffer.put_label(else_label);

                    match else_block {
                        Some(block) => {
                            self.cmp(block, false, true);
                        }
                        None => {
                            self.code_buffer.add_instr(BcOp::LDNULL_OP);
                        }
                    };
                    self.code_buffer.put_label(end_label);
                }

                true
            }
            "{" => {
                if expr.args.is_empty() {
                    self.cmp(&SexpKind::Nil.into(), false, true);
                    return true;
                }

                let orig_loc = std::mem::replace(&mut self.code_buffer.current_expr, None);

                let tailcall = self.context.tailcall;
                self.context.tailcall = false;
                for inner in &expr.args[0..(expr.args.len() - 1)] {
                    self.code_buffer.set_current_expr(inner.data.clone());
                    self.cmp(&inner.data, false, false);
                    self.code_buffer.set_current_expr(inner.data.clone());
                    self.code_buffer.add_instr(BcOp::POP_OP);
                }
                self.context.tailcall = tailcall;

                self.code_buffer
                    .set_current_expr(expr.args.last().unwrap().data.clone());
                self.cmp(&expr.args.last().unwrap().data, false, false);

                self.code_buffer.restore_current_expr(orig_loc);

                true
            }
            "<-" if expr.args.len() == 2 && matches!(&expr.args[0].data.kind, SexpKind::Sym(_)) => {
                let tailcall = self.context.tailcall;
                self.context.tailcall = false;
                self.cmp(&expr.args[1].data, false, true);
                self.context.tailcall = tailcall;

                let index = self.code_buffer.add_const(expr.args[0].data.clone());
                self.code_buffer.add_instr2(BcOp::SETVAR_OP, index);
                true
            }
            "+" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::ADD_OP);
                true
            }
            "[[" => {
                todo!()
            }
            "while" => {
                let cond = &expr.args[0].data;
                let body = &expr.args[1].data;

                if self.check_skip_loopctx(cond, true) && self.check_skip_loopctx(body, true) {
                    self.cmp_while_body(expr, cond, body);
                } else {
                    let returnjmp = self.context.need_returnjmp;
                    self.context.need_returnjmp = true;
                    let long_jump_label = self.code_buffer.make_label();
                    self.code_buffer
                        .add_instr_n(BcOp::STARTLOOPCNTXT_OP, &[0, DEFLABEL]);
                    self.code_buffer.set_label(long_jump_label);
                    self.cmp_while_body(expr, cond, body);
                    self.code_buffer.put_label(long_jump_label);
                    self.code_buffer.add_instr2(BcOp::ENDLOOPCNTXT_OP, 0);
                    self.context.need_returnjmp = returnjmp;
                }

                self.code_buffer.add_instr(BcOp::LDNULL_OP);
                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::INVISIBLE_OP);
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }

                true
            }
            ">" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::GT_OP);
                true
            }
            "break" => match &self.context.loop_ctx {
                Some(loop_ctx) => {
                    self.code_buffer.add_instr2(BcOp::GOTO_OP, DEFLABEL);
                    self.code_buffer.set_label(loop_ctx.end_label);
                    true
                }
                None => {
                    self.warnings.push(Warning::NoLoopContext);
                    false
                }
            },
            "function" => {
                let forms = &expr.args[0].data;
                let body = &expr.args[1].data;

                let SexpKind::List(forms) = &forms.kind else {
                    return false;
                };

                let tmp = CompilerContext::new_function(&self.context, forms, body);
                let orig = std::mem::replace(&mut self.context, tmp);

                let comp_body = self.gen_code(body, None);

                let index = self.code_buffer.add_const(
                    SexpKind::Vec(vec![
                        SexpKind::List(forms.clone()).into(),
                        comp_body.into(),
                        SexpKind::Nil.into(),
                    ])
                    .into(),
                );
                self.code_buffer.add_instr2(BcOp::MAKECLOSURE_OP, index);

                let _ = std::mem::replace(&mut self.context, orig);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }

                true
            }
            _ if info.base_var && self.builtins.contains(sym) => {
                let index = self.code_buffer.add_const(expr.target.clone().into());
                self.code_buffer.add_instr2(BcOp::GETBUILTIN_OP, index);
                self.cmp_builtin_args(&expr.args);

                let index = self.code_buffer.add_const(expr.clone().into());

                self.code_buffer.add_instr2(BcOp::CALLBUILTIN_OP, index);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }

                true
            }
            _ => false,
        }
    }

    fn has_handler(&self, sym: &str) -> bool {
        match sym {
            "if" | "{" | "<-" | "+" | "while" | ">" | "break" | "function" => true,
            "[[" => {
                todo!()
            }
            _ if self.builtins.contains(sym) => true,
            _ => false,
        }
    }

    fn cmp_builtin_args(&mut self, args: &data::List) {
        let tmp = CompilerContext::new_promise(&self.context);
        let mut orig_context = std::mem::replace(&mut self.context, tmp);

        for arg in args {
            match &arg.data.kind {
                SexpKind::MissingArg => todo!(),
                SexpKind::Sym(sym) if sym.data.as_str() == ".." => todo!(),
                SexpKind::Bc(_) => todo!(),
                SexpKind::Promise {
                    environment: _,
                    expr: _,
                    value: _,
                } => todo!(),
                SexpKind::Sym(_) | SexpKind::Lang(_) => {
                    self.cmp(&arg.data, false, true);
                    self.code_buffer.add_instr(BcOp::PUSHARG_OP);
                }
                SexpKind::Nil => {
                    self.code_buffer.add_instr(BcOp::PUSHNULLARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Logic(logs) if logs.len() == 1 && logs[0] => {
                    self.code_buffer.add_instr(BcOp::PUSHTRUEARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Logic(logs) if logs.len() == 1 && !logs[0] => {
                    self.code_buffer.add_instr(BcOp::PUSHFALSEARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                _ => {
                    let index = self.code_buffer.add_const(arg.data.clone());
                    self.code_buffer.add_instr2(BcOp::PUSHCONSTARG_OP, index);
                    self.cmp_tag(&arg.tag);
                }
            }
        }

        std::mem::swap(&mut self.context, &mut orig_context);
    }

    fn cmp_while_body(&mut self, full: &lang::Lang, cond: &Sexp, body: &Sexp) {
        let loop_label = self.code_buffer.make_label();
        let end_label = self.code_buffer.make_label();
        self.code_buffer.put_label(loop_label);

        let tmp = CompilerContext::new_loop(&self.context, loop_label, end_label);
        let orig = std::mem::replace(&mut self.context, tmp);

        self.cmp(cond, false, true);

        let callidx = self.code_buffer.add_const(full.clone().into());
        self.code_buffer
            .add_instr_n(BcOp::BRIFNOT_OP, &[callidx, DEFLABEL]);
        self.code_buffer.set_label(end_label);
        self.cmp(body, false, true);

        self.code_buffer.add_instr(BcOp::POP_OP);
        self.code_buffer.add_instr2(BcOp::GOTO_OP, DEFLABEL);
        self.code_buffer.set_label(loop_label);
        self.code_buffer.put_label(end_label);

        let _ = std::mem::replace(&mut self.context, orig);
    }

    fn check_skip_loopctx_lang(&self, lang: &lang::Lang, break_ok: bool) -> bool {
        match &lang.target {
            lang::Target::Sym(sym) => {
                if !break_ok && matches!(sym.data.as_str(), "break" | "next") {
                    false
                } else if self.is_loop_stop_fun(sym.data.as_str()) {
                    true
                } else if self.is_loop_top_fun(sym.data.as_str()) {
                    self.check_skip_loopctx_list(&lang.args, break_ok)
                } else if matches!(sym.data.as_str(), "eval" | "evalq" | "source") {
                    false
                } else {
                    self.check_skip_loopctx_list(&lang.args, false)
                }
            }
            lang::Target::Lang(target) => {
                self.check_skip_loopctx_lang(target, false)
                    || self.check_skip_loopctx_list(&lang.args, false)
            }
        }
    }

    // default for break_ok is true
    fn check_skip_loopctx(&self, sexp: &Sexp, break_ok: bool) -> bool {
        match &sexp.kind {
            SexpKind::Lang(lang) => self.check_skip_loopctx_lang(lang, break_ok),
            _ => true,
        }
    }

    fn check_skip_loopctx_list(&self, list: &data::List, break_ok: bool) -> bool {
        for arg in list {
            if !self.missing(&arg.data) && !self.check_skip_loopctx(&arg.data, break_ok) {
                return false;
            }
        }
        true
    }

    fn is_loop_stop_fun(&self, name: &str) -> bool {
        matches!(name, "function" | "for" | "while" | "repeat") && self.is_base_var(name)
    }

    fn is_loop_top_fun(&self, name: &str) -> bool {
        matches!(name, "(" | "{" | "if") && self.is_base_var(name)
    }

    fn missing(&self, sexp: &Sexp) -> bool {
        // TODO
        false
    }

    fn find_baseenv(&self, name: &str) -> Option<&Sexp> {
        match &self.baseenv {
            Some(env) => env.find_local_var(name),
            None => None,
        }
    }

    fn find_namespacebase(&self, name: &str) -> Option<&Sexp> {
        match &self.namespacebase {
            Some(env) => env.find_local_var(name),
            None => None,
        }
    }

    fn is_base_var(&self, name: &str) -> bool {
        let local = self.env.find_local_var(name).is_some() || self.localenv.contains(name);
        (self.find_baseenv(name).is_some() || self.find_namespacebase(name).is_some()) && !local
    }

    fn get_inlineinfo(&self, function: &str) -> Option<InlineInfo> {
        let base_var = self.is_base_var(function);
        if self.options.inline_level > 0 && base_var && self.has_handler(function) {
            let info = InlineInfo {
                guard: !(self.options.inline_level >= 3
                    || (self.options.inline_level >= 2 && LANG_FUNCS.contains(&function))),
                base_var,
            };
            Some(info)
        } else {
            None
        }
    }

    fn get_assigned_var(&mut self, sexp: &Sexp) -> Option<String> {
        match &sexp.kind {
            SexpKind::Sym(sym) => Some(sym.data.clone()),
            SexpKind::Lang(_) => todo!(),
            SexpKind::MissingArg => todo!(),
            _ => None,
        }
    }

    fn find_locals(&mut self, sexp: &Sexp) {
        let SexpKind::Lang(sexp) = &sexp.kind else {
            return;
        };

        let lang::Target::Sym(target) = &sexp.target else {
            return;
        };

        match target.data.as_str() {
            "=" | "<-" => {
                let var = self.get_assigned_var(&sexp.args[0].data);
                if let Some(var) = var {
                    self.localenv.insert(var);
                }
            }
            "for" => todo!(),
            "delayedAssign" => todo!(),
            "assign" => todo!(),
            "function" => (),
            "~" => todo!(),
            "local" => todo!(),
            "expression" => todo!(),
            "quote" => todo!(),
            _ => (),
        }

        for arg in &sexp.args {
            self.find_locals(&arg.data);
        }
    }
}

pub struct CompilerOptions {
    inline_level: usize,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self { inline_level: 2 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{BufWriter, Read, Write};

    use crate::rds::{rds_reader::RDSReader, rds_writer::RDSWriter, RDSResult};

    macro_rules! test_fun_noopt {
        ( $name:ident, $code:expr) => {
            mod $name {
                use super::*;

                #[test]
                fn compiler() {
                    let path = format!("temp/{}_compiler.dat", stringify!($name));
                    let path = path.as_str();
                    let path_comp = format!("temp/{}_compiler_corr.dat", stringify!($name));
                    let path_comp = path_comp.as_str();
                    let mut command = std::process::Command::new("./create_serdata.R")
                        .args(["-d", $code, path])
                        .spawn()
                        .unwrap();
                    assert!(command.wait().unwrap().success());
                    let mut command = std::process::Command::new("./create_serdata.R")
                        .args([
                            "-d",
                            format!("compiler::cmpfun({}, options=list(optimize=0))", $code)
                                .as_str(),
                            path_comp,
                        ])
                        .spawn()
                        .unwrap();
                    assert!(command.wait().unwrap().success());

                    let mut file = std::fs::File::open(path).unwrap();
                    let RDSResult {
                        header,
                        data: input,
                    } = file.read_rds().unwrap();

                    let mut input_vec = vec![];
                    let mut file = std::fs::File::open(path_comp).unwrap();
                    file.read_to_end(&mut input_vec).unwrap();

                    let mut file = std::fs::File::open(path_comp).unwrap();
                    let RDSResult {
                        header: _,
                        data: correct,
                    } = file.read_rds().unwrap();

                    println!("{}", input);

                    let mut compiler = Compiler::new_options(0);
                    let SexpKind::Closure(cl) = input.kind else {
                        unreachable!();
                    };
                    let bc = compiler.cmpfun(cl);

                    insta::assert_debug_snapshot!(compiler.warnings);
                    let input: Sexp = bc.into();

                    println!("My compilation:\n{input}\n");
                    println!("Correct compilation:\n{correct}");

                    let outdata: Vec<u8> = vec![];
                    let mut writer = BufWriter::new(outdata);
                    writer.write_rds(header, input).unwrap();
                    writer.flush().unwrap();

                    assert_eq!(writer.get_ref(), &input_vec);
                    std::fs::remove_file(path).unwrap();
                    std::fs::remove_file(path_comp).unwrap();
                }
            }
        };
    }

    macro_rules! test_fun_default {
        ( $name:ident, $code:expr) => {
            mod $name {
                use super::*;

                #[test]
                fn compiler() {
                    let path = format!("temp/{}_compiler.dat", stringify!($name));
                    let path = path.as_str();
                    let path_comp = format!("temp/{}_compiler_corr.dat", stringify!($name));
                    let path_comp = path_comp.as_str();
                    let path_env = format!("temp/{}_compiler_env.dat", stringify!($name));
                    let path_env = path_env.as_str();

                    // input data serialized
                    let mut command = std::process::Command::new("./create_serdata.R")
                        .args(["-d", $code, path])
                        .spawn()
                        .unwrap();
                    assert!(command.wait().unwrap().success());

                    // compiled data serialized
                    let mut command = std::process::Command::new("./create_serdata.R")
                        .args([
                            "-d",
                            format!("compiler::cmpfun({})", $code).as_str(),
                            path_comp,
                        ])
                        .spawn()
                        .unwrap();
                    assert!(command.wait().unwrap().success());

                    // base environment
                    let mut command = std::process::Command::new("./baseenv.R")
                        .args([path_env])
                        .spawn()
                        .unwrap();
                    assert!(command.wait().unwrap().success());

                    let mut file = std::fs::File::open(path).unwrap();
                    let RDSResult {
                        header,
                        data: input,
                    } = file.read_rds().unwrap();

                    let mut input_vec = vec![];
                    let mut file = std::fs::File::open(path_comp).unwrap();
                    file.read_to_end(&mut input_vec).unwrap();

                    let mut file = std::fs::File::open(path_comp).unwrap();
                    let RDSResult {
                        header: _,
                        data: correct,
                    } = file.read_rds().unwrap();

                    let mut file = std::fs::File::open(path_env).unwrap();
                    let RDSResult {
                        header: _,
                        data: baseenv,
                    } = file.read_rds().unwrap();

                    let mut file = std::fs::File::open(format!("{path_env}.specials")).unwrap();
                    let RDSResult {
                        header: _,
                        data: specials,
                    } = file.read_rds().unwrap();

                    let mut file = std::fs::File::open(format!("{path_env}.builtins")).unwrap();
                    let RDSResult {
                        header: _,
                        data: builtins,
                    } = file.read_rds().unwrap();

                    let SexpKind::Str(specials) = specials.kind else {
                        unreachable!()
                    };

                    let SexpKind::Str(builtins) = builtins.kind else {
                        unreachable!()
                    };

                    println!("{}", input);

                    let mut compiler = Compiler::new();
                    let SexpKind::Closure(cl) = input.kind else {
                        unreachable!();
                    };
                    let SexpKind::Environment(lang::Environment::Normal(baseenv)) = baseenv.kind
                    else {
                        unreachable!()
                    };

                    compiler.specials = HashSet::from_iter(specials.into_iter());
                    compiler.builtins = HashSet::from_iter(builtins.into_iter());

                    compiler.set_baseenv(baseenv);
                    let bc = compiler.cmpfun(cl);

                    insta::assert_debug_snapshot!(compiler.warnings);
                    let input: Sexp = bc.into();

                    println!("My compilation:\n{input}\n");
                    println!("Correct compilation:\n{correct}");

                    let outdata: Vec<u8> = vec![];
                    let mut writer = BufWriter::new(outdata);
                    writer.write_rds(header, input).unwrap();
                    writer.flush().unwrap();

                    assert_eq!(writer.get_ref(), &input_vec, "Binary are not the same");
                    std::fs::remove_file(path).unwrap();
                    std::fs::remove_file(path_comp).unwrap();
                }
            }
        };
    }

    test_fun_noopt![basic, "function() NULL"];
    test_fun_noopt![basic_real, "function() 1"];
    test_fun_noopt![identity, "function(x) x"];
    test_fun_noopt![
        bool_arg,
        "
        function(f) {
            f(T);
            f(F);
        }"
    ];
    test_fun_noopt![addition, "function(x, y) x + y"];
    test_fun_noopt![
        block,
        "
        function(a, b = 0) {
            x <- c(a, 1);
            x[[b]];
        }"
    ];
    test_fun_noopt![
        fib,
        "
        function(n) {
            a <- 0;
            b <- 1;
            while(n > 0) {
                tmp <- b;
                b <- a + b;
                a <- tmp;
            }
            a
        }"
    ];
    test_fun_noopt![call_lang_target, "function(f, x) f(x)()"];
    test_fun_noopt![call_tag, "function() list(a=1)"];
    test_fun_noopt![if_expression_noopt, "function(x) if (x) 1 else 2"];
    test_fun_noopt![
        while_break,
        "
        function() { 
            while(T) {
                break;
            } 
        }"
    ];
    test_fun_noopt![
        while_break_more,
        "
        function(x) { 
            n <- 0;
            while(T) {
                if (n > x) break;
                n <- n + 1;
            } 
        }"
    ];
    test_fun_noopt![
        basefun_shadowed,
        "
        function(x) {
            list <- function(...) print(1);
            list(x);
        }"
    ];

    test_fun_default![basic_opt, "function() NULL"];
    test_fun_default![basic_real_opt, "function() 1"];
    test_fun_default![identity_opt, "function(x) x"];
    test_fun_default![
        bool_arg_opt,
        "
        function(f) {
            f(T);
            f(F);
        }"
    ];
    test_fun_default![addition_opt, "function(x, y) x + y"];
    test_fun_default![
        block_opt,
        "
        function(a, b = 0) {
            x <- c(a, 1);
            x[[b]];
        }"
    ];
    test_fun_default![
        block02_opt,
        "
        function(x) {
            if (x) 1 else 2;
            if (x) 3 else 4
        }"
    ];
    test_fun_default![
        fib_opt,
        "
        function(n) {
            a <- 0;
            b <- 1;
            while(n > 0) {
                tmp <- b;
                b <- a + b;
                a <- tmp;
            }
            a
        }"
    ];
    test_fun_default![call_lang_target_opt, "function(f, x) f(x)()"];
    test_fun_default![call_tag_opt, "function() list(a=1)"];
    test_fun_default![if_expression, "function(x) if (x) 1 else 2"];
    test_fun_default![set_var_opt, "function(x) {a <- x; a}"];
    test_fun_default![
        while_break_opt,
        "
        function() { 
            while(T) {
                break;
            } 
        }"
    ];
    test_fun_default![
        while_break_more_opt,
        "
        function(x) { 
            n <- 0;
            while(T) {
                if (n > x) break;
                n <- n + 1;
            } 
        }"
    ];
    test_fun_default![
        basefun_shadowed_opt,
        "
        function(x) {
            list <- function(...) print(1);
            list(x);
        }"
    ];

    test_fun_default![tmp, "function() print(1)"];
}
