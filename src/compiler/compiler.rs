use std::{
    cell::UnsafeCell,
    collections::{HashMap, HashSet},
};

use crate::{
    compiler::code_buf::DEFLISTLABEL,
    sexp::{
        bc::{Bc, BcOp, ConstPoolItem},
        sexp::{data, lang, MetaData, Sexp, SexpKind},
        sexp_alloc::Alloc,
    },
};

use super::{
    code_buf::{CodeBuffer, DEFLABEL},
    compiler_context::CompilerContext,
};

#[derive(Debug)]
pub enum Warning {
    VariableDoesNotExist(String),
    NoCasesInSwitch,
    MultipleSwitchDefaults,
    NoLoopContext,
}

struct InlineInfo {
    guard: bool,
    base_var: bool,
}

pub struct Compiler<'a> {
    options: CompilerOptions,
    context: CompilerContext<'a>,
    code_buffer: CodeBuffer<'a>,

    pub warnings: Vec<Warning>,

    env: &'a lang::Environment<'a>,
    localenv: HashSet<&'a str>,
    baseenv: Option<&'a lang::NormalEnv<'a>>,
    namespacebase: Option<&'a lang::NormalEnv<'a>>,

    pub specials: HashSet<&'a str>,
    pub builtins: HashSet<&'a str>,
    pub internals: HashSet<&'a str>,

    arena: &'a Alloc<'a>,
}

const LANG_FUNCS: [&str; 46] = [
    "^", "~", "<", "<<-", "<=", "<-", "=", "==", ">", ">=", "|", "||", "-", ":", "!", "!=", "/",
    "(", "[", "[<-", "[[", "[[<-", "{", "@", "$", "$<-", "*", "&", "&&", "%/%", "%*%", "%%", "+",
    "::", ":::", "@<-", "break", "for", "function", "if", "next", "repeat", "while", "local",
    "return", "switch",
];

const MATH1_FUNCS: [&str; 24] = [
    "floor", "ceiling", "sign", "expm1", "log1p", "cos", "sin", "tan", "acos", "asin", "atan",
    "cosh", "sinh", "tanh", "acosh", "asinh", "atanh", "lgamma", "gamma", "digamma", "trigamma",
    "cospi", "sinpi", "tanpi",
];

const SAFE_BASE_INTERNALS: [&str; 20] = [
    "atan2",
    "besselY",
    "beta",
    "choose",
    "drop",
    "inherits",
    "is.vector",
    "lbeta",
    "lchoose",
    "nchar",
    "polyroot",
    "typeof",
    "vector",
    "which.max",
    "which.min",
    "is.loaded",
    "identical",
    "match",
    "rep.int",
    "rep_len",
];

// This is a hack because from what I have seen I was not able to
// query only internals which have their internal set to builtin
// so these are the values from names.c that I found fall into the
// internal but are not builtin according to is.builtin.internal (10 in eval)
const NON_BUILTIN_INTERNAL: [&str; 5] = ["eapply", "lapply", "vapply", "NextMethod", "rbind"];

impl<'a> Compiler<'a> {
    pub fn new(arena: &'a Alloc<'a>) -> Self {
        Self {
            options: CompilerOptions::default(),
            context: CompilerContext::new_top(&CompilerContext::default()),
            code_buffer: CodeBuffer::new(),

            env: &lang::Environment::Global,
            localenv: HashSet::new(),
            baseenv: None,
            namespacebase: None,

            warnings: vec![],

            specials: HashSet::new(),
            builtins: HashSet::new(),
            internals: HashSet::new(),

            arena,
        }
    }

    #[allow(dead_code)]
    pub fn new_options(inline_level: usize, arena: &'a Alloc<'a>) -> Self {
        Self {
            options: CompilerOptions { inline_level },
            context: CompilerContext::new_top(&CompilerContext::default()),
            code_buffer: CodeBuffer::new(),

            env: &lang::Environment::Global,
            localenv: HashSet::new(),
            baseenv: None,
            namespacebase: None,

            warnings: vec![],

            specials: HashSet::new(),
            builtins: HashSet::new(),
            internals: HashSet::new(),

            arena,
        }
    }

    pub fn set_baseenv(&mut self, env: &'a lang::NormalEnv<'a>) {
        self.baseenv = Some(env);
    }

    pub fn cmpfun(&mut self, closure: &lang::Closure<'a>) -> lang::Closure<'a> {
        let closure = closure;
        let data: Vec<data::TaggedSexp> = closure
            .formals
            .iter()
            .map(|x| data::TaggedSexp::new_with_tag(x.value, &x.name))
            .collect();
        let tmp = data::List {
            data: self.arena.alloc_slice_clone(data.as_slice()),
        };
        let env = self.arena.alloc(lang::NormalEnv::new(
            closure.environment,
            false,
            lang::ListFrame::new(tmp, &mut self.arena),
            lang::HashFrame::new(self.arena.nil_vec, &mut self.arena),
        ));
        let env = self.arena.alloc(lang::Environment::Normal(env));
        self.env = env;
        if self.options.inline_level > 0 {
            self.localenv = HashSet::new();
            self.find_locals(&closure.body);
        }
        let body = self
            .arena
            .alloc(SexpKind::Bc(self.gen_code(closure.body, Some(closure.body.into()))).into());
        //closure.body = body;
        //closure
        lang::Closure::new(closure.formals, body, closure.environment)
    }

    fn gen_code(&mut self, target: &'a Sexp<'a>, loc: Option<ConstPoolItem<'a>>) -> Bc<'a> {
        let tmp = if let Some(loc) = loc {
            CodeBuffer::new_with_expr(loc.clone())
        } else {
            CodeBuffer::new()
        };
        let orig = std::mem::replace(&mut self.code_buffer, tmp);
        self.code_buffer.add_const(target.into());
        self.cmp(&target, false, false);
        self.code_buffer.patch_labels(self.arena);
        let locs = self.create_expression_loc();
        self.code_buffer.add_const(locs.into());
        let tmp = self.code_buffer.create_bc(self.arena);
        let _ = std::mem::replace(&mut self.code_buffer, orig);
        tmp
    }

    /// Default for missing_ok and set_loc in original compiler
    /// is missing_ok = FALSE and set_loc = TRUE
    fn cmp(&mut self, sexp: &'a Sexp<'a>, missing_ok: bool, set_loc: bool) {
        let orig = if set_loc {
            self.code_buffer.set_current_expr(sexp.into())
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
                self.cmp_const(&sexp);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
            }
        };
        self.code_buffer.restore_current_expr(orig);
    }

    fn cmp_const(&mut self, sexp: &'a Sexp<'a>) {
        match &sexp.kind {
            SexpKind::Nil => self.code_buffer.add_instr(BcOp::LDNULL_OP),
            SexpKind::Logic(val) if val.len() == 1 && val[0] == data::Logic::False => {
                self.code_buffer.add_instr(BcOp::LDFALSE_OP)
            }
            SexpKind::Logic(val) if val.len() == 1 && val[0] == data::Logic::True => {
                self.code_buffer.add_instr(BcOp::LDTRUE_OP)
            }
            _ => {
                let ci = self.code_buffer.add_const(sexp.into());
                self.code_buffer.add_instr2(BcOp::LDCONST_OP, ci);
            }
        }
    }

    fn cmp_sym(&mut self, sym: &'a lang::Sym<'a>, missing_ok: bool) {
        match sym.data {
            "..." => self.code_buffer.add_instr(BcOp::DOTSERR_OP),
            name if name.starts_with("..") && name[2..].parse::<usize>().is_ok() => {
                let index = self.code_buffer.add_const(sym.into());
                if missing_ok {
                    self.code_buffer.add_instr2(BcOp::DDVAL_MISSOK_OP, index)
                } else {
                    self.code_buffer.add_instr2(BcOp::DDVAL_OP, index)
                }
            }
            name => {
                if !self.var_exist(name) {
                    self.warnings
                        .push(Warning::VariableDoesNotExist(name.to_string()))
                }
                let index = self.code_buffer.add_const(sym.into());
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

    fn cmp_call(&mut self, call: &'a lang::Lang<'a>, inline_ok: bool) {
        // set up state
        let mut sexp_tmp = std::mem::take(&mut self.code_buffer.current_expr);
        let orig = self.code_buffer.set_current_expr(call.into());
        let tmp = CompilerContext::new_call(&self.context, call);
        let mut orig_context = std::mem::replace(&mut self.context, tmp);

        if inline_ok && self.try_inline(call) {
            return;
        }

        match &call.target {
            lang::Target::Lang(lang) => {
                let orig_tailcall = self.context.tailcall;
                self.context.tailcall = false;

                // originally this
                //self.cmp(&lang.as_ref().clone().into(), false, true);
                let orig = self.code_buffer.set_current_expr((*lang).into());
                self.cmp_call(lang, true);
                self.code_buffer.restore_current_expr(orig);

                self.code_buffer.add_instr(BcOp::CHECKFUN_OP);
                self.cmp_args(&call.args);
                let index = self.code_buffer.add_const_lang(call);
                self.code_buffer.add_instr2(BcOp::CALL_OP, index);

                self.context.tailcall = orig_tailcall;
                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
            }
            lang::Target::Sym(sym) => {
                self.cmp_call_sym_fun(call, sym, &call.args);
            }
        };

        // restore state
        self.code_buffer.restore_current_expr(orig);
        std::mem::swap(&mut self.context, &mut orig_context);
        std::mem::swap(&mut self.code_buffer.current_expr, &mut sexp_tmp);
    }

    fn cmp_call_sym_fun(
        &mut self,
        orig: &'a lang::Lang<'a>,
        sym: &'a lang::Sym<'a>,
        args: &[data::TaggedSexp<'a>],
    ) {
        let index = self.code_buffer.add_const(sym.into());
        self.code_buffer.add_instr2(BcOp::GETFUN_OP, index);
        self.cmp_args(args);
        let index = self.code_buffer.add_const(orig.into());
        self.code_buffer.add_instr2(BcOp::CALL_OP, index);
        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }
    }

    fn cmp_args(&mut self, args: &[data::TaggedSexp<'a>]) {
        let tmp = CompilerContext::new_promise(&self.context);
        let mut orig_context = std::mem::replace(&mut self.context, tmp);

        for arg in args.into_iter() {
            match &arg.data.kind {
                SexpKind::MissingArg => {
                    self.code_buffer.add_instr(BcOp::DOMISSING_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Sym(sym) if sym.data == "..." => {
                    self.code_buffer.add_instr(BcOp::DODOTS_OP);
                }
                SexpKind::Bc(_) => todo!(),
                SexpKind::Promise {
                    environment: _,
                    expr: _,
                    value: _,
                } => todo!(),
                SexpKind::Sym(_) | SexpKind::Lang(_) => {
                    let curr = self.code_buffer.current_expr.clone();
                    let code = self
                        .arena
                        .alloc(SexpKind::Bc(self.gen_code(&arg.data, curr)).into())
                        as &'a Sexp<'a>;
                    let index = self.code_buffer.add_const(code.into());
                    self.code_buffer.add_instr2(BcOp::MAKEPROM_OP, index);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Nil => {
                    self.code_buffer.add_instr(BcOp::PUSHNULLARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Logic(logs)
                    if logs.len() == 1 && matches!(logs[0], data::Logic::True) =>
                {
                    self.code_buffer.add_instr(BcOp::PUSHTRUEARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Logic(logs)
                    if logs.len() == 1 && matches!(logs[0], data::Logic::False) =>
                {
                    self.code_buffer.add_instr(BcOp::PUSHFALSEARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                _ => {
                    let index = self.code_buffer.add_const(arg.data.into());
                    self.code_buffer.add_instr2(BcOp::PUSHCONSTARG_OP, index);
                    self.cmp_tag(&arg.tag);
                }
            }
        }

        std::mem::swap(&mut self.context, &mut orig_context);
    }

    fn cmp_tag(&mut self, tag: &Option<&'a lang::Sym<'a>>) {
        if let Some(tag) = tag {
            let index = self.code_buffer.add_const((*tag).into());
            self.code_buffer.add_instr2(BcOp::SETTAG_OP, index);
        }
    }

    fn var_exist(&self, name: &str) -> bool {
        self.env.find_local_var(name).is_some()
    }

    fn create_expression_loc(&mut self) -> &'a Sexp<'a> {
        let ints = self
            .arena
            .alloc_slice_copy(self.code_buffer.expression_buffer.as_slice());
        self.code_buffer.expression_buffer = Vec::default();

        let string = self
            .arena
            .alloc_slice_copy(&[self.arena.alloc_str("expressionsIndex") as &'a str]);
        let tag = self
            .arena
            .alloc(lang::Sym::new(self.arena.alloc_str("class")))
            as &'a lang::Sym<'a>;
        let data = self.arena.alloc(Sexp {
            kind: SexpKind::Str(data::RVec::new(string)),
            metadata: MetaData {
                attr: UnsafeCell::new(None),
            },
        });
        let data = self
            .arena
            .alloc_slice_fill_with(1, |_| data::TaggedSexp::new_with_tag(data, tag));
        let attr = data::List { data };
        let attr = self.arena.alloc(SexpKind::List(attr).into());
        self.arena.alloc(Sexp {
            kind: SexpKind::Int(data::RVec::new(ints)),
            metadata: MetaData {
                attr: UnsafeCell::new(Some(attr)),
            },
        })
    }

    fn cmp_prim2(
        &mut self,
        first: &'a Sexp<'a>,
        second: &'a Sexp<'a>,
        full: &'a lang::Lang<'a>,
        op: BcOp,
    ) {
        if full.args.len() != 2 || self.dots_or_missing(&full.args) {
            self.cmp_builtin(full, false);
            return;
        }
        let taicall = self.context.tailcall;
        self.context.tailcall = false;
        self.cmp(first, false, true);
        self.context.tailcall = taicall;

        let tmp = CompilerContext::new_arg(&self.context);
        let mut orig = std::mem::replace(&mut self.context, tmp);

        self.cmp(second, false, true);

        let index = self.code_buffer.add_const(full.into());
        self.code_buffer.add_instr2(op, index);

        std::mem::swap(&mut self.context, &mut orig);

        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }
    }

    fn cmp_prim1(&mut self, arg: &'a Sexp<'a>, full: &'a lang::Lang<'a>, op: BcOp) {
        if full.args.len() != 1 || self.dots_or_missing(&full.args) {
            self.cmp_builtin(full, false);
            return;
        }
        let taicall = self.context.tailcall;
        self.context.tailcall = false;
        self.cmp(arg, false, true);
        self.context.tailcall = taicall;

        let index = self.code_buffer.add_const(full.into());
        self.code_buffer.add_instr2(op, index);

        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }
    }

    fn any_dots(&self, sexp: &lang::Lang) -> bool {
        for args in sexp.args.iter() {
            match &args.data.kind {
                SexpKind::Sym(sym) if sym.data == "..." => return true,
                _ => (),
            }
        }
        return false;
    }

    fn cmp_special(&mut self, sexp: &'a lang::Lang<'a>) -> bool {
        let index = self.code_buffer.add_const(sexp.into());
        self.code_buffer.add_instr2(BcOp::CALLSPECIAL_OP, index);
        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }
        true
    }

    // missing_ok default is true
    fn cmp_dispatch(
        &mut self,
        start: BcOp,
        end: BcOp,
        expr: &'a lang::Lang<'a>,
        missing_ok: bool,
    ) -> bool {
        if (missing_ok && self.any_dots(expr))
            || (!missing_ok && self.dots_or_missing(&expr.args))
            || expr.args.len() == 0
        {
            self.cmp_special(expr)
        } else if matches!(expr.args[0].data.kind, SexpKind::MissingArg) {
            self.cmp_special(expr)
        } else {
            let tmp = CompilerContext::new_arg(&self.context);
            let orig = std::mem::replace(&mut self.context, tmp);

            self.cmp(&expr.args[0].data, false, true);

            let _ = std::mem::replace(&mut self.context, orig);

            let index = self.code_buffer.add_const(expr.into());
            let end_label = self.code_buffer.make_label();
            self.code_buffer.add_instr_n(start, &[index, DEFLABEL]);
            self.code_buffer.set_label(end_label);

            if expr.args.len() > 1 {
                self.cmp_builtin_args(&expr.args[1..], missing_ok);
            }

            self.code_buffer.add_instr(end);
            self.code_buffer.put_label(end_label);

            if self.context.tailcall {
                self.code_buffer.add_instr(BcOp::RETURN_OP);
            }

            true
        }
    }

    fn cmp_subset_dispatch(
        &mut self,
        start: BcOp,
        end: BcOp,
        rank: bool,
        expr: &'a lang::Lang<'a>,
    ) -> bool {
        let tmp = CompilerContext::new_arg(&self.context);
        let orig = std::mem::replace(&mut self.context, tmp);

        let index = self.code_buffer.add_const(expr.into());
        let label = self.code_buffer.make_label();

        // compile target
        self.cmp(&expr.args[0].data, false, true);

        self.code_buffer.add_instr_n(start, &[index, DEFLABEL]);
        self.code_buffer.set_label(label);

        // TODO check with Filip since in the original code is some weird R
        self.cmp_indicies(&expr.args[1..]);

        if rank {
            self.code_buffer
                .add_instr_n(end, &[index, (expr.args.len() - 1) as i32])
        } else {
            self.code_buffer.add_instr2(end, index)
        }

        self.code_buffer.put_label(label);

        let _ = std::mem::replace(&mut self.context, orig);

        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }

        true
    }

    fn cmp_is(&mut self, op: BcOp, sexp: &'a lang::Lang<'a>) -> bool {
        if self.any_dots(sexp) || sexp.args.len() != 1 {
            self.cmp_builtin(sexp, false)
        } else {
            let tmp = CompilerContext::new_arg(&self.context);
            let orig = std::mem::replace(&mut self.context, tmp);

            self.cmp(&sexp.args[0].data, false, true);

            let _ = std::mem::replace(&mut self.context, orig);

            self.code_buffer.add_instr(op);

            if self.context.tailcall {
                self.code_buffer.add_instr(BcOp::RETURN_OP);
            }
            true
        }
    }

    fn cmp_assign(
        &mut self,
        lhs: &'a Sexp<'a>,
        value: &'a Sexp<'a>,
        orig: &'a lang::Lang<'a>,
        super_assign: bool,
    ) -> bool {
        let Some(sym) = self.get_assigned_var(lhs) else {
            return self.cmp_special(orig);
        };
        if super_assign && self.find_any_var(sym.data).is_none() {}
        match &lhs.kind {
            SexpKind::Sym(_) => {
                let tailcall = self.context.tailcall;
                self.context.tailcall = false;
                self.cmp(value, false, true);
                self.context.tailcall = tailcall;

                let index = self.code_buffer.add_const(lhs.into());
                if super_assign {
                    self.code_buffer.add_instr2(BcOp::SETVAR2_OP, index);
                } else {
                    self.code_buffer.add_instr2(BcOp::SETVAR_OP, index);
                }
                true
            }
            SexpKind::Lang(lang) => self.cmp_complex_assign(sym, lang, value, orig, super_assign),
            _ => self.cmp_special(orig),
        }
    }

    fn flatten_place(
        &self,
        lhs: &'a lang::Lang<'a>,
    ) -> Vec<(&'a lang::Lang<'a>, &'a lang::Lang<'a>)> {
        let mut orig_places = vec![];
        let mut places = vec![];

        let mut acc = lhs;

        loop {
            orig_places.push(acc);
            let data = self.arena.alloc_slice_clone(&acc.args);
            data[0] = data::TaggedSexp::new(self.arena.tmp_var);
            let n_place = self
                .arena
                .alloc(lang::Lang::new(acc.target.clone(), data::List { data }));
            places.push(n_place as &lang::Lang<'a>);
            match &acc.args[0].data.kind {
                SexpKind::Lang(lang) => acc = lang,
                SexpKind::Sym(_) => break,
                _ => {
                    unreachable!()
                }
            }
        }

        places.into_iter().zip(orig_places.into_iter()).collect()
    }

    fn cmp_complex_assign(
        &mut self,
        sym: &'a lang::Sym<'a>,
        lhs: &'a lang::Lang<'a>,
        value: &'a Sexp<'a>,
        _orig: &'a lang::Lang<'a>,
        super_assign: bool,
    ) -> bool {
        let (start_op, end_op) = if super_assign {
            (BcOp::STARTASSIGN2_OP, BcOp::ENDASSIGN2_OP)
        } else {
            if !self.var_exist(sym.data) {
                self.warnings
                    .push(Warning::VariableDoesNotExist(sym.data.to_string()))
            }
            (BcOp::STARTASSIGN_OP, BcOp::ENDASSIGN_OP)
        };
        if !self.context.top_level {
            self.code_buffer.add_instr(BcOp::INCLNKSTK_OP)
        }

        let tailcall = self.context.tailcall;
        self.context.tailcall = false;
        self.cmp(value, false, true);
        self.context.tailcall = tailcall;

        let sym_index = self.code_buffer.add_const_sym(sym.into());

        self.code_buffer.add_instr2(start_op, sym_index);

        let tmp = CompilerContext::new_arg(&self.context);
        let orig = std::mem::replace(&mut self.context, tmp);

        let places = self.flatten_place(lhs);
        for (place, orig_place) in places[1..].iter().rev() {
            self.cmp_getter_call(place, orig_place)
        }
        self.cmp_setter_call(places[0].0, places[0].1, value);
        for (place, orig_place) in &places[1..] {
            self.cmp_setter_call(place, orig_place, self.arena.vtmp_var);
        }

        self.code_buffer.add_instr2(end_op, sym_index);
        _ = std::mem::replace(&mut self.context, orig);

        if !self.context.top_level {
            self.code_buffer.add_instr(BcOp::DECLNKSTK_OP);
        }

        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::INVISIBLE_OP);
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }

        true
    }

    fn cmp_getter_call(&mut self, place: &'a lang::Lang<'a>, orig_place: &'a lang::Lang<'a>) {
        let tmp = CompilerContext::new_call(&self.context, place);
        let orig = std::mem::replace(&mut self.context, tmp);

        assert!(place.args.len() >= 1);

        let curr_loc = self.code_buffer.set_current_expr(orig_place.into());

        match &place.target {
            lang::Target::Sym(sym) => {
                if !self.try_getter_inline(place) {
                    let sym_index = self.code_buffer.add_const_sym(sym.into());
                    self.code_buffer.add_instr2(BcOp::GETFUN_OP, sym_index);
                    self.code_buffer.add_instr(BcOp::PUSHNULLARG_OP);
                    self.cmp_args(&place.args);
                    let index = self.code_buffer.add_const_lang(place.into());
                    self.code_buffer.add_instr2(BcOp::GETTER_CALL_OP, index);
                    self.code_buffer.add_instr(BcOp::SWAP_OP);
                }
            }
            lang::Target::Lang(lang) => {
                self.cmp_call(lang, true);
                self.code_buffer.add_instr(BcOp::CHECKFUN_OP);
                self.code_buffer.add_instr(BcOp::PUSHNULLARG_OP);
                self.cmp_args(&place.args);
                let index = self.code_buffer.add_const_lang(place.into());
                self.code_buffer.add_instr2(BcOp::GETTER_CALL_OP, index);
                self.code_buffer.add_instr(BcOp::SWAP_OP);
            }
        }

        self.code_buffer.restore_current_expr(curr_loc);
        _ = std::mem::replace(&mut self.context, orig);
    }

    // most basic getter inline handler
    fn try_getter_inline(&mut self, expr: &'a lang::Lang<'a>) -> bool {
        let sym = match &expr.target {
            lang::Target::Lang(_) => return false,
            lang::Target::Sym(s) => s.data,
        };

        let info = self.get_inlineinfo(sym);

        if info.is_none() {
            return false;
        }
        match sym {
            "$" => {
                if self.any_dots(expr) || expr.args.len() != 2 {
                    return false;
                }
                let SexpKind::Sym(sym) = &expr.args[1].data.kind else {
                    return false;
                };
                let call_index = self.code_buffer.add_const_lang(expr.into());
                let sym_index = self.code_buffer.add_const_sym(sym.into());
                self.code_buffer.add_instr(BcOp::DUP2ND_OP);
                self.code_buffer
                    .add_instr_n(BcOp::DOLLAR_OP, &[call_index, sym_index]);
                self.code_buffer.add_instr(BcOp::SWAP_OP);
                true
            }
            "[[" => {
                if self.dots_or_missing(&expr.args)
                    || !self.names(&expr.args).is_empty()
                    || expr.args.len() < 2
                {
                    return self.cmp_getter_dispatch(
                        BcOp::STARTSUBSET2_OP,
                        BcOp::DFLTSUBSET2_OP,
                        expr,
                    );
                }
                let nidx = expr.args.len() - 1;
                let (code, rank) = match nidx {
                    1 => (BcOp::VECSUBSET2_OP, false),
                    2 => (BcOp::MATSUBSET2_OP, false),
                    _ => (BcOp::SUBSET2_N_OP, true),
                };
                self.cmp_subset_getter_dispatch(BcOp::STARTSUBSET2_N_OP, code, rank, expr)
            }
            "[" => {
                if self.dots_or_missing(&expr.args)
                    || !self.names(&expr.args).is_empty()
                    || expr.args.len() < 2
                {
                    return self.cmp_getter_dispatch(
                        BcOp::STARTSUBSET_OP,
                        BcOp::DFLTSUBSET_OP,
                        expr,
                    );
                }
                let nidx = expr.args.len() - 1;
                let (code, rank) = match nidx {
                    1 => (BcOp::VECSUBSET_OP, false),
                    2 => (BcOp::MATSUBSET_OP, false),
                    _ => (BcOp::SUBSET_N_OP, true),
                };
                self.cmp_subset_getter_dispatch(BcOp::STARTSUBSET_N_OP, code, rank, expr)
            }
            _ => false,
        }
    }

    fn cmp_getter_dispatch(&mut self, start: BcOp, end: BcOp, call: &'a lang::Lang<'a>) -> bool {
        if self.any_dots(call) {
            return false;
        }
        let call_index = self.code_buffer.add_const_lang(call.into());
        let end_label = self.code_buffer.make_label();
        self.code_buffer.add_instr(BcOp::DUP2ND_OP);
        self.code_buffer.add_instr_n(start, &[call_index, DEFLABEL]);
        self.code_buffer.set_label(end_label);
        if call.args.len() > 1 {
            self.cmp_builtin_args(&call.args[1..], true);
        }
        self.code_buffer.add_instr(end);
        self.code_buffer.put_label(end_label);
        self.code_buffer.add_instr(BcOp::SWAP_OP);
        true
    }

    fn cmp_subset_getter_dispatch(
        &mut self,
        start: BcOp,
        end: BcOp,
        rank: bool,
        call: &'a lang::Lang<'a>,
    ) -> bool {
        if self.dots_or_missing(&call.args)
            || !self.names(&call.args).is_empty()
            || call.args.len() < 2
        {
            panic!()
        }

        let call_index = self.code_buffer.add_const_lang(call);
        let label = self.code_buffer.make_label();
        self.code_buffer.add_instr(BcOp::DUP2ND_OP);
        self.code_buffer.add_instr_n(start, &[call_index, DEFLABEL]);
        self.code_buffer.set_label(label);

        self.cmp_indicies(&call.args[1..]);

        if rank {
            self.code_buffer
                .add_instr_n(end, &[call_index, (call.args.len() - 1) as i32])
        } else {
            self.code_buffer.add_instr2(end, call_index);
        }
        self.code_buffer.put_label(label);
        self.code_buffer.add_instr(BcOp::SWAP_OP);

        true
    }

    fn append_tagged_expr(
        &self,
        target: lang::Target<'a>,
        args: &data::List<'a>,
        tagged: data::TaggedSexp<'a>,
    ) -> &'a lang::Lang<'a> {
        let data = self
            .arena
            .alloc_slice_fill_with(args.len() + 1, |_| data::TaggedSexp::new(self.arena.nil));
        for (i, val) in args.iter().cloned().chain([tagged].into_iter()).enumerate() {
            data[i] = val
        }
        let args = data::List { data };
        let acall = self.arena.alloc(lang::Lang::new(target, args));
        acall
    }

    fn cmp_setter_call(
        &mut self,
        place: &'a lang::Lang<'a>,
        orig_place: &'a lang::Lang<'a>,
        value_expr: &'a Sexp<'a>,
    ) {
        let Some(afun) = self.get_assign_fun(&place.target) else {
            unreachable!()
        };

        let acall = self.append_tagged_expr(
            afun.clone(),
            &place.args,
            data::TaggedSexp::new_with_tag(value_expr, self.arena.value_sym),
        );

        let tmp = CompilerContext::new_call(&self.context, acall);
        let orig = std::mem::replace(&mut self.context, tmp);

        let cexpr = self.append_tagged_expr(
            afun.clone(),
            &orig_place.args,
            data::TaggedSexp::new_with_tag(value_expr, self.arena.value_sym),
        );
        let sloc = self.code_buffer.set_current_expr(cexpr.into());

        match afun {
            lang::Target::Sym(sym) => {
                if !self.try_setter_inline(sym.clone(), place, orig_place, acall) {
                    let sym: &'a lang::Sym<'a> = self.arena.alloc(sym);
                    let sym_index = self.code_buffer.add_const_sym(sym.into());
                    self.code_buffer.add_instr2(BcOp::GETFUN_OP, sym_index);
                    self.code_buffer.add_instr(BcOp::PUSHNULLARG_OP);
                    self.cmp_args(&place.args[1..]);
                    let call_index = self.code_buffer.add_const_lang(acall.into());
                    let value_index = self.code_buffer.add_const_sexp(value_expr.into());
                    self.code_buffer
                        .add_instr_n(BcOp::SETTER_CALL_OP, &[call_index, value_index]);
                }
            }
            lang::Target::Lang(lang) => {
                self.cmp_call(lang, true);

                self.code_buffer.add_instr(BcOp::CHECKFUN_OP);
                self.code_buffer.add_instr(BcOp::PUSHNULLARG_OP);
                self.cmp_args(&place.args[1..]);

                let call_index = self.code_buffer.add_const_lang(acall.into());
                let value_index = self.code_buffer.add_const_sexp(value_expr.into());
                self.code_buffer
                    .add_instr_n(BcOp::SETTER_CALL_OP, &[call_index, value_index]);
            }
        }

        self.code_buffer.restore_current_expr(sloc);

        let _ = std::mem::replace(&mut self.context, orig);
    }

    fn try_setter_inline(
        &mut self,
        afun: lang::Sym<'a>,
        place: &'a lang::Lang<'a>,
        orig_place: &'a lang::Lang<'a>,
        call: &'a lang::Lang<'a>,
    ) -> bool {
        let info = self.get_inlineinfo(afun.data);

        if info.is_none() {
            return false;
        }

        match afun.data {
            "$<-" => {
                if self.any_dots(place) || place.args.len() != 2 {
                    return false;
                }
                let SexpKind::Sym(sym) = &place.args[1].data.kind else {
                    return false;
                };
                let call_index = self.code_buffer.add_const_lang(call.into());
                let sym_index = self.code_buffer.add_const_sym(sym.into());
                self.code_buffer
                    .add_instr_n(BcOp::DOLLARGETS_OP, &[call_index, sym_index]);
                true
            }
            "[<-" => {
                if self.dots_or_missing(&place.args) || place.args.len() < 2 {
                    self.cmp_setter_dispatch(
                        BcOp::STARTSUBASSIGN_OP,
                        BcOp::DFLTSUBASSIGN_OP,
                        place,
                        call,
                    )
                } else {
                    let nidx = place.args.len() - 1;
                    let (code, rank) = match nidx {
                        1 => (BcOp::VECSUBASSIGN_OP, false),
                        2 => (BcOp::MATSUBASSIGN_OP, false),
                        _ => (BcOp::SUBASSIGN_N_OP, true),
                    };
                    self.cmp_subassign_dispatch(
                        BcOp::STARTSUBASSIGN_N_OP,
                        code,
                        rank,
                        afun,
                        place,
                        call,
                    )
                }
            }
            "[[<-" => {
                if self.dots_or_missing(&place.args) || place.args.len() < 2 {
                    self.cmp_setter_dispatch(
                        BcOp::STARTSUBASSIGN2_OP,
                        BcOp::DFLTSUBASSIGN2_OP,
                        place,
                        call,
                    )
                } else {
                    let nidx = place.args.len() - 1;
                    let (code, rank) = match nidx {
                        1 => (BcOp::VECSUBASSIGN2_OP, false),
                        2 => (BcOp::MATSUBASSIGN2_OP, false),
                        _ => (BcOp::SUBASSIGN2_N_OP, true),
                    };
                    self.cmp_subassign_dispatch(
                        BcOp::STARTSUBASSIGN2_N_OP,
                        code,
                        rank,
                        afun,
                        place,
                        call,
                    )
                }
            }
            _ => false,
        }
    }

    fn cmp_setter_dispatch(
        &mut self,
        start_op: BcOp,
        dflt_op: BcOp,
        place: &'a lang::Lang<'a>,
        call: &'a lang::Lang<'a>,
    ) -> bool {
        if self.any_dots(place) {
            return false;
        }

        let call_index = self.code_buffer.add_const_lang(call.into());
        let end_label = self.code_buffer.make_label();
        self.code_buffer
            .add_instr_n(start_op, &[call_index, DEFLABEL]);
        self.code_buffer.set_label(end_label);
        if place.args.len() > 1 {
            self.cmp_builtin_args(&place.args[1..], true)
        }
        self.code_buffer.add_instr(dflt_op);
        self.code_buffer.put_label(end_label);
        true
    }

    fn cmp_subassign_dispatch(
        &mut self,
        start_op: BcOp,
        code: BcOp,
        rank: bool,
        afun: lang::Sym<'a>,
        place: &'a lang::Lang<'a>,
        call: &'a lang::Lang<'a>,
    ) -> bool {
        if self.dots_or_missing(&place.args) || place.args.len() < 2 {
            // ohno
            panic!()
        } else {
            let call_index = self.code_buffer.add_const_lang(call.into());
            let label = self.code_buffer.make_label();
            self.code_buffer
                .add_instr_n(start_op, &[call_index, DEFLABEL]);
            self.code_buffer.set_label(label);
            self.cmp_indicies(&place.args[1..]);
            if rank {
                self.code_buffer
                    .add_instr_n(code, &[call_index, (place.args.len() - 1) as i32]);
            } else {
                self.code_buffer.add_instr2(code, call_index);
            }
            self.code_buffer.put_label(label);
            true
        }
    }

    fn cmp_indicies(&mut self, indicies: &'a [data::TaggedSexp<'a>]) {
        for idx in indicies {
            self.cmp(&idx.data, true, true);
        }
    }

    fn dots_or_missing(&self, args: &'a data::List<'a>) -> bool {
        for arg in args.into_iter() {
            match &arg.data.kind {
                SexpKind::Sym(sym) if sym.data == "..." => return true,
                _ if self.missing(&arg.data) => return true,
                _ => (),
            }
        }
        false
    }

    fn is_builtin_internal(&self, name: &'a str) -> bool {
        self.internals.contains(name) && !NON_BUILTIN_INTERNAL.contains(&name)
    }

    fn try_inline(&mut self, expr: &'a lang::Lang<'a>) -> bool {
        let sym = match &expr.target {
            lang::Target::Lang(_) => return false,
            lang::Target::Sym(s) => s.data,
        };

        let info = self.get_inlineinfo(sym);

        if info.is_none() {
            return false;
        }

        let info = info.unwrap();

        if info.guard {
            let tailcall = self.context.tailcall;
            self.context.tailcall = false;
            let expr_index = self.code_buffer.add_const(expr.into());
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

    fn handle_inline(&mut self, sym: &'a str, expr: &'a lang::Lang<'a>, info: InlineInfo) -> bool {
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

                let call_idx = self.code_buffer.add_const(expr.into());
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
                    self.cmp(self.arena.nil, false, true);
                    return true;
                }

                let orig_loc = std::mem::replace(&mut self.code_buffer.current_expr, None);

                let tailcall = self.context.tailcall;
                self.context.tailcall = false;
                for inner in &expr.args[0..(expr.args.len() - 1)] {
                    self.code_buffer.set_current_expr(inner.data.into());
                    self.cmp(&inner.data, false, false);
                    self.code_buffer.set_current_expr(inner.data.into());
                    self.code_buffer.add_instr(BcOp::POP_OP);
                }
                self.context.tailcall = tailcall;

                self.code_buffer
                    .set_current_expr(expr.args.last().unwrap().data.into());
                self.cmp(&expr.args.last().unwrap().data, false, false);

                self.code_buffer.restore_current_expr(orig_loc);

                true
            }
            "(" => {
                if self.any_dots(expr) {
                    self.cmp_builtin(expr, false)
                } else if expr.args.len() != 1 {
                    // here should be a warning
                    self.cmp_builtin(expr, false)
                } else if self.context.tailcall {
                    self.context.tailcall = false;
                    self.cmp(&expr.args[0].data, false, true);
                    self.code_buffer.add_instr(BcOp::VISIBLE_OP);
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                    self.context.tailcall = true;
                    true
                } else {
                    self.cmp(&expr.args[0].data, false, true);
                    true
                }
            }
            "<-" => self.cmp_assign(&expr.args[0].data, &expr.args[1].data, expr, false),
            "<<-" => self.cmp_assign(&expr.args[0].data, &expr.args[1].data, expr, true),
            /*
            "<-" if expr.args.len() == 2
                && false
                && matches!(&expr.args[0].data.kind, SexpKind::Lang(_)) =>
            {
                if !self.context.top_level {
                    self.code_buffer.add_instr(BcOp::INCLNKSTK_OP);
                }
                // start op = STARTASSIGN.OP
                // end op = ENDASSIGN.OP
                let tailcall = self.context.tailcall;
                self.context.tailcall = false;
                self.cmp(expr.args[1].data, false, true);
                self.context.tailcall = tailcall;

                let sym = self
                    .get_assigned_var(&expr.args[0].data)
                    .expect("Target of the assign cannot be resoved");

                let index = self.code_buffer.add_const_sym(sym);
                self.code_buffer.add_instr2(BcOp::STARTASSIGN_OP, index);

                true
            }*/
            "+" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::ADD_OP);
                true
            }
            "-" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::SUB_OP);
                true
            }
            "+" if expr.args.len() == 1 => {
                self.cmp_prim1(&expr.args[0].data, expr, BcOp::UPLUS_OP);
                true
            }
            "-" if expr.args.len() == 1 => {
                self.cmp_prim1(&expr.args[0].data, expr, BcOp::UMINUS_OP);
                true
            }
            "*" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::MUL_OP);
                true
            }
            "/" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::DIV_OP);
                true
            }
            "^" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::EXPT_OP);
                true
            }
            ":" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::COLON_OP);
                true
            }
            "seq_along" if expr.args.len() == 1 => {
                self.cmp_prim1(&expr.args[0].data, expr, BcOp::SEQALONG_OP);
                true
            }
            "seq_len" if expr.args.len() == 1 => {
                self.cmp_prim1(&expr.args[0].data, expr, BcOp::SEQLEN_OP);
                true
            }
            "exp" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::EXP_OP);
                true
            }
            "sqrt" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::SQRT_OP);
                true
            }
            "[[" => {
                if self.dots_or_missing(&expr.args) {
                    self.cmp_dispatch(BcOp::STARTSUBSET2_OP, BcOp::DFLTSUBSET2_OP, expr, true)
                } else {
                    let nidx = expr.args.len() - 1;
                    let (code, rank) = match nidx {
                        1 => (BcOp::VECSUBSET2_OP, false),
                        2 => (BcOp::MATSUBSET2_OP, false),
                        _ => (BcOp::SUBSET2_N_OP, true),
                    };
                    self.cmp_subset_dispatch(BcOp::STARTSUBSET2_N_OP, code, rank, expr)
                }
            }
            "[" => {
                if self.dots_or_missing(&expr.args) {
                    self.cmp_dispatch(BcOp::STARTSUBSET_OP, BcOp::DFLTSUBSET_OP, expr, true)
                } else {
                    let nidx = expr.args.len() - 1;
                    let (code, rank) = match nidx {
                        1 => (BcOp::VECSUBSET_OP, false),
                        2 => (BcOp::MATSUBSET_OP, false),
                        _ => (BcOp::SUBSET_N_OP, true),
                    };
                    self.cmp_subset_dispatch(BcOp::STARTSUBSET_N_OP, code, rank, expr)
                }
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
            "for" => {
                let sym = &expr.args[0].data;
                let seq = &expr.args[1].data;
                let body = &expr.args[2].data;
                if !matches!(sym.kind, SexpKind::Sym(_)) {
                    return false;
                }
                let taicall = self.context.tailcall;
                self.context.tailcall = false;
                self.cmp(seq, false, true);
                self.context.tailcall = taicall;
                let index = self.code_buffer.add_const((*sym).into());
                let call_idx = self.code_buffer.add_const(expr.into());
                if self.check_skip_loopctx(body, true) {
                    self.cmp_for_body(false, call_idx, body, index);
                } else {
                    let orig_ret = self.context.need_returnjmp;
                    self.context.need_returnjmp = true;

                    let ctx_label = self.code_buffer.make_label();
                    self.code_buffer
                        .add_instr_n(BcOp::STARTFOR_OP, &[call_idx, index, DEFLABEL]);
                    self.code_buffer.set_label(ctx_label);

                    self.code_buffer.put_label(ctx_label);

                    let ljmpend_label = self.code_buffer.make_label();
                    self.code_buffer
                        .add_instr_n(BcOp::STARTLOOPCNTXT_OP, &[1, DEFLABEL]);
                    self.code_buffer.set_label(ljmpend_label);

                    self.cmp_for_body(true, call_idx, body, index);

                    self.code_buffer.put_label(ljmpend_label);
                    self.code_buffer.add_instr2(BcOp::ENDLOOPCNTXT_OP, 1);

                    self.context.need_returnjmp = orig_ret;
                }
                self.code_buffer.add_instr(BcOp::ENDFOR_OP);
                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::INVISIBLE_OP);
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
                true
            }
            "switch" => {
                if expr.args.len() < 1 || self.any_dots(expr) {
                    return self.cmp_special(expr);
                }
                self.cmp_switch(expr)
            }
            "==" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::EQ_OP);
                true
            }
            "!=" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::NE_OP);
                true
            }
            "<" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::LT_OP);
                true
            }
            "<=" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::LE_OP);
                true
            }
            ">=" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::GE_OP);
                true
            }
            ">" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::GT_OP);
                true
            }
            "&" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::AND_OP);
                true
            }
            "|" if expr.args.len() == 2 => {
                self.cmp_prim2(&expr.args[0].data, &expr.args[1].data, expr, BcOp::OR_OP);
                true
            }
            "!" if expr.args.len() == 1 => {
                self.cmp_prim1(&expr.args[0].data, expr, BcOp::NOT_OP);
                true
            }
            "&&" => {
                let tmp = CompilerContext::new_arg(&self.context);
                let orig = std::mem::replace(&mut self.context, tmp);

                let index = self.code_buffer.add_const(expr.into());

                let label = self.code_buffer.make_label();
                self.cmp(&expr.args[0].data, false, true);
                self.code_buffer
                    .add_instr_n(BcOp::AND1ST_OP, &[index, DEFLABEL]);
                self.code_buffer.set_label(label);
                self.cmp(&expr.args[1].data, false, true);
                self.code_buffer.add_instr2(BcOp::AND2ND_OP, index);
                self.code_buffer.put_label(label);

                let _ = std::mem::replace(&mut self.context, orig);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
                true
            }
            "||" => {
                let tmp = CompilerContext::new_arg(&self.context);
                let orig = std::mem::replace(&mut self.context, tmp);

                let index = self.code_buffer.add_const(expr.into());

                let label = self.code_buffer.make_label();
                self.cmp(&expr.args[0].data, false, true);
                self.code_buffer
                    .add_instr_n(BcOp::OR1ST_OP, &[index, DEFLABEL]);
                self.code_buffer.set_label(label);
                self.cmp(&expr.args[1].data, false, true);
                self.code_buffer.add_instr2(BcOp::OR2ND_OP, index);
                self.code_buffer.put_label(label);

                let _ = std::mem::replace(&mut self.context, orig);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }
                true
            }
            "::" | ":::" => {
                if self.dots_or_missing(&expr.args) || expr.args.len() != 2 {
                    return false;
                }

                if !matches!(expr.args[0].data.kind, SexpKind::Sym(_))
                    || !matches!(expr.args[1].data.kind, SexpKind::Sym(_))
                {
                    return false;
                }

                let lang::Target::Sym(ref sym) = expr.target else {
                    unreachable!();
                };

                let SexpKind::Sym(ref a) = expr.args[0].data.kind else {
                    unreachable!()
                };
                let SexpKind::Sym(ref b) = expr.args[1].data.kind else {
                    unreachable!()
                };

                let a = data::RVec::new(self.arena.alloc_slice_clone(&[a.data]));
                let b = data::RVec::new(self.arena.alloc_slice_clone(&[b.data]));

                let str_args: [data::TaggedSexp<'a>; 2] = [
                    data::TaggedSexp::new(self.arena.alloc(SexpKind::Str(a).into())),
                    data::TaggedSexp::new(self.arena.alloc(SexpKind::Str(b).into())),
                ];

                self.cmp_call_sym_fun(expr, sym, &str_args);

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
            "next" => match &self.context.loop_ctx {
                Some(loop_ctx) if loop_ctx.goto_ok => {
                    self.code_buffer.add_instr2(BcOp::GOTO_OP, DEFLABEL);
                    self.code_buffer.set_label(loop_ctx.loop_label);
                    true
                }
                _ => self.cmp_special(expr),
            },
            "function" => {
                let forms = &expr.args[0].data;
                let body = &expr.args[1].data;

                let SexpKind::List(forms) = &forms.kind else {
                    return false;
                };

                let tmp = CompilerContext::new_function(&self.context, forms, body);
                let orig = std::mem::replace(&mut self.context, tmp);

                let comp_body = self.gen_code(body, self.code_buffer.get_current_expr());
                let data = self.arena.alloc_slice_copy(&[
                    self.arena.alloc(SexpKind::List(forms.clone()).into()) as &'a Sexp<'a>,
                    self.arena.alloc(SexpKind::Bc(comp_body).into()) as &'a Sexp<'a>,
                    self.arena.nil,
                ]);
                let index = self
                    .code_buffer
                    .add_const_sexp(self.arena.alloc(SexpKind::Vec(data).into()));
                self.code_buffer.add_instr2(BcOp::MAKECLOSURE_OP, index);

                let _ = std::mem::replace(&mut self.context, orig);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }

                true
            }
            "return" if expr.args.len() > 1 || self.dots_or_missing(&expr.args) => {
                self.cmp_special(expr)
            }
            "return" => {
                let mut val: &'a Sexp<'a> = self.arena.nil;
                if expr.args.len() == 1 {
                    val = &expr.args[0].data;
                }

                let tailcall = self.context.tailcall;
                self.context.tailcall = false;
                self.cmp(val, false, true);
                self.context.tailcall = tailcall;

                if self.context.need_returnjmp {
                    self.code_buffer.add_instr(BcOp::RETURNJMP_OP);
                } else {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }

                true
            }
            "$" if expr.args.len() != 2 || self.any_dots(expr) => self.cmp_special(expr),
            "$" if expr.args.len() == 2 => match &expr.args[1].data.kind {
                SexpKind::Sym(_) => {
                    let tmp = CompilerContext::new_arg(&self.context);
                    let orig = std::mem::replace(&mut self.context, tmp);
                    self.cmp(&expr.args[0].data, false, true);
                    let _ = std::mem::replace(&mut self.context, orig);

                    let expr_idx = self.code_buffer.add_const(expr.into());
                    let sym_idx = self.code_buffer.add_const(expr.args[1].data.into());

                    self.code_buffer
                        .add_instr_n(BcOp::DOLLAR_OP, &[expr_idx, sym_idx]);
                    if self.context.tailcall {
                        self.code_buffer.add_instr(BcOp::RETURN_OP);
                    }

                    true
                }
                _ => self.cmp_special(expr),
            },
            "local" if expr.args.len() == 1 => {
                let fun_sym = lang::Sym::new("function".into());
                let fun_sym: lang::Target = fun_sym.into();
                let data = self.arena.alloc_slice_clone(&[
                    data::TaggedSexp::new(self.arena.nil),
                    expr.args[0].clone(),
                    data::TaggedSexp::new(self.arena.nil),
                ]);
                let args = data::List { data };
                let lang = self.arena.alloc(lang::Lang::new(fun_sym, args));
                let target: lang::Target = lang::Target::Lang(lang);
                let lang = self
                    .arena
                    .alloc(lang::Lang::new(target, self.arena.nil_list));

                let orig = self.code_buffer.set_current_expr(ConstPoolItem::Lang(lang));
                self.cmp_call(lang, true);
                self.code_buffer.restore_current_expr(orig);

                true
            }
            "is.character" => self.cmp_is(BcOp::ISCHARACTER_OP, expr),
            "is.complex" => self.cmp_is(BcOp::ISCOMPLEX_OP, expr),
            "is.double" => self.cmp_is(BcOp::ISDOUBLE_OP, expr),
            "is.integer" => self.cmp_is(BcOp::ISINTEGER_OP, expr),
            "is.logical" => self.cmp_is(BcOp::ISLOGICAL_OP, expr),
            "is.name" => self.cmp_is(BcOp::ISSYMBOL_OP, expr),
            "is.null" => self.cmp_is(BcOp::ISNULL_OP, expr),
            "is.object" => self.cmp_is(BcOp::ISOBJECT_OP, expr),
            "is.symbol" => self.cmp_is(BcOp::ISSYMBOL_OP, expr),
            ".Internal" => match &expr.args[0].data.kind {
                SexpKind::Lang(lang) => {
                    let name = if let lang::Target::Sym(sym) = &lang.target {
                        sym
                    } else {
                        return self.cmp_special(expr);
                    };
                    if self.is_builtin_internal(name.data) {
                        self.cmp_builtin(lang, true)
                    } else {
                        return self.cmp_special(expr);
                    }
                }
                _ => self.cmp_special(expr),
            },
            _ if info.base_var && MATH1_FUNCS.contains(&sym) => {
                if self.dots_or_missing(&expr.args) {
                    return self.cmp_builtin(expr, false);
                } else if expr.args.len() != 1 {
                    return self.cmp_builtin(expr, false);
                }

                let math_index = MATH1_FUNCS.iter().position(|x| x == &sym).unwrap() as i32;
                let tmp = CompilerContext::new_arg(&self.context);
                let orig = std::mem::replace(&mut self.context, tmp);

                self.cmp(&expr.args[0].data, false, true);

                let _ = std::mem::replace(&mut self.context, orig);

                let index = self.code_buffer.add_const(expr.into());

                self.code_buffer
                    .add_instr_n(BcOp::MATH1_OP, &[index, math_index]);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }

                true
            }
            ".Call" => {
                // comment in orig says
                // this should match DOTCALL_MAX in eval.c
                let nargsmax = 16;
                // is.null(names(e)) is missing
                if self.dots_or_missing(&expr.args)
                    || expr.args.len() < 1
                    || expr.args.len() > nargsmax + 1
                {
                    return self.cmp_builtin(expr, false);
                }
                let tailcall = self.context.tailcall;
                self.context.tailcall = false;
                self.cmp(expr.args[0].data, false, true);
                self.context.tailcall = tailcall;

                let nargs = expr.args.len() - 1;
                if nargs > 0 {
                    let tmp = CompilerContext::new_arg(&self.context);
                    let orig = std::mem::replace(&mut self.context, tmp);
                    for arg in expr.args[1..].iter() {
                        self.cmp(arg.data, false, true);
                    }

                    _ = std::mem::replace(&mut self.context, orig);
                }

                let index = self.code_buffer.add_const_lang(expr);
                self.code_buffer
                    .add_instr_n(BcOp::DOTCALL_OP, &[index, nargs as i32]);

                if self.context.tailcall {
                    self.code_buffer.add_instr(BcOp::RETURN_OP);
                }

                true
            }
            _ if info.base_var && SAFE_BASE_INTERNALS.contains(&sym) => {
                self.cmp_simple_internal(sym, expr)
            }
            _ if info.base_var && self.builtins.contains(sym) => self.cmp_builtin(expr, false),
            _ if info.base_var && self.specials.contains(sym) => self.cmp_special(expr),
            _ => false,
        }
    }

    fn match_args(
        def: &'a lang::Closure<'a>,
        args: &'a data::List<'a>,
        dots: &[data::TaggedSexp<'a>],
        dots_pos: usize,
    ) -> HashMap<&'a str, data::TaggedSexp<'a>> {
        let mut res: Vec<Option<data::TaggedSexp<'a>>> = vec![];
        res.resize(def.formals.len(), None);

        let mut used: Vec<bool> = vec![];
        used.resize(args.len() + dots.len(), false);

        // exact match
        for (target_index, formal_val) in def.formals.iter().enumerate() {
            if formal_val.name.data == "..." {
                continue;
            }
            for (source_index, arg_val) in args[0..dots_pos]
                .iter()
                .chain(dots.iter())
                .chain(args[dots_pos..].iter())
                .enumerate()
            {
                if Some(&formal_val.name) == arg_val.tag {
                    if used[source_index] {
                        panic!()
                    }
                    used[source_index] = true;
                    res[target_index] = Some(data::TaggedSexp::new_with_tag(
                        &arg_val.data,
                        &formal_val.name,
                    ));
                }
            }
        }

        // partial match
        let mut seendots = false;
        for (target_index, formal_val) in def.formals.iter().enumerate() {
            // after the dots in formal are reached
            // exact match is enfoced
            if formal_val.name.data == "..." {
                seendots = true;
                continue;
            }
            for (source_index, arg_val) in args[0..dots_pos]
                .iter()
                .chain(dots.iter())
                .chain(args[dots_pos..].iter())
                .enumerate()
            {
                if used[source_index] {
                    continue;
                }
                let Some(arg_tag) = arg_val.tag else {
                    continue;
                };
                if formal_val.name.data == arg_tag.data
                    || (!seendots && &formal_val.name.data[0..arg_tag.data.len()] == arg_tag.data)
                {
                    used[source_index] = true;
                    res[target_index] = Some(data::TaggedSexp::new_with_tag(
                        &arg_val.data,
                        &formal_val.name,
                    ));
                }
            }
        }

        // order match
        let mut target_index = 0;
        let mut source_iter = args[0..dots_pos]
            .iter()
            .chain(dots.iter())
            .chain(args[dots_pos..].iter())
            .enumerate()
            .peekable();
        while target_index < def.formals.len() && source_iter.peek().is_some() {
            let (source_index, source_val) = source_iter.peek().unwrap();
            if def.formals[target_index].name.data == "..." {
                break;
            } else if res[target_index].is_some() {
                target_index += 1;
                continue;
            } else if used[*source_index] || source_val.tag.is_some() {
                source_iter.next();
                continue;
            } else {
                let name = &def.formals[target_index].name;
                res[target_index] = Some(data::TaggedSexp::new_with_tag(source_val.data, name));
                used[*source_index] = true;
                target_index += 1;
                source_iter.next();
            }
        }

        // not all implemented
        assert!(used.iter().all(|x| *x));

        //println!("{:?}", res);

        let res: HashMap<&'a str, data::TaggedSexp<'a>> =
            HashMap::from_iter(res.into_iter().filter(|x| x.is_some()).map(|x| {
                let tmp = x.unwrap();
                (tmp.tag.unwrap().data, tmp)
            }));

        return res;
    }

    fn extract_simple_internal(
        &mut self,
        target_sym_str: &'a str,
        expr: &'a lang::Lang<'a>,
        def: &'a lang::Closure<'a>,
    ) -> Option<&'a lang::Lang<'a>> {
        // get body
        let body = def.body()?;
        let body = match &body.kind {
            SexpKind::Lang(lang)
                if matches!(lang.target, lang::Target::Sym(lang::Sym { data: "{" })) =>
            {
                &lang.args[0].data
            }
            _ => body,
        };

        let SexpKind::Lang(lang) = &body.kind else {
            return None;
        };

        // this should always hold since this fully depends on my implementation
        assert!(matches!(
            lang.target,
            lang::Target::Sym(lang::Sym { data: ".Internal" })
        ));

        let icall = lang.args.data[0].data;

        let SexpKind::Lang(ref icall) = icall.kind else {
            unreachable!()
        };

        // formals are formals and actuals are paramenters from expr

        // 1. fixup params
        //    to fixup you need to find dots and substitute them
        //    if you cannot sub them just remove them
        //    to sub them you just check the env and expand them
        // 2. match params to formals

        let mut dots_pos: Option<usize> = None;
        for (index, arg) in expr.args.iter().enumerate() {
            if let Some(sym) = &arg.tag {
                if sym.data == "..." {
                    dots_pos = Some(index);
                    break;
                }
            }
        }

        let dots = self.find_any_var("...");

        let tmp_empty = [];
        // this are parameters with the dots spliced in
        //let actuals: <dyn Iterator<Item = &data::TaggedSexp<'a>>> =
        let (dots_pos, dots): (usize, &[data::TaggedSexp<'a>]) = if let Some(dots_pos) = dots_pos {
            if dots.is_none() {
                (0, &tmp_empty)
            } else {
                match &dots.unwrap().kind {
                    SexpKind::List(list) => (dots_pos, list.data),
                    _ => (0, &tmp_empty),
                }
            }
        } else {
            (0, &tmp_empty)
        };

        let matched = Self::match_args(def, &expr.args, dots, dots_pos);

        let actuals = self.arena.alloc_slice_fill_with(icall.args.len(), |_| {
            data::TaggedSexp::new(self.arena.missing)
        });

        for (index, arg) in icall.args.iter().enumerate() {
            match &arg.data.kind {
                SexpKind::Sym(sym) => {
                    if let Some(data) = matched.get(sym.data) {
                        actuals[index] = data.clone().strip_tag();
                        continue;
                    }
                    let tmp = def
                        .formals
                        .iter()
                        .find(|x| &x.name == sym)
                        .map(|x| data::TaggedSexp::new_with_tag(x.value, &x.name));
                    actuals[index] = tmp.unwrap_or_else(|| self.arena.missing.into()).strip_tag();
                }
                _ => actuals[index] = arg.clone(),
            }
        }

        let actuals = data::List { data: actuals };
        let res = lang::Lang::new(lang::Target::Sym(lang::Sym::new(target_sym_str)), actuals);

        Some(self.arena.alloc(res))
    }

    fn cmp_simple_internal(&mut self, name: &'a str, expr: &'a lang::Lang<'a>) -> bool {
        if self.any_dots(expr) {
            return false;
        }

        // this should been changed to take account
        // different enviroments
        let target = self.find_baseenv(name);
        // there should be better check
        if target.is_none() {
            return false;
        }

        let SexpKind::Closure(def) = &target.unwrap().kind else {
            return false;
        };

        let Some(simple_inter) = self.extract_simple_internal(name, expr, def) else {
            return false;
        };

        self.cmp_builtin(simple_inter, true)
    }

    fn cmp_builtin(&mut self, expr: &'a lang::Lang<'a>, internal: bool) -> bool {
        if self.dots_or_missing(&expr.args) {
            return false;
        }
        let index = self.code_buffer.add_const((&expr.target).into());

        if internal {
            self.code_buffer.add_instr2(BcOp::GETINTLBUILTIN_OP, index);
        } else {
            self.code_buffer.add_instr2(BcOp::GETBUILTIN_OP, index);
        }
        self.cmp_builtin_args(&expr.args, false);

        let index = self.code_buffer.add_const(expr.into());

        self.code_buffer.add_instr2(BcOp::CALLBUILTIN_OP, index);

        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }

        true
    }

    fn has_handler(&self, sym: &str) -> bool {
        match sym {
            "if" | "{" | "<-" | "<<-" | "+" | "-" | "*" | "/" | "^" | "exp" | ":" | "seq_along"
            | "seq_len" | "sqrt" | "while" | "for" | "break" | "next" | "return" | "function"
            | "local" | "[[" | "[" | ".Internal" | "==" | "!=" | "<" | "<=" | ">=" | ">" | "&"
            | "|" | "!" | "&&" | "||" | "$" => true,

            "is.character" | "is.complex" | "is.double" | "is.integer" | "is.logical"
            | "is.name" | "is.null" | "is.object" | "is.symbol" => true,

            _ if MATH1_FUNCS.contains(&sym) => true,
            _ if SAFE_BASE_INTERNALS.contains(&sym) => true,
            _ if self.builtins.contains(sym) => true,
            _ if self.specials.contains(sym) => true,
            _ => false,
        }
    }

    // missing_ok default false
    fn cmp_builtin_args(&mut self, args: &'a [data::TaggedSexp<'a>], missing_ok: bool) {
        let tmp = CompilerContext::new_arg(&self.context);
        let mut orig_context = std::mem::replace(&mut self.context, tmp);

        for arg in args {
            match &arg.data.kind {
                SexpKind::MissingArg => {
                    if missing_ok {
                        self.code_buffer.add_instr(BcOp::DOMISSING_OP);
                        self.cmp_tag(&arg.tag);
                    } else {
                        todo!()
                    }
                }
                SexpKind::Sym(sym) if sym.data == ".." => todo!(),
                SexpKind::Bc(_) => todo!(),
                SexpKind::Promise {
                    environment: _,
                    expr: _,
                    value: _,
                } => todo!(),
                SexpKind::Sym(sym) => {
                    self.cmp_sym(sym, missing_ok);
                    self.code_buffer.add_instr(BcOp::PUSHARG_OP);
                }
                SexpKind::Lang(_) => {
                    self.cmp(&arg.data, false, true);
                    self.code_buffer.add_instr(BcOp::PUSHARG_OP);
                }
                SexpKind::Nil => {
                    self.code_buffer.add_instr(BcOp::PUSHNULLARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Logic(logs)
                    if logs.len() == 1 && matches!(logs[0], data::Logic::True) =>
                {
                    self.code_buffer.add_instr(BcOp::PUSHTRUEARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                SexpKind::Logic(logs)
                    if logs.len() == 1 && matches!(logs[0], data::Logic::False) =>
                {
                    self.code_buffer.add_instr(BcOp::PUSHFALSEARG_OP);
                    self.cmp_tag(&arg.tag);
                }
                _ => {
                    let index = self.code_buffer.add_const(arg.data.into());
                    self.code_buffer.add_instr2(BcOp::PUSHCONSTARG_OP, index);
                    self.cmp_tag(&arg.tag);
                }
            }
        }

        std::mem::swap(&mut self.context, &mut orig_context);
    }

    fn cmp_while_body(&mut self, full: &'a lang::Lang<'a>, cond: &'a Sexp<'a>, body: &'a Sexp<'a>) {
        let loop_label = self.code_buffer.make_label();
        let end_label = self.code_buffer.make_label();
        self.code_buffer.put_label(loop_label);

        let tmp = CompilerContext::new_loop(&self.context, loop_label, end_label);
        let orig = std::mem::replace(&mut self.context, tmp);

        self.cmp(cond, false, true);

        let callidx = self.code_buffer.add_const(full.into());
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

    fn cmp_for_body(&mut self, skip_init: bool, call_idx: i32, body: &'a Sexp<'a>, sym_idx: i32) {
        let body_label = self.code_buffer.make_label();
        let loop_label = self.code_buffer.make_label();
        let end_label = self.code_buffer.make_label();

        // original compiler has check if sym_idx (ci) is null
        // this is only possible only when skip loop ctx is not
        // possible so I just did it by bool flag
        if skip_init {
            self.code_buffer.add_instr2(BcOp::GOTO_OP, DEFLABEL);
            self.code_buffer.set_label(loop_label);
        } else {
            self.code_buffer
                .add_instr_n(BcOp::STARTFOR_OP, &[call_idx, sym_idx, DEFLABEL]);
            self.code_buffer.set_label(loop_label);
        }

        self.code_buffer.put_label(body_label);

        let tmp = CompilerContext::new_loop(&self.context, loop_label, end_label);
        let orig = std::mem::replace(&mut self.context, tmp);

        self.cmp(body, false, true);

        let _ = std::mem::replace(&mut self.context, orig);

        self.code_buffer.add_instr(BcOp::POP_OP);
        self.code_buffer.put_label(loop_label);
        self.code_buffer.add_instr2(BcOp::STEPFOR_OP, DEFLABEL);
        self.code_buffer.set_label(body_label);
        self.code_buffer.put_label(end_label);
    }

    fn check_skip_loopctx_lang(&self, lang: &'a lang::Lang<'a>, break_ok: bool) -> bool {
        match &lang.target {
            lang::Target::Sym(sym) => {
                if !break_ok && matches!(sym.data, "break" | "next") {
                    false
                } else if self.is_loop_stop_fun(sym.data) {
                    true
                } else if self.is_loop_top_fun(sym.data) {
                    self.check_skip_loopctx_list(&lang.args, break_ok)
                } else if matches!(sym.data, "eval" | "evalq" | "source") {
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
    fn check_skip_loopctx(&self, sexp: &'a Sexp<'a>, break_ok: bool) -> bool {
        match &sexp.kind {
            SexpKind::Lang(lang) => self.check_skip_loopctx_lang(lang, break_ok),
            _ => true,
        }
    }

    fn check_skip_loopctx_list(&self, list: &'a data::List<'a>, break_ok: bool) -> bool {
        for arg in list.into_iter() {
            if !self.missing(&arg.data) && !self.check_skip_loopctx(&arg.data, break_ok) {
                return false;
            }
        }
        true
    }

    fn is_loop_stop_fun(&self, name: &'a str) -> bool {
        matches!(name, "function" | "for" | "while" | "repeat") && self.is_base_var(name)
    }

    fn is_loop_top_fun(&self, name: &'a str) -> bool {
        matches!(name, "(" | "{" | "if") && self.is_base_var(name)
    }

    fn cmp_switch(&mut self, switch: &'a lang::Lang<'a>) -> bool {
        //return self.cmp_special(switch);
        let expr = switch.args[0].data;
        let cases = &switch.args[1..];
        if cases.len() == 0 {
            self.warnings.push(Warning::NoCasesInSwitch)
        }

        let miss = self.missing_args(cases);
        let mut names = self.names(cases);
        if names.len() == 0 && cases.len() == 1 {
            names.push(self.arena.empty_string);
        }

        let mut have_names = false;
        let mut have_char_default = false;
        // could happen if cases len in not 1
        if names.len() != 0 {
            have_names = true;
            let n_default = names.iter().filter(|x| **x == "").count();
            if n_default > 1 {
                self.warnings.push(Warning::MultipleSwitchDefaults);
                return self.cmp_special(switch);
            }
            if n_default > 0 {
                have_char_default = true;
            }
        }
        let miss_label = if miss.iter().any(|x| *x) {
            Some(self.code_buffer.make_label())
        } else {
            None
        };

        let mut lab = |missing| {
            if missing {
                miss_label.unwrap()
            } else {
                self.code_buffer.make_label()
            }
        };
        let mut labels: Vec<_> = miss.iter().map(|x| lab(*x)).collect();
        let default_label = self.code_buffer.make_label();
        labels.push(default_label);

        let end_label = if !self.context.tailcall {
            Some(self.code_buffer.make_label())
        } else {
            None
        };

        if have_names {
            // get uniq without loosing order
            // I must find better way
            let mut unique_names: Vec<_> = names.iter().filter(|x| **x != "").cloned().collect();
            for i in 0..unique_names.len() {
                for j in ((i + 1)..unique_names.len()).rev() {
                    if unique_names[i] == unique_names[j] {
                        unique_names.remove(j);
                    }
                }
            }

            if have_char_default {
                unique_names.push(self.arena.empty_string);
            }

            let miss_indexes: Vec<usize> = miss
                .iter()
                .zip(0..)
                .filter(|x| !*x.0)
                .map(|x| x.1)
                .collect();
            let mut nlabels: Vec<usize> = unique_names
                .iter()
                .map(|name| {
                    let index = Self::find_action_index(name, &names, &miss_indexes);
                    labels[index]
                })
                .collect();

            if !have_char_default {
                unique_names.push("");
                nlabels.push(default_label);
            }

            let tailcall = self.context.tailcall;
            self.context.tailcall = false;
            self.cmp(expr, false, true);
            self.context.tailcall = tailcall;

            let expr_index = self.code_buffer.add_const_lang(switch.into());
            let names_sexp = self.arena.alloc_slice_clone(names.as_slice());
            let names_sexp = data::RVec::new(names_sexp);
            let names_sexp = SexpKind::Str(names_sexp);
            let names_sexp: &'a Sexp<'a> = self.arena.alloc(names_sexp.into());
            let names_index = self.code_buffer.add_const_sexp(names_sexp);
            self.code_buffer.add_instr_n(
                BcOp::SWITCH_OP,
                &[expr_index, names_index, DEFLISTLABEL, DEFLISTLABEL],
            );
            self.code_buffer.add_list_label(nlabels, 1);
            self.code_buffer.add_list_label(labels.clone(), 0);
        } else {
            let tailcall = self.context.tailcall;
            self.context.tailcall = false;
            self.cmp(expr, false, true);
            self.context.tailcall = tailcall;

            let expr_index = self.code_buffer.add_const_lang(switch.into());
            let names_index = self.code_buffer.add_const_sexp(self.arena.nil.into());
            self.code_buffer.add_instr_n(
                BcOp::SWITCH_OP,
                &[expr_index, names_index, names_index, DEFLISTLABEL],
            );
            self.code_buffer.add_list_label(labels.clone(), 0);
        }

        if miss.iter().any(|x| *x) {
            self.code_buffer.put_label(miss_label.unwrap());
            let stop_sexp = self.code_buffer.get_stop_switch(self.arena);
            self.cmp_call(stop_sexp, true);
        }

        self.code_buffer.put_label(default_label);
        self.code_buffer.add_instr(BcOp::LDNULL_OP);
        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::INVISIBLE_OP);
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        } else {
            self.code_buffer.add_instr2(BcOp::GOTO_OP, DEFLABEL);
            self.code_buffer.set_label(end_label.unwrap());
        }

        for (idx, case) in cases.iter().enumerate() {
            if !miss[idx] {
                self.code_buffer.put_label(labels[idx]);
                self.cmp(&case.data, false, true);

                if !self.context.tailcall {
                    self.code_buffer.add_instr2(BcOp::GOTO_OP, DEFLABEL);
                    self.code_buffer.set_label(end_label.unwrap());
                }
            }
        }

        if !self.context.tailcall {
            self.code_buffer.put_label(end_label.unwrap());
        }
        true
    }

    fn find_action_index(name: &'a str, names: &Vec<&'a str>, miss_indexes: &Vec<usize>) -> usize {
        let start = names.iter().position(|x| *x == name).unwrap();
        let mut aidxs: Vec<usize> = miss_indexes
            .iter()
            .filter(|x| **x >= start)
            .cloned()
            .collect();
        if names.len() > start {
            aidxs.push(names.len())
        }
        aidxs.iter().min().unwrap().clone()
    }

    fn ddval(&self, sym: &'a lang::Sym<'a>) -> Option<i32> {
        if sym.data.len() > 2 && sym.data.starts_with("..") {
            match sym.data[2..].parse::<i32>() {
                Ok(num) => Some(num),
                Err(_) => None,
            }
        } else {
            None
        }
    }

    fn missing(&self, sexp: &'a Sexp<'a>) -> bool {
        // TODO
        match &sexp.kind {
            SexpKind::MissingArg => true,
            SexpKind::Sym(sym) if sym.data == "..." =>  true,
            SexpKind::Sym(sym) if self.ddval(sym).is_some() => true,
            _ => false,
        }
    }

    fn missing_args(&self, list: &[data::TaggedSexp<'a>]) -> Vec<bool> {
        let mut res = vec![];
        for item in list {
            match item.data.kind {
                SexpKind::MissingArg => res.push(true),
                _ => res.push(false),
            }
        }
        res
    }

    fn names(&self, list: &[data::TaggedSexp<'a>]) -> Vec<&'a str> {
        let mut res = vec![];
        let mut any = false;
        for item in list {
            match item.tag {
                Some(tag) if !tag.data.is_empty() => {
                    any = true;
                    res.push(tag.data)
                }
                _ => res.push(self.arena.empty_string),
            }
        }

        if any {
            res
        } else {
            vec![]
        }
    }

    fn find_any_var(&self, name: &'a str) -> Option<&'a Sexp<'a>> {
        if let Some(x) = self.env.find_local_var(name) {
            return Some(x);
        }

        if let Some(x) = self.find_namespacebase(name) {
            return Some(x);
        }

        if let Some(x) = self.find_baseenv(name) {
            return Some(x);
        }
        None
    }

    fn find_baseenv(&self, name: &'a str) -> Option<&'a Sexp<'a>> {
        match self.baseenv {
            Some(env) => env.find_local_var(name),
            None => None,
        }
    }

    fn find_namespacebase(&self, name: &str) -> Option<&'a Sexp<'a>> {
        match &self.namespacebase {
            Some(env) => env.find_local_var(name),
            None => None,
        }
    }

    fn is_base_var(&self, name: &'a str) -> bool {
        let local = self.env.find_local_var(name).is_some() || self.localenv.contains(name);
        (self.find_baseenv(name).is_some() || self.find_namespacebase(name).is_some()) && !local
    }

    fn get_inlineinfo(&self, function: &'a str) -> Option<InlineInfo> {
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

    fn get_assigned_var(&self, sexp: &'a Sexp<'a>) -> Option<&'a lang::Sym<'a>> {
        match &sexp.kind {
            SexpKind::Sym(sym) => Some(sym),
            SexpKind::Lang(lang) => self.get_assigned_var(&lang.args[0].data),
            SexpKind::MissingArg => todo!(),
            _ => None,
        }
    }

    fn get_assign_fun(&self, sexp: &'a lang::Target<'a>) -> Option<lang::Target<'a>> {
        match &sexp {
            lang::Target::Sym(sym) => {
                // temp heap alloc
                let n_sym = sym.data.to_string() + "<-";
                let n_sym = lang::Sym::new(self.arena.alloc_str(n_sym.as_str()));
                Some(lang::Target::Sym(n_sym))
            }
            lang::Target::Lang(lang) if lang.args.len() == 1 => {
                let lang::Target::Sym(sym) = &lang.target else {
                    return None;
                };
                if !matches!(sym.data, "::" | ":::") {
                    return None;
                }
                match (&lang.args[0].data.kind, &lang.args[1].data.kind) {
                    (SexpKind::Sym(_), SexpKind::Sym(function)) => {
                        let n_sym = function.data.to_string() + "<-";
                        let n_sym = lang::Sym::new(self.arena.alloc_str(n_sym.as_str()));
                        let n_sym: &'a Sexp<'a> = self.arena.alloc(SexpKind::Sym(n_sym).into());
                        let n_sym = data::TaggedSexp::new(n_sym);
                        let data = self.arena.alloc_slice_clone(&[lang.args[0].clone(), n_sym]);
                        let args = data::List { data };
                        let n_lang = self.arena.alloc(lang::Lang::new(lang.target.clone(), args));
                        Some(lang::Target::Lang(n_lang))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn find_locals(&mut self, sexp: &'a Sexp<'a>) {
        let SexpKind::Lang(sexp) = &sexp.kind else {
            return;
        };

        let lang::Target::Sym(target) = &sexp.target else {
            return;
        };

        match target.data {
            "=" | "<-" | "for" => {
                let var = self.get_assigned_var(&sexp.args[0].data);
                if let Some(var) = var {
                    self.localenv.insert(var.data);
                }
            }
            "delayedAssign" | "assign" => match &sexp.args[0].data.kind {
                SexpKind::Str(name) if name.len() == 1 => {
                    self.localenv.insert(name[0]);
                }
                _ => (),
            },
            "function" => (),
            "~" => todo!(),
            "local" => todo!(),
            "expression" | "quote" => (),
            _ => (),
        }

        for arg in sexp.args.into_iter() {
            self.find_locals(&arg.data);
        }
    }
}

pub struct CompilerOptions {
    pub inline_level: usize,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self { inline_level: 2 }
    }
}

impl CompilerOptions {
    #[allow(dead_code)]
    pub fn new(inline_level: usize) -> Self {
        Self { inline_level }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bumpalo::Bump;
    use std::cell::UnsafeCell;
    use std::io::{BufWriter, Read, Write};
    use std::sync::Once;

    use crate::rds::{rds_reader::RDSReader, rds_writer::RDSWriter, RDSResult};

    macro_rules! test_fun_noopt {
        ( $name:ident, $code:expr) => {
            mod $name {
                use super::*;

                #[test]
                fn compiler() {
                    let mut arena = Bump::new();
                    let arena = Alloc::new(&mut arena);
                    let path = format!("temp/{}_compiler.dat", stringify!($name));
                    let path = path.as_str();
                    let path_comp = format!("temp/{}_compiler_corr.dat", stringify!($name));
                    let path_comp = path_comp.as_str();

                    // input and output data serialized
                    let mut command = std::process::Command::new("./scripts/create_testdata.R")
                        .args([$code, path, path_comp, "-noopt"])
                        .spawn()
                        .unwrap();
                    assert!(command.wait().unwrap().success());

                    let file = std::fs::File::open(path).unwrap();
                    let file = RDSReader::new(UnsafeCell::new(file), &arena);
                    let RDSResult {
                        header,
                        data: input,
                    } = file.read_rds().unwrap();

                    let mut input_vec = vec![];
                    let mut file = std::fs::File::open(path_comp).unwrap();
                    file.read_to_end(&mut input_vec).unwrap();

                    let file = std::fs::File::open(path_comp).unwrap();
                    let file = RDSReader::new(UnsafeCell::new(file), &arena);
                    let RDSResult {
                        header: _,
                        data: correct,
                    } = file.read_rds().unwrap();

                    println!("{}", input);

                    let mut compiler = Compiler::new_options(0, &arena);
                    let SexpKind::Closure(cl) = &input.kind else {
                        unreachable!();
                    };
                    let bc = compiler.cmpfun(cl);

                    insta::assert_debug_snapshot!(compiler.warnings);
                    let input: &Sexp = arena.alloc(SexpKind::Closure(bc).into());

                    println!("My compilation:\n{input}\n");
                    println!("Correct compilation:\n{correct}");

                    let outdata: Vec<u8> = vec![];
                    let mut writer = BufWriter::new(outdata);
                    writer.write_rds(header, input, &arena).unwrap();
                    writer.flush().unwrap();

                    assert_eq!(writer.get_ref(), &input_vec);
                    std::fs::remove_file(path).unwrap();
                    std::fs::remove_file(path_comp).unwrap();
                }
            }
        };
    }

    static SETUP: Once = Once::new();
    static mut DONE_CHECK: bool = false;
    fn setup() {
        let path_env = "temp/test_compiler_env.dat";
        SETUP.call_once(|| {
            if std::fs::metadata(path_env).is_ok_and(|x| x.is_file()) {
                std::fs::remove_file(path_env).unwrap()
            }
            // base environment
            let mut command = std::process::Command::new("./scripts/baseenv.R")
                .args([path_env])
                .spawn()
                .unwrap();
            assert!(command.wait().unwrap().success());
            unsafe { DONE_CHECK = true }
        })
    }

    macro_rules! test_fun_default {
        ( $name:ident, $code:expr) => {
            mod $name {
                use super::*;

                #[test]
                fn compiler() {
                    let mut arena = Bump::new();
                    let arena = Alloc::new(&mut arena);
                    let path = format!("temp/{}_compiler.dat", stringify!($name));
                    let path = path.as_str();
                    let path_comp = format!("temp/{}_compiler_corr.dat", stringify!($name));
                    let path_comp = path_comp.as_str();
                    let path_env = "temp/test_compiler_env.dat";

                    // input and output data serialized
                    let mut command = std::process::Command::new("./scripts/create_testdata.R")
                        .args([$code, path, path_comp, "-opt"])
                        .spawn()
                        .unwrap();
                    assert!(command.wait().unwrap().success());

                    if unsafe { !DONE_CHECK } {
                        setup();
                    }

                    let file = std::fs::File::open(path).unwrap();

                    let file = RDSReader::new(UnsafeCell::new(file), &arena);
                    let RDSResult {
                        header,
                        data: input,
                    } = file.read_rds().unwrap();

                    let mut input_vec = vec![];
                    let mut file = std::fs::File::open(path_comp).unwrap();
                    file.read_to_end(&mut input_vec).unwrap();

                    let file = std::fs::File::open(path_comp).unwrap();
                    let file = RDSReader::new(UnsafeCell::new(file), &arena);
                    let RDSResult {
                        header: _,
                        data: correct,
                    } = file.read_rds().unwrap();

                    let file = std::fs::File::open(path_env).unwrap();
                    let file = RDSReader::new(UnsafeCell::new(file), &arena);
                    let RDSResult {
                        header: _,
                        data: baseenv,
                    } = file.read_rds().unwrap();

                    let file = std::fs::File::open(format!("{path_env}.specials")).unwrap();
                    let file = RDSReader::new(UnsafeCell::new(file), &arena);
                    let RDSResult {
                        header: _,
                        data: specials,
                    } = file.read_rds().unwrap();

                    let file = std::fs::File::open(format!("{path_env}.builtins")).unwrap();
                    let file = RDSReader::new(UnsafeCell::new(file), &arena);
                    let RDSResult {
                        header: _,
                        data: builtins,
                    } = file.read_rds().unwrap();

                    let file = std::fs::File::open(format!("{path_env}.internal")).unwrap();
                    let file = RDSReader::new(UnsafeCell::new(file), &arena);
                    let RDSResult {
                        header: _,
                        data: internals,
                    } = file.read_rds().unwrap();

                    let SexpKind::Str(specials) = specials.kind else {
                        unreachable!()
                    };

                    let SexpKind::Str(builtins) = builtins.kind else {
                        unreachable!()
                    };

                    let SexpKind::Str(internals) = internals.kind else {
                        unreachable!()
                    };

                    println!("{}", input);

                    let mut compiler = Compiler::new(&arena);
                    let SexpKind::Closure(cl) = &input.kind else {
                        unreachable!();
                    };
                    let SexpKind::Environment(lang::Environment::Normal(baseenv)) = baseenv.kind
                    else {
                        unreachable!()
                    };

                    compiler.specials = HashSet::from_iter(specials.to_vec().into_iter());
                    compiler.builtins = HashSet::from_iter(builtins.to_vec().into_iter());
                    compiler.internals = HashSet::from_iter(internals.to_vec().into_iter());

                    compiler.set_baseenv(baseenv);
                    let bc = compiler.cmpfun(cl);

                    insta::assert_debug_snapshot!(compiler.warnings);
                    let input: &Sexp = arena.alloc(SexpKind::Closure(bc).into());

                    println!("My compilation:\n{input}\n");
                    println!("Correct compilation:\n{correct}");

                    let outdata: Vec<u8> = vec![];
                    let mut writer = BufWriter::new(outdata);
                    writer.write_rds(header, input, &arena).unwrap();
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
    test_fun_noopt![higher_order, "(function(x) function(y) x + y)(1)"];
    test_fun_noopt![
        tmp_noopt,
        "
        function (x) {
            g(x);
            f(y);
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
    test_fun_default![higher_order_opt, "(function(x) function(y) x + y)(1)"];
    test_fun_default![dotrow, "function(dim) .Internal(row(dim))"];
    test_fun_default![
        print_asis,
        "function (x, ...) {
            cl <- oldClass(x)
            oldClass(x) <- cl[cl != \"AsIs\"]
            NextMethod(\"print\")
            invisible(x)
        }"
    ];
    test_fun_default![
        two_calls,
        "
        function (x) {
            g(x);
            f(x);
        }"
    ];
    test_fun_default![
        gettext,
        "function (..., domain = NULL, trim = TRUE) {
            char <- unlist(lapply(list(...), as.character))
            .Internal(gettext(domain, char, trim))
        }"
    ];
    test_fun_default![xor, "function(x, y) (x | y) & !(x & y)"];
    test_fun_default![
        vapply,
        "function (X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) {
            FUN <- match.fun(FUN)
            if (!is.vector(X) || is.object(X))
                X <- as.list(X)
            .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES))
        }"
    ];
    test_fun_default![
        vapply_simple,
        "function (X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) {
            .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES))
        }"
    ];
    test_fun_default![
        for_endloopctx,
        "function (pars){
            for (p in pars) {
              prompt <- \"... (a list): \"
              args <- c(args, eval(parse(prompt = prompt)))
            }
            .Internal(.invokeRestart(r, args))
        }"
    ];
    test_fun_default![
        isa,
        "function (x, what) {
            if (isS4(x))
                methods::is(x, what)
            else all(class(x) %in% what)
        }"
    ];
    test_fun_default![
        remove_task_callback,
        "function (id){
            if (!is.character(id))
                id <- as.integer(id)
            .Call(.C_R_removeTaskCallback, id)
        }"
    ];
    test_fun_default![
        replace,
        "function (x, list, values) {
            x[list] <- values
            x
        }"
    ];
    test_fun_default![
        nextmethod,
        "function (generic = NULL, object = NULL, ...)
        .Internal(NextMethod(generic, object, ...))"
    ];
    test_fun_default![
        range_posixct,
        "function (..., na.rm = FALSE, finite = FALSE)
        .rangeNum(..., na.rm = na.rm, finite = finite, isNumeric = function(.) TRUE)"
    ];
    test_fun_default![
        storage_mode,
        "function (x)
        switch(tx <- typeof(x), closure = , builtin = , special = \"function\", tx)"
    ];

    test_fun_noopt![
        tmp_test,
        "
        function (frame, rownames.force = NA)
        {
            if (!is.data.frame(frame))
                return(as.matrix(frame))
            d <- dim(frame)
            rn <- if (isFALSE(rownames.force))
                NULL
            else if (isTRUE(rownames.force))
                row.names(frame)
            else if (.row_names_info(frame) <= 0L)
                NULL
            else row.names(frame)
            for (i in seq_len(d[2L])) {
                xi <- frame[[i]]
                if (is.integer(xi) || is.numeric(xi))
                    next
                if (is.logical(xi) || is.factor(xi)) {
                    frame[[i]] <- as.integer(xi)
                    next
                }
                if (is.character(xi)) {
                    frame[[i]] <- as.integer(factor(xi))
                    next
                }
                frame[[i]] <- if (isS4(xi))
                    methods::as(xi, \"numeric\")
                else as.numeric(xi)
            }
            intOK <- all(unlist(lapply(frame, is.integer)))
            x <- matrix(if (intOK)
                NA_integer_
            else NA_real_, nrow = d[1L], ncol = d[2L], dimnames = list(rn,
                names(frame)))
            for (i in seq_len(d[2L])) x[, i] <- frame[[i]]
            x
        }
        "
    ];

    test_fun_default![
        rbind,
        "function (..., deparse.level = 1)
        .Internal(rbind(deparse.level, ...))"
    ];

    // This will need subset of interpret
    /*
    test_fun_default![
        gc,
        r#"function (verbose = getOption("verbose"), reset = FALSE, full = TRUE)
        {
            res <- .Internal(gc(verbose, reset, full))
            res <- matrix(res, 2L, 7L, dimnames = list(c("Ncells", "Vcells"),
                c("used", "(Mb)", "gc trigger", "(Mb)", "limit (Mb)",
                    "max used", "(Mb)")))
            if (all(is.na(res[, 5L])))
                res[, -5L]
            else res
        }"#
    ];*/

    test_fun_default![
        dot_merge_import_methods,
        r#"
        function (impenv, expenv, metaname)
        {
            impMethods <- impenv[[metaname]]
            if (!is.null(impMethods))
                impenv[[metaname]] <- methods:::.mergeMethodsTable2(impMethods,
                    newtable = expenv[[metaname]], expenv, metaname)
            impMethods
        }"#
    ];

    test_fun_default![
        format_data_frame,
        r#"
        function (x, ..., justify = "none")
        {
            nc <- length(x)
            if (!nc)
                return(x)
            nr <- .row_names_info(x, 2L)
            rval <- vector("list", nc)
            for (i in seq_len(nc)) rval[[i]] <- format(x[[i]], ..., justify = justify)
            lens <- vapply(rval, NROW, 1)
            if (any(lens != nr)) {
                warning("corrupt data frame: columns will be truncated or padded with NAs")
                for (i in seq_len(nc)) {
                    len <- NROW(rval[[i]])
                    if (len == nr)
                        next
                    if (length(dim(rval[[i]])) == 2L) {
                        rval[[i]] <- if (len < nr)
                          rbind(rval[[i]], matrix(NA, nr - len, ncol(rval[[i]])))
                        else rval[[i]][seq_len(nr), ]
                    }
                    else {
                        rval[[i]] <- if (len < nr)
                          c(rval[[i]], rep.int(NA, nr - len))
                        else rval[[i]][seq_len(nr)]
                    }
                }
            }
            for (i in seq_len(nc)) {
                if (is.character(rval[[i]]) && inherits(rval[[i]], "character"))
                    oldClass(rval[[i]]) <- "AsIs"
            }
            y <- as.data.frame.list(rval, row.names = seq_len(nr), col.names = names(x),
                optional = TRUE, fix.empty.names = FALSE, cut.names = TRUE)
            attr(y, "row.names") <- row.names(x)
            y
        }"#
    ];

    #[test]
    fn test_find_action_index() {
        let miss = vec![true, true, false, false];
        let names = vec!["a", "b", "c", ""];
        let unique_names = vec!["a", "b", "c"];
        let miss_indexes: Vec<usize> = miss
            .iter()
            .zip(0..)
            .filter(|x| !*x.0)
            .map(|x| x.1)
            .collect();
        println!("{miss_indexes:?}");
        let res: Vec<usize> = unique_names
            .iter()
            .map(|name| Compiler::find_action_index(name, &names, &miss_indexes))
            .collect();

        assert_eq!(res, vec![2, 2, 2]);
    }
}
