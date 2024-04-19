use crate::sexp::{
    bc::{Bc, BcOp},
    sexp::{data, lang, MetaData, Sexp, SexpKind},
};

#[derive(Default, Clone)]
struct LoopContext {
    loop_label: LabelIdx,
    end_label: LabelIdx,
    goto_ok: bool,
}

#[derive(Default, Clone)]
struct CompilerContext {
    top_level: bool,
    need_returnjmp: bool,
    tailcall: bool,
    loop_ctx: Option<LoopContext>,
    call: Option<Sexp>,
}

impl CompilerContext {
    fn new_top(ctxt: &CompilerContext) -> Self {
        Self {
            top_level: true,
            tailcall: true,
            call: None,
            ..ctxt.clone()
        }
    }

    fn new_promise(ctxt: &CompilerContext) -> Self {
        Self {
            top_level: false,
            tailcall: true,
            ..ctxt.clone()
        }
    }

    fn new_call(ctxt: &CompilerContext, call: Sexp) -> Self {
        Self {
            call: Some(call.clone()),
            ..ctxt.clone()
        }
    }

    fn new_arg(ctxt: &CompilerContext) -> Self {
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

    fn new_loop(ctxt: &CompilerContext, loop_label: LabelIdx, end_label: LabelIdx) -> Self {
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

/// i32 min represents NA in intepreter
const NA: i32 = i32::MIN;

// placeholder label which is inserted before patching
// I choose this value since I wanted to be easily visible
const DEFLABEL: i32 = 0xeeeeeee;

#[derive(Debug)]
pub enum Warning {
    VariableDoesNotExist(String),
    NoLoopContext,
}

type LabelIdx = usize;

#[derive(Default)]
struct Label {
    value: i32,
    positions: Vec<usize>,
}

struct CodeBuffer {
    bc: Bc,
    current_expr: Option<Sexp>,
    expression_buffer: Vec<i32>,
    labels: Vec<Label>,
}

impl CodeBuffer {
    fn new() -> Self {
        Self {
            bc: Bc::new(),
            current_expr: None,
            expression_buffer: vec![NA], // first instruction is version and does not have a source
            labels: vec![],
        }
    }

    fn new_with_expr(curr_expr: Sexp) -> Self {
        Self {
            bc: Bc::new(),
            current_expr: Some(curr_expr),
            expression_buffer: vec![NA], // first instruction is version and does not have a source
            labels: vec![],
        }
    }

    fn insert_currexpr(&mut self, bc_count: usize) {
        if self.current_expr.is_some() {
            let expr = self.current_expr.clone().unwrap();
            let index = self.add_const(expr.clone());
            self.expression_buffer
                .append(&mut (0..bc_count).map(|_| index).collect());
        }
    }

    fn add_instr(&mut self, op: BcOp) {
        assert_eq!(op.arity(), 0, "Wrong arity");
        self.bc.instructions.push(op.into());
        self.insert_currexpr(1);
    }

    fn add_instr2(&mut self, op: BcOp, idx: i32) {
        assert_eq!(op.arity(), 1, "Wrong arity");
        self.bc.instructions.push(op.into());
        self.bc.instructions.push(idx);
        self.insert_currexpr(2);
    }

    fn add_instr_n(&mut self, op: BcOp, idxs: &[i32]) {
        assert_eq!(op.arity(), idxs.len(), "Wrong arity");
        self.bc.instructions.push(op.into());
        self.bc.instructions.extend_from_slice(idxs);
        self.insert_currexpr(1 + idxs.len());
    }

    fn add_const(&mut self, val: Sexp) -> i32 {
        match self.bc.constpool.iter().position(|x| x == &val) {
            Some(idx) => idx as i32,
            None => {
                self.bc.constpool.push(val);
                (self.bc.constpool.len() - 1) as i32
            }
        }
    }

    fn set_current_expr(&mut self, sexp: Sexp) -> Option<Sexp> {
        std::mem::replace(&mut self.current_expr, Some(sexp))
    }

    fn restore_current_expr(&mut self, orig: Option<Sexp>) {
        let _ = std::mem::replace(&mut self.current_expr, orig);
    }

    fn make_label(&mut self) -> LabelIdx {
        self.labels.push(Label::default());
        self.labels.len() - 1
    }

    fn set_label(&mut self, label: LabelIdx) {
        self.labels[label]
            .positions
            .push(self.bc.instructions.len() - 1)
    }

    fn put_label(&mut self, label: LabelIdx) {
        self.labels[label as usize].value = self.bc.instructions.len() as i32;
    }

    fn patch_labels(&mut self) {
        for label in &self.labels {
            for pos in &label.positions {
                self.bc.instructions[*pos] = label.value;
            }
        }
    }
}

pub struct Compiler {
    options: CompilerOptions,
    context: CompilerContext,
    code_buffer: CodeBuffer,

    environment: lang::Environment,
    pub warnings: Vec<Warning>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            options: CompilerOptions::default(),
            context: CompilerContext::new_top(&CompilerContext::default()),
            code_buffer: CodeBuffer::new(),

            environment: lang::Environment::Global,
            warnings: vec![],
        }
    }

    pub fn new_options(inline_level: usize) -> Self {
        Self {
            options: CompilerOptions { inline_level },
            context: CompilerContext::new_top(&CompilerContext::default()),
            code_buffer: CodeBuffer::new(),

            environment: lang::Environment::Global,
            warnings: vec![],
        }
    }

    pub fn cmpfun(&mut self, closure: lang::Closure) -> lang::Closure {
        let mut closure = closure;
        self.environment = lang::NormalEnv::new(
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
        let body =
            SexpKind::Bc(self.gen_code(closure.body.as_ref(), Some(closure.body.as_ref()))).into();
        closure.body = Box::new(body);
        let lang::Environment::Normal(env) = &mut self.environment else {
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
            SexpKind::Promise => todo!(),
            SexpKind::Lang(lang) => self.cmp_call(lang),
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

    fn cmp_call(&mut self, call: &lang::Lang) {
        // set up state
        let mut sexp_tmp = std::mem::take(&mut self.code_buffer.current_expr);
        let orig = self.code_buffer.set_current_expr(call.clone().into());
        let tmp = CompilerContext::new_call(&self.context, call.clone().into());
        let mut orig_context = std::mem::replace(&mut self.context, tmp);

        if self.try_inline(call) {
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
                SexpKind::Promise => todo!(),
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
        self.environment.find_local_var(name).is_some()
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
            _ => false,
        }
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

    fn missing(&self, sexpr: &Sexp) -> bool {
        // TODO
        false
    }

    fn is_base_var(&self, name: &str) -> bool {
        // TODO
        true
    }

    fn get_inlineinfo(&self, function: &str) -> Option<bool> {
        if self.options.inline_level > 0 {
            Some(false)
        } else {
            None
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
                    let mut command = std::process::Command::new("./create_serdata.R")
                        .args(["-d", $code, path])
                        .spawn()
                        .unwrap();
                    assert!(command.wait().unwrap().success());
                    let mut command = std::process::Command::new("./create_serdata.R")
                        .args([
                            "-d",
                            format!("compiler::cmpfun({})", $code).as_str(),
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

                    let mut compiler = Compiler::new();
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

}
