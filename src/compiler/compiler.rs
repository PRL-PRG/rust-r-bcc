use crate::sexp::{
    bc::{Bc, BcOp},
    sexp::{data, lang, MetaData, Sexp, SexpKind},
};

#[derive(Default, Clone)]
struct CompilerContext {
    top_level: bool,
    tailcall: bool,
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
}

/// i32 min represents NA in intepreter
const NA: i32 = i32::MIN;

#[derive(Debug)]
pub enum Warning {
    VariableDoesNotExist(String),
}

struct CodeBuffer {
    bc: Bc,
    current_expr: Option<Sexp>,
    expression_buffer: Vec<i32>,
}

impl CodeBuffer {
    fn new() -> Self {
        Self {
            bc: Bc::new(),
            current_expr: None,
            expression_buffer: vec![NA], // first instruction is version and does not have a source
        }
    }

    fn new_with_expr(curr_expr: Sexp) -> Self {
        Self {
            bc: Bc::new(),
            current_expr: Some(curr_expr),
            expression_buffer: vec![NA], // first instruction is version and does not have a source
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
        println!("set curr expr {sexp}");
        std::mem::replace(&mut self.current_expr, Some(sexp))
    }

    fn restore_current_expr(&mut self, orig: Option<Sexp>) {
        let _ = std::mem::replace(&mut self.current_expr, orig);
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
            options: CompilerOptions,
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
        let tmp = if let Some(loc) =loc {
            CodeBuffer::new_with_expr(loc.clone())
        } else {
            CodeBuffer::new() 
        };
        let orig = std::mem::replace(&mut self.code_buffer, tmp);
        self.code_buffer.add_const(target.clone());
        self.cmp(&target, false, false);
        let locs = self.create_expression_loc();
        self.code_buffer.add_const(locs);
        std::mem::replace(&mut self.code_buffer, orig).bc
    }

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

        match &call.target {
            lang::Target::Lang(_) => todo!(),
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
                }
            }
        }
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
}

pub struct CompilerOptions;

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

                    let mut compiler = Compiler::new();
                    let SexpKind::Closure(cl) = input.kind else {
                        unreachable!();
                    };
                    let bc = compiler.cmpfun(cl);
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
    test_fun_noopt![addition, "function(x, y) x + y"];
    test_fun_noopt![block, "function(a, b = 0) {x <- c(a, 1); x[[b]];}"];
}
