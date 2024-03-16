use crate::sexp::{
    bc::{Bc, BcOp},
    sexp::{data, lang, MetaData, Sexp, SexpKind},
};

struct CompilerContext {
    tailcall: bool,
}

impl CompilerContext {
    fn new_top() -> Self {
        Self { tailcall: true }
    }
}

const NA: i32 = i32::MIN;

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

    fn insert_currexpr(&mut self, bc_count: usize) {
        if self.current_expr.is_some() {
            let expr = std::mem::replace(&mut self.current_expr, None).unwrap();
            let index = self.add_const(expr.clone());
            self.expression_buffer
                .append(&mut (0..bc_count).map(|_| index).collect());
        }
    }

    fn add_instr(&mut self, op: BcOp) {
        self.bc.instructions.push(op.into());
        self.insert_currexpr(1);
    }

    fn add_instr2(&mut self, op: BcOp, idx: i32) {
        self.bc.instructions.push(op.into());
        self.bc.instructions.push(idx);
        self.insert_currexpr(2);
    }

    fn add_instr_n(&mut self, op: BcOp, idxs: &[i32]) {
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

    fn set_current_expr(&mut self, sexp: Sexp) {
        self.current_expr = Some(sexp);
    }
}

pub struct Compiler {
    options: CompilerOptions,
    context: CompilerContext,
    code_buffer: CodeBuffer,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            options: CompilerOptions,
            context: CompilerContext::new_top(),
            code_buffer: CodeBuffer::new(),
        }
    }

    pub fn cmpfun(self, closure: lang::Closure) -> lang::Closure {
        let mut closure = closure;
        let body = SexpKind::Bc(self.gen_code(*closure.body)).into();
        closure.body = Box::new(body);
        closure
    }

    fn gen_code(self, target: Sexp) -> Bc {
        let mut tmp = self;
        //tmp.code_buffer.add_const(target.clone());
        tmp.cmp(&target, false);
        tmp.code_buffer.add_const(Compiler::create_temp_loc());
        tmp.code_buffer.bc
    }

    fn cmp(&mut self, sexp: &Sexp, missing_ok: bool) {
        self.code_buffer.set_current_expr(sexp.clone());
        match &sexp.kind {
            SexpKind::Sym(sym) => self.cmp_sym(sym, missing_ok),
            SexpKind::Nil => self.code_buffer.add_instr(BcOp::LDNULL_OP),
            SexpKind::Environment(_) => todo!(),
            SexpKind::Promise => todo!(),
            SexpKind::Lang(_) => todo!(),
            _ => {
                let ci = self.code_buffer.add_const(sexp.clone());
                self.code_buffer.add_instr2(BcOp::LDCONST_OP, ci);
            }
        };

        self.code_buffer.set_current_expr(sexp.clone());

        if self.context.tailcall {
            self.code_buffer.add_instr(BcOp::RETURN_OP);
        }
    }

    fn cmp_sym(&mut self, sym: &lang::Sym, missing_ok: bool) {
        match sym.data.as_str() {
            ".." => todo!(),
            name if name.starts_with("..") => todo!(),
            _ => {
                if !self.var_exist() {
                    todo!()
                }
                let index = self.code_buffer.add_const(sym.clone().into());
                if missing_ok {
                    self.code_buffer.add_instr2(BcOp::GETVAR_MISSOK_OP, index);
                } else {
                    self.code_buffer.add_instr2(BcOp::GETVAR_OP, index);
                }
            }
        }
    }

    fn cmp_lang(&mut self, call: &lang::Lang) {
        match &call.target {
            lang::Target::Lang(lang) => todo!(),
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
    }

    fn cmp_args(&mut self, args: &data::List) {}

    // TODO
    fn var_exist(&self) -> bool {
        true
    }

    // TODO create real source tracking
    // this is only for time being so i could
    // create some compiled code
    fn create_temp_loc() -> Sexp {
        let loc_temp: Sexp = Sexp {
            kind: SexpKind::Int(vec![-2147483648, 0, 0]),
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
        };
        loc_temp
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
    use std::io::{BufReader, Cursor};

    use crate::rds::{rds_reader::RDSReader, RDSResult};

    #[test]
    fn test_basic() {
        let bc_corr: Vec<u8> = vec![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00,
            0x00, 0x00, 0xfd, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x15, 0x00, 0x00, 0x00,
            0x01, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x0c, 0x00,
            0x00, 0x00, 0x11, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00,
            0x00, 0x00, 0x03, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
            0x00, 0x00, 0x05, 0x63, 0x6c, 0x61, 0x73, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00,
            0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x10, 0x65, 0x78, 0x70, 0x72,
            0x65, 0x73, 0x73, 0x69, 0x6f, 0x6e, 0x73, 0x49, 0x6e, 0x64, 0x65, 0x78, 0x00, 0x00,
            0x00, 0xfe,
        ];

        let bc_corr = Cursor::new(bc_corr);
        let mut reader = BufReader::new(bc_corr);
        let RDSResult {
            header: _,
            data: bc_correct,
        } = reader.read_rds().unwrap();
        let Sexp {
            kind: SexpKind::Closure(bc_correct),
            ..
        } = bc_correct
        else {
            panic!("This is not bc");
        };

        let source: Vec<u8> = vec![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00,
            0x00, 0x00, 0xfd, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe,
        ];

        let source = Cursor::new(source);
        let mut reader = BufReader::new(source);
        let RDSResult {
            header: _,
            data: source,
        } = reader.read_rds().unwrap();
        let Sexp {
            kind: SexpKind::Closure(source),
            ..
        } = source
        else {
            panic!("Source must be closure");
        };

        let compiler = Compiler::new();

        let bc = compiler.cmpfun(source);

        assert_eq!(bc, bc_correct);
    }
}
