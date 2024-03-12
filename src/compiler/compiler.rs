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

    pub fn cmpfun(self, closure: lang::Closure) -> lang::Closure {
        let mut closure = closure;
        let body = SexpKind::Bc(self.gen_code(*closure.body)).into();
        closure.body = Box::new(body);
        closure
    }

    fn gen_code(self, target: Sexp) -> Bc {
        let mut tmp = self;
        tmp.code_buffer.add_const(target.clone());
        tmp.cmp(target);
        tmp.code_buffer.add_const(Compiler::create_temp_loc());
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
