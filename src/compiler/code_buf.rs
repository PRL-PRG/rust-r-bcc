use crate::sexp::{
    bc::{Bc, BcOp, ConstPoolItem},
    sexp::{lang, Sexp},
    sexp_alloc::Alloc,
};

/// i32 min represents NA in intepreter
const NA: i32 = i32::MIN;

// placeholder label which is inserted before patching
// I choose this value since I wanted to be easily visible
pub const DEFLABEL: i32 = 0xeeeeeee;

pub type LabelIdx = usize;

#[derive(Default)]
struct Label {
    value: i32,
    positions: Vec<usize>,
}

// code buffer is mutable Bc
// with additional data
pub struct CodeBuffer<'a> {
    instructions: Vec<i32>,
    constpool: Vec<ConstPoolItem<'a>>,
    pub current_expr: Option<ConstPoolItem<'a>>,
    pub expression_buffer: Vec<i32>,
    labels: Vec<Label>,
}

impl<'a> CodeBuffer<'a> {
    pub fn new() -> Self {
        Self {
            instructions: vec![Bc::version()],
            constpool: vec![],
            current_expr: None,
            expression_buffer: vec![NA], // first instruction is version and does not have a source
            labels: vec![],
        }
    }

    pub fn new_with_expr(curr_expr: ConstPoolItem<'a>) -> Self {
        Self {
            instructions: vec![Bc::version()],
            constpool: vec![],
            current_expr: Some(curr_expr),
            expression_buffer: vec![NA], // first instruction is version and does not have a source
            labels: vec![],
        }
    }

    pub fn insert_currexpr(&mut self, bc_count: usize) {
        if self.current_expr.is_some() {
            let expr = self.current_expr.unwrap();
            let index = self.add_const(expr);
            self.expression_buffer
                .append(&mut (0..bc_count).map(|_| index).collect());
        }
    }

    pub fn add_instr(&mut self, op: BcOp) {
        assert_eq!(op.arity(), 0, "Wrong arity");
        self.instructions.push(op.into());
        self.insert_currexpr(1);
    }

    pub fn add_instr2(&mut self, op: BcOp, idx: i32) {
        assert_eq!(op.arity(), 1, "Wrong arity");
        self.instructions.push(op.into());
        self.instructions.push(idx);
        self.insert_currexpr(2);
    }

    pub fn add_instr_n(&mut self, op: BcOp, idxs: &[i32]) {
        assert_eq!(op.arity(), idxs.len(), "Wrong arity");
        self.instructions.push(op.into());
        self.instructions.extend_from_slice(idxs);
        self.insert_currexpr(1 + idxs.len());
    }

    pub fn add_const(&mut self, val: ConstPoolItem<'a>) -> i32 {
        match val {
            ConstPoolItem::Sexp(sexp) => self.add_const_sexp(sexp),
            ConstPoolItem::Sym(sym) => self.add_const_sym(sym),
            ConstPoolItem::Lang(lang) => self.add_const_lang(lang),
        }
    }

    pub fn add_const_sexp(&mut self, val: &'a Sexp<'a>) -> i32 {
        match self.constpool.iter().position(|x| x == val) {
            Some(idx) => idx as i32,
            None => {
                self.constpool.push(ConstPoolItem::Sexp(val));
                (self.constpool.len() - 1) as i32
            }
        }
    }

    pub fn add_const_lang(&mut self, val: &'a lang::Lang<'a>) -> i32 {
        match self.constpool.iter().position(|x| x == val) {
            Some(idx) => idx as i32,
            None => {
                self.constpool.push(ConstPoolItem::Lang(val));
                (self.constpool.len() - 1) as i32
            }
        }
    }

    pub fn add_const_sym(&mut self, val: &'a lang::Sym<'a>) -> i32 {
        match self.constpool.iter().position(|x| x == val) {
            Some(idx) => idx as i32,
            None => {
                self.constpool.push(ConstPoolItem::Sym(val));
                (self.constpool.len() - 1) as i32
            }
        }
    }

    pub fn set_current_expr(&mut self, sexp: ConstPoolItem<'a>) -> Option<ConstPoolItem<'a>> {
        std::mem::replace(&mut self.current_expr, Some(sexp))
    }

    pub fn restore_current_expr(&mut self, orig: Option<ConstPoolItem<'a>>) {
        self.current_expr = orig;
    }

    pub fn make_label(&mut self) -> LabelIdx {
        self.labels.push(Label::default());
        self.labels.len() - 1
    }

    pub fn set_label(&mut self, label: LabelIdx) {
        self.labels[label]
            .positions
            .push(self.instructions.len() - 1)
    }

    pub fn put_label(&mut self, label: LabelIdx) {
        self.labels[label as usize].value = self.instructions.len() as i32;
    }

    fn patch_labels(&mut self) {
        for label in &self.labels {
            for pos in &label.positions {
                self.instructions[*pos] = label.value;
            }
        }
    }

    pub fn create_bc(&mut self, arena: &'a Alloc<'a>) -> Bc<'a> {
        self.patch_labels();

        //assert!(self.check_uniqueness_const());

        Bc::new(
            arena.alloc_slice_copy(self.instructions.as_slice()),
            arena.alloc_slice_copy(self.constpool.as_slice()),
        )
    }
}
