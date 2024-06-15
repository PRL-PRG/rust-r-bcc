use crate::sexp::{
    bc::{Bc, BcOp},
    sexp::Sexp,
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

pub struct CodeBuffer<'a> {
    pub bc: Bc<'a>,
    pub current_expr: Option<&'a Sexp<'a>>,
    pub expression_buffer: Vec<i32>,
    labels: Vec<Label>,
}

impl CodeBuffer {
    pub fn new() -> Self {
        Self {
            bc: Bc::new(),
            current_expr: None,
            expression_buffer: vec![NA], // first instruction is version and does not have a source
            labels: vec![],
        }
    }

    pub fn new_with_expr(curr_expr: Sexp) -> Self {
        Self {
            bc: Bc::new(),
            current_expr: Some(curr_expr),
            expression_buffer: vec![NA], // first instruction is version and does not have a source
            labels: vec![],
        }
    }

    pub fn insert_currexpr(&mut self, bc_count: usize) {
        if self.current_expr.is_some() {
            let expr = self.current_expr.clone().unwrap();
            let index = self.add_const(expr.clone());
            self.expression_buffer
                .append(&mut (0..bc_count).map(|_| index).collect());
        }
    }

    pub fn add_instr(&mut self, op: BcOp) {
        assert_eq!(op.arity(), 0, "Wrong arity");
        self.bc.instructions.push(op.into());
        self.insert_currexpr(1);
    }

    pub fn add_instr2(&mut self, op: BcOp, idx: i32) {
        assert_eq!(op.arity(), 1, "Wrong arity");
        self.bc.instructions.push(op.into());
        self.bc.instructions.push(idx);
        self.insert_currexpr(2);
    }

    pub fn add_instr_n(&mut self, op: BcOp, idxs: &[i32]) {
        assert_eq!(op.arity(), idxs.len(), "Wrong arity");
        self.bc.instructions.push(op.into());
        self.bc.instructions.extend_from_slice(idxs);
        self.insert_currexpr(1 + idxs.len());
    }

    pub fn add_const(&mut self, val: Sexp) -> i32 {
        match self.bc.constpool.iter().position(|x| x == &val) {
            Some(idx) => idx as i32,
            None => {
                self.bc.constpool.push(val);
                (self.bc.constpool.len() - 1) as i32
            }
        }
    }

    pub fn set_current_expr(&mut self, sexp: Sexp) -> Option<Sexp> {
        std::mem::replace(&mut self.current_expr, Some(sexp))
    }

    pub fn restore_current_expr(&mut self, orig: Option<Sexp>) {
        let _ = std::mem::replace(&mut self.current_expr, orig);
    }

    pub fn make_label(&mut self) -> LabelIdx {
        self.labels.push(Label::default());
        self.labels.len() - 1
    }

    pub fn set_label(&mut self, label: LabelIdx) {
        self.labels[label]
            .positions
            .push(self.bc.instructions.len() - 1)
    }

    pub fn put_label(&mut self, label: LabelIdx) {
        self.labels[label as usize].value = self.bc.instructions.len() as i32;
    }

    pub fn patch_labels(&mut self) {
        for label in &self.labels {
            for pos in &label.positions {
                self.bc.instructions[*pos] = label.value;
            }
        }
    }
}
