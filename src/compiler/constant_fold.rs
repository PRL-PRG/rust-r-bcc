use std::cmp::max;
use std::fmt::Display;
use std::iter::{repeat_n, zip};
use either::Either;
use crate::sexp::sexp::{Sexp, SexpKind};
use crate::sexp::sexp::data::{Complex, Double, List, Logic, RVec, TaggedSexp};
use crate::sexp::sexp::lang::Sym;
use crate::sexp::sexp_alloc::Alloc;

pub struct ConstantFold<'a> {
    arena: &'a Alloc<'a>,
}

impl<'a> ConstantFold<'a> {
    pub fn new(arena: &'a Alloc<'a>) -> Self {
        Self { arena }
    }

    pub fn paren(&self, inner: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        Some(inner)
    }

    pub fn c(&self, args: &[TaggedSexp<'a>]) -> Option<&'a Sexp<'a>> {
        if args.is_empty() {
            return Some(self.alloc(SexpKind::Nil));
        }

        let sexp = if args.iter().all(|a| matches!(&a.data.kind, SexpKind::Logic(_))) {
            Some(self.alloc(SexpKind::Logic(
                self.alloc_slice(args.iter().flat_map(|a| match &a.data.kind {
                    SexpKind::Logic(l) => l.iter().copied(),
                    _ => unreachable!(),
                }))
            )))
        } else if args.iter().all(|a| matches!(&a.data.kind, SexpKind::Logic(_) | SexpKind::Int(_))) {
            Some(self.alloc(SexpKind::Int(
                RVec::new(self.alloc_slice(args.iter().flat_map(|a| match &a.data.kind {
                    SexpKind::Logic(l) => Either::Left(l.iter().map(i32::from)),
                    SexpKind::Int(i) => Either::Right(i.iter().copied()),
                    _ => unreachable!(),
                })))
            )))
        } else if args.iter().all(|a| matches!(&a.data.kind, SexpKind::Logic(_) | SexpKind::Int(_) | SexpKind::Real(_))) {
            Some(self.alloc(SexpKind::Real(
                RVec::new(self.alloc_slice(args.iter().flat_map(|a| match &a.data.kind {
                    SexpKind::Logic(l) => Either::Left(Either::Left(l.iter().map(i32::from).map(Double::from))),
                    SexpKind::Int(i) => Either::Left(Either::Right(i.iter().copied().map(Double::from))),
                    SexpKind::Real(r) => Either::Right(r.iter().copied()),
                    _ => unreachable!(),
                })))
            )))
        } else if args.iter().all(|a| matches!(&a.data.kind, SexpKind::Logic(_) | SexpKind::Int(_) | SexpKind::Real(_) | SexpKind::Complex(_))) {
            Some(self.alloc(SexpKind::Complex(
                RVec::new(self.alloc_slice(args.iter().flat_map(|a| match &a.data.kind {
                    SexpKind::Logic(l) => Either::Left(Either::Left(l.iter().map(i32::from).map(Double::from).map(Complex::from))),
                    SexpKind::Int(i) => Either::Left(Either::Right(i.iter().copied().map(Double::from).map(Complex::from))),
                    SexpKind::Real(r) => Either::Right(Either::Left(r.iter().copied().map(Complex::from))),
                    SexpKind::Complex(c) => Either::Right(Either::Right(c.iter().copied())),
                    _ => unreachable!(),
                })))
            )))
        } else if args.iter().all(|a| matches!(&a.data.kind, SexpKind::Logic(_) | SexpKind::Int(_) | SexpKind::Real(_) | SexpKind::Complex(_) | SexpKind::Str(_))) {
            Some(self.alloc(SexpKind::Str(
                RVec::new(self.alloc_slice(args.iter().flat_map(|a| match &a.data.kind {
                    SexpKind::Logic(l) => Either::Left(Either::Left(Either::Left(l.iter().map(|l| self.alloc_str(l))))),
                    SexpKind::Int(i) => Either::Left(Either::Left(Either::Right(i.iter().map(|i| self.alloc_str(i))))),
                    SexpKind::Real(r) => Either::Left(Either::Right(Either::Left(r.iter().map(|r| self.alloc_str(r))))),
                    SexpKind::Complex(c) => Either::Left(Either::Right(Either::Right(c.iter().map(|c| self.alloc_str(c))))),
                    SexpKind::Str(s) => Either::Right(s.iter().copied()),
                    _ => unreachable!(),
                })))
            )))
        } else {
            None
        }?;

        // Set names
        if args.iter().any(|a| a.tag.is_some()) {
            *sexp.metadata.get_attr_mut() = Some(
                self.alloc(SexpKind::List(List {
                    data: self.arena.alloc([TaggedSexp::new_with_tag(
                        self.alloc(SexpKind::Str(
                            RVec::new(self.alloc_slice(args.iter().map(|a| {
                                a.tag.map(|t| t.data).unwrap_or("")
                            })))
                        )),
                        self.arena.alloc(Sym { data: "names" }),
                    )]),
                }))
            )
        }

        Some(sexp)
    }

    pub fn plus(&self, arg: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math1(MathOp1::Plus, arg)
    }

    pub fn add(&self, lhs: &'a Sexp<'a>, rhs: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math2(MathOp2::Add, lhs, rhs)
    }

    pub fn minus(&self, arg: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math1(MathOp1::Minus, arg)
    }

    pub fn sub(&self, lhs: &'a Sexp<'a>, rhs: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math2(MathOp2::Sub, lhs, rhs)
    }

    pub fn mul(&self, lhs: &'a Sexp<'a>, rhs: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math2(MathOp2::Mul, lhs, rhs)
    }

    pub fn div(&self, lhs: &'a Sexp<'a>, rhs: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math2(MathOp2::Div, lhs, rhs)
    }

    pub fn colon(&self, min: &'a Sexp<'a>, max: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        let min = i32::from(NumberScalarSxp::try_from(min).ok()?);
        let max = i32::from(NumberScalarSxp::try_from(max).ok()?);

        Some(if min < max {
            self.alloc(SexpKind::Int(RVec::new(self.alloc_slice(min..=max))))
        } else {
            self.alloc(SexpKind::Int(RVec::new(self.alloc_slice((max..=min).rev()))))
        })
    }

    pub fn pow(&self, lhs: &'a Sexp<'a>, rhs: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math2(MathOp2::Pow, lhs, rhs)
    }

    pub fn log(&self, arg: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math1(MathOp1::Log, arg)
    }

    pub fn log2(&self, arg: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math1(MathOp1::Log2, arg)
    }

    pub fn sqrt(&self, arg: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        self.math1(MathOp1::Sqrt, arg)
    }

    pub fn rep(&self, x: &'a Sexp<'a>, times: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        let x = PrimVecSxp::try_from(x).ok()?;
        let times = NumberVecSxp::try_from(times).ok()?;

        if times.len() == 1 {
            Some(match x {
                PrimVecSxp::Logic(x) => self.alloc(SexpKind::Logic(
                    self.alloc_slice(
                        x.iter().copied().cycle().take(times.index_int(0) as usize)
                    )
                )),
                PrimVecSxp::Number(NumberVecSxp::Int(x)) => self.alloc(SexpKind::Int(
                    RVec::new(self.alloc_slice(
                        x.iter().copied().cycle().take(times.index_int(0) as usize)
                    ))
                )),
                PrimVecSxp::Number(NumberVecSxp::Double(x)) => self.alloc(SexpKind::Real(
                    RVec::new(self.alloc_slice(
                        x.iter().copied().cycle().take(times.index_int(0) as usize)
                    ))
                )),
                PrimVecSxp::Number(NumberVecSxp::Complex(x)) => self.alloc(SexpKind::Complex(
                    RVec::new(self.alloc_slice(
                        x.iter().copied().cycle().take(times.index_int(0) as usize)
                    ))
                )),
                PrimVecSxp::String(x) => self.alloc(SexpKind::Str(
                    RVec::new(self.alloc_slice(
                        x.iter().copied().cycle().take(times.index_int(0) as usize)
                    ))
                )),
            })
        } else if times.len() == x.len() {
            Some(match x {
                PrimVecSxp::Logic(x) => self.alloc(SexpKind::Logic(
                    self.alloc_slice(
                        zip(x.iter().copied(), times.iter_ints()).flat_map(|(x, t)| repeat_n(x, t as usize))
                    )
                )),
                PrimVecSxp::Number(NumberVecSxp::Int(x)) => self.alloc(SexpKind::Int(
                    RVec::new(self.alloc_slice(
                        zip(x.iter().copied(), times.iter_ints()).flat_map(|(x, t)| repeat_n(x, t as usize))
                    ))
                )),
                PrimVecSxp::Number(NumberVecSxp::Double(x)) => self.alloc(SexpKind::Real(
                    RVec::new(self.alloc_slice(
                        zip(x.iter().copied(), times.iter_ints()).flat_map(|(x, t)| repeat_n(x, t as usize))
                    ))
                )),
                PrimVecSxp::Number(NumberVecSxp::Complex(x)) => self.alloc(SexpKind::Complex(
                    RVec::new(self.alloc_slice(
                        zip(x.iter().copied(), times.iter_ints()).flat_map(|(x, t)| repeat_n(x, t as usize))
                    ))
                )),
                PrimVecSxp::String(x) => self.alloc(SexpKind::Str(
                    RVec::new(self.alloc_slice(
                        zip(x.iter().copied(), times.iter_ints()).flat_map(|(x, t)| repeat_n(x, t as usize))
                    ))
                )),
            })
        } else {
            None
        }
    }

    pub fn seq_int(&self, from: &'a Sexp<'a>, to: &'a Sexp<'a>, by: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        let from = NumberScalarSxp::try_from(from).ok()?;
        let to = NumberScalarSxp::try_from(to).ok()?;
        let by = NumberScalarSxp::try_from(by).ok()?;

        match (from, to, by) {
            (NumberScalarSxp::Int(from), NumberScalarSxp::Int(to), NumberScalarSxp::Int(by)) => {
                let seq = (from..=to).step_by(by as usize);
                
                Some(self.alloc(SexpKind::Int(RVec::new(self.alloc_slice(seq)))))
            }
            (
                from @ (NumberScalarSxp::Int(_) | NumberScalarSxp::Double(_)),
                to @ (NumberScalarSxp::Int(_) | NumberScalarSxp::Double(_)),
                by @ (NumberScalarSxp::Int(_) | NumberScalarSxp::Double(_)),
            ) => {
                let from = Double::from(from);
                let to = Double::from(to);
                let by = Double::from(by);
                
                let size = i32::from((to - from) / by) + 1;
                let seq = (0..size).map(|i| from + Double::from(i) * by);

                Some(self.alloc(SexpKind::Real(RVec::new(self.alloc_slice(seq)))))
            }
            _ => None
        }
    }

    fn math1(&self, op: MathOp1, arg: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        Some(self.do_math1(op, NumberVecSxp::try_from(arg).ok()?))
    }

    fn math2(&self, op: MathOp2, lhs: &'a Sexp<'a>, rhs: &'a Sexp<'a>) -> Option<&'a Sexp<'a>> {
        Some(self.do_math2(op, NumberVecSxp::try_from(lhs).ok()?, NumberVecSxp::try_from(rhs).ok()?))
    }

    fn do_math1(&self, op: MathOp1, arg: NumberVecSxp<'a>) -> &'a Sexp<'a> {
        match (op, arg) {
            // `MathOp1::Plus` is a no-op
            (MathOp1::Plus, NumberVecSxp::Int(i)) => self.alloc(SexpKind::Int(
                RVec::new(self.alloc_slice(i.iter().copied()))
            )),
            (MathOp1::Plus, NumberVecSxp::Double(d)) => self.alloc(SexpKind::Real(
                RVec::new(self.alloc_slice(d.iter().copied()))
            )),
            (MathOp1::Plus, NumberVecSxp::Complex(c)) => self.alloc(SexpKind::Complex(
                RVec::new(self.alloc_slice(c.iter().copied()))
            )),
            (MathOp1::Minus, NumberVecSxp::Int(i)) => self.alloc(SexpKind::Int(
                RVec::new(self.alloc_slice(i.iter().copied().map(|i| -i)))
            )),
            (MathOp1::Minus, NumberVecSxp::Double(d)) => self.alloc(SexpKind::Real(
                RVec::new(self.alloc_slice(d.iter().copied().map(|d| -d)))
            )),
            (MathOp1::Minus, NumberVecSxp::Complex(c)) => self.alloc(SexpKind::Complex(
                RVec::new(self.alloc_slice(c.iter().copied().map(|c| -c)))
            )),
            (MathOp1::Log, n) => self.alloc(SexpKind::Real(
                RVec::new(self.alloc_slice(n.iter_doubles().map(|c| c.ln().into())))
            )),
            (MathOp1::Log2, n) => self.alloc(SexpKind::Real(
                RVec::new(self.alloc_slice(n.iter_doubles().map(|c| c.log2().into())))
            )),
            (MathOp1::Sqrt, n) => self.alloc(SexpKind::Real(
                RVec::new(self.alloc_slice(n.iter_doubles().map(|c| c.sqrt().into())))
            )),
        }
    }

    fn do_math2(&self, op: MathOp2, lhs: NumberVecSxp<'a>, rhs: NumberVecSxp<'a>) -> &'a Sexp<'a> {
        // Coerce to same type
        let lhs_rhs = match (lhs, rhs) {
            (NumberVecSxp::Int(lhs), NumberVecSxp::Int(rhs)) => NumberVecSxp2::Int2(lhs, rhs),
            (NumberVecSxp::Double(lhs), NumberVecSxp::Double(rhs)) => NumberVecSxp2::Double2(lhs, rhs),
            (NumberVecSxp::Complex(lhs), NumberVecSxp::Complex(rhs)) => NumberVecSxp2::Complex2(lhs, rhs),
            (NumberVecSxp::Int(lhs), NumberVecSxp::Double(rhs)) => NumberVecSxp2::Double2(
                RVec::new(self.alloc_slice(lhs.iter().copied().map(Double::from))),
                rhs,
            ),
            (NumberVecSxp::Double(lhs), NumberVecSxp::Int(rhs)) => NumberVecSxp2::Double2(
                lhs,
                RVec::new(self.alloc_slice(rhs.iter().copied().map(Double::from))),
            ),
            (NumberVecSxp::Int(lhs), NumberVecSxp::Complex(rhs)) => NumberVecSxp2::Complex2(
                RVec::new(self.alloc_slice(lhs.iter().copied().map(Double::from).map(Complex::from))),
                rhs,
            ),
            (NumberVecSxp::Complex(lhs), NumberVecSxp::Int(rhs)) => NumberVecSxp2::Complex2(
                lhs,
                RVec::new(self.alloc_slice(rhs.iter().copied().map(Double::from).map(Complex::from))),
            ),
            (NumberVecSxp::Double(lhs), NumberVecSxp::Complex(rhs)) => NumberVecSxp2::Complex2(
                RVec::new(self.alloc_slice(lhs.iter().copied().map(Complex::from))),
                rhs,
            ),
            (NumberVecSxp::Complex(lhs), NumberVecSxp::Double(rhs)) => NumberVecSxp2::Complex2(
                lhs,
                RVec::new(self.alloc_slice(rhs.iter().copied().map(Complex::from))),
            ),
        };

        match (op, lhs_rhs) {
            (MathOp2::Add, NumberVecSxp2::Int2(lhs, rhs)) => {
                self.alloc(SexpKind::Int(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l + r)))
                ))
            }
            (MathOp2::Add, NumberVecSxp2::Double2(lhs, rhs)) => {
                self.alloc(SexpKind::Real(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l + r)))
                ))
            }
            (MathOp2::Add, NumberVecSxp2::Complex2(lhs, rhs)) => {
                self.alloc(SexpKind::Complex(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l + r)))
                ))
            }
            (MathOp2::Sub, NumberVecSxp2::Int2(lhs, rhs)) => {
                self.alloc(SexpKind::Int(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l - r)))
                ))
            }
            (MathOp2::Sub, NumberVecSxp2::Double2(lhs, rhs)) => {
                self.alloc(SexpKind::Real(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l - r)))
                ))
            }
            (MathOp2::Sub, NumberVecSxp2::Complex2(lhs, rhs)) => {
                self.alloc(SexpKind::Complex(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l - r)))
                ))
            }
            (MathOp2::Mul, NumberVecSxp2::Int2(lhs, rhs)) => {
                self.alloc(SexpKind::Int(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l * r)))
                ))
            }
            (MathOp2::Mul, NumberVecSxp2::Double2(lhs, rhs)) => {
                self.alloc(SexpKind::Real(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l * r)))
                ))
            }
            (MathOp2::Mul, NumberVecSxp2::Complex2(lhs, rhs)) => {
                self.alloc(SexpKind::Complex(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l * r)))
                ))
            }
            (MathOp2::Div, NumberVecSxp2::Int2(lhs, rhs)) => {
                self.alloc(SexpKind::Int(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l / r)))
                ))
            }
            (MathOp2::Div, NumberVecSxp2::Double2(lhs, rhs)) => {
                self.alloc(SexpKind::Real(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l / r)))
                ))
            }
            (MathOp2::Div, NumberVecSxp2::Complex2(lhs, rhs)) => {
                self.alloc(SexpKind::Complex(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l / r)))
                ))
            }
            (MathOp2::Pow, NumberVecSxp2::Int2(lhs, rhs)) => {
                // Yes, it's coerced to double.
                self.alloc(SexpKind::Real(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| Double::from(l).pow(Double::from(r)))))
                ))
            }
            (MathOp2::Pow, NumberVecSxp2::Double2(lhs, rhs)) => {
                self.alloc(SexpKind::Real(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l.pow(r))))
                ))
            }
            (MathOp2::Pow, NumberVecSxp2::Complex2(lhs, rhs)) => {
                self.alloc(SexpKind::Complex(
                    RVec::new(self.alloc_slice(zip_cycle(lhs, rhs).map(|(l, r)| l.pow(r))))
                ))
            }
        }
    }

    fn alloc(&self, kind: SexpKind<'a>) -> &'a Sexp<'a> {
        self.arena.alloc(kind.into())
    }

    fn alloc_slice<T>(&self, elems: impl IntoIterator<Item=T>) -> &'a [T] {
        self.arena.alloc(elems.into_iter().collect::<Vec<_>>()).as_slice()
    }

    fn alloc_str(&self, content: impl Display) -> &'a str {
        self.arena.alloc(content.to_string()).as_str()
    }
}

fn zip_cycle<'a, T: Copy + PartialEq>(lhs: RVec<'a, T>, rhs: RVec<'a, T>) -> impl Iterator<Item=(T, T)> + 'a {
    (0..max(lhs.len(), rhs.len())).map(move |i| (lhs[i % lhs.len()], rhs[i % rhs.len()]))
}

enum MathOp1 {
    Plus,
    Minus,
    Log,
    Log2,
    Sqrt,
}

enum MathOp2 {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

enum PrimVecSxp<'a> {
    Logic(&'a [Logic]),
    Number(NumberVecSxp<'a>),
    String(RVec<'a, &'a str>),
}

enum NumberVecSxp<'a> {
    Int(RVec<'a, i32>),
    Double(RVec<'a, Double>),
    Complex(RVec<'a, Complex>)
}

enum NumberVecSxp2<'a> {
    Int2(RVec<'a, i32>, RVec<'a, i32>),
    Double2(RVec<'a, Double>, RVec<'a, Double>),
    Complex2(RVec<'a, Complex>, RVec<'a, Complex>)
}

enum NumberScalarSxp {
    Int(i32),
    Double(Double),
    Complex(Complex)
}

impl<'a> TryFrom<&'a Sexp<'a>> for PrimVecSxp<'a> {
    type Error = ();

    fn try_from(value: &'a Sexp<'a>) -> Result<Self, Self::Error> {
        match &value.kind {
            SexpKind::Logic(l) => Ok(PrimVecSxp::Logic(*l)),
            SexpKind::Int(i) => Ok(PrimVecSxp::Number(NumberVecSxp::Int(*i))),
            SexpKind::Real(r) => Ok(PrimVecSxp::Number(NumberVecSxp::Double(*r))),
            SexpKind::Complex(c) => Ok(PrimVecSxp::Number(NumberVecSxp::Complex(*c))),
            SexpKind::Str(s) => Ok(PrimVecSxp::String(*s)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<&'a Sexp<'a>> for NumberVecSxp<'a> {
    type Error = ();

    fn try_from(value: &'a Sexp<'a>) -> Result<Self, Self::Error> {
        match &value.kind {
            SexpKind::Int(i) => Ok(NumberVecSxp::Int(*i)),
            SexpKind::Real(r) => Ok(NumberVecSxp::Double(*r)),
            SexpKind::Complex(c) => Ok(NumberVecSxp::Complex(*c)),
            _ => Err(()),
        }
    }
}

impl TryFrom<&Sexp<'_>> for NumberScalarSxp {
    type Error = ();

    fn try_from(value: &Sexp) -> Result<Self, Self::Error> {
        match &value.kind {
            SexpKind::Int(i) if i.len() == 1=> Ok(NumberScalarSxp::Int(i[0])),
            SexpKind::Real(r) if r.len() == 1=> Ok(NumberScalarSxp::Double(r[0])),
            SexpKind::Complex(c) if c.len() == 1=> Ok(NumberScalarSxp::Complex(c[0])),
            _ => Err(()),
        }
    }
}

impl PrimVecSxp<'_> {
    fn len(&self) -> usize {
        match self {
            PrimVecSxp::Logic(l) => l.len(),
            PrimVecSxp::Number(n) => n.len(),
            PrimVecSxp::String(s) => s.len(),
        }
    }
}

impl NumberVecSxp<'_> {
    fn len(&self) -> usize {
        match self {
            NumberVecSxp::Int(i) => i.len(),
            NumberVecSxp::Double(d) => d.len(),
            NumberVecSxp::Complex(c) => c.len(),
        }
    }

    fn index_int(&self, idx: usize) -> i32 {
        match self {
            NumberVecSxp::Int(i) => i[idx],
            NumberVecSxp::Double(d) => d[idx].into(),
            NumberVecSxp::Complex(c) => c[idx].into(),
        }
    }

    fn iter_ints(&self) -> impl Iterator<Item=i32> + '_ {
        match self {
            NumberVecSxp::Int(i) => Either::Left(i.iter().copied()),
            NumberVecSxp::Double(d) => Either::Right(Either::Left(d.iter().copied().map(i32::from))),
            NumberVecSxp::Complex(c) => Either::Right(Either::Right(c.iter().copied().map(i32::from))),
        }
    }

    fn iter_doubles(&self) -> impl Iterator<Item=Double> + '_ {
        match self {
            NumberVecSxp::Int(i) => Either::Left(Either::Left(i.iter().copied().map(Double::from))),
            NumberVecSxp::Double(d) => Either::Right(d.iter().copied()),
            NumberVecSxp::Complex(c) => Either::Left(Either::Right(c.iter().copied().map(Double::from))),
        }
    }
}

impl From<NumberScalarSxp> for i32 {
    fn from(value: NumberScalarSxp) -> i32 {
        match value {
            NumberScalarSxp::Int(i) => i,
            NumberScalarSxp::Double(d) => d.into(),
            NumberScalarSxp::Complex(c) => c.into(),
        }
    }
}

impl From<NumberScalarSxp> for Double {
    fn from(value: NumberScalarSxp) -> Double {
        match value {
            NumberScalarSxp::Int(i) => i.into(),
            NumberScalarSxp::Double(d) => d,
            NumberScalarSxp::Complex(c) => c.into(),
        }
    }
}

impl From<NumberScalarSxp> for Complex {
    fn from(value: NumberScalarSxp) -> Complex {
        match value {
            NumberScalarSxp::Int(i) => i.into(),
            NumberScalarSxp::Double(d) => d.into(),
            NumberScalarSxp::Complex(c) => c,
        }
    }
}