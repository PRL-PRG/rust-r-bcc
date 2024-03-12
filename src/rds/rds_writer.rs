use std::io::{BufWriter, Write};

use crate::sexp::sexp::{data, lang, MetaData, Sexp, SexpKind};

use super::{Flag, RDSHeader, RefsTable};

#[derive(Debug)]
pub enum RDSWriterError {
    DataError(String),
    IO(std::io::Error),
}

impl From<std::io::Error> for RDSWriterError {
    fn from(value: std::io::Error) -> Self {
        RDSWriterError::IO(value)
    }
}

type Ret = Result<(), RDSWriterError>;

pub trait RDSWriter: Write {
    fn write_byte(&mut self, byte: u8) -> Ret {
        let len = self.write(&[byte])?;
        if len != 1 {
            return Err(RDSWriterError::DataError("Cannot write byte".into()));
        }
        Ok(())
    }

    fn write_int(&mut self, value: i32) -> Ret {
        let data = i32::to_be_bytes(value);
        let len = self.write(&data)?;
        if len != 4 {
            return Err(RDSWriterError::DataError("Cannot write int".into()));
        }
        Ok(())
    }

    fn write_double(&mut self, value: f64) -> Ret {
        let data = f64::to_be_bytes(value);
        let len = self.write(&data)?;
        if len != 8 {
            return Err(RDSWriterError::DataError("Cannot write double".into()));
        }
        Ok(())
    }

    fn write_str(&mut self, value: &str) -> Ret {
        self.write_int(value.len() as i32)?;
        for val in value.bytes() {
            self.write_byte(val)?;
        }
        Ok(())
    }

    fn write_header(&mut self, header: RDSHeader) -> Ret {
        self.write_byte(b'X')?;
        self.write_byte(b'\n')?;
        self.write_int(header.format_version)?;

        self.write_int(header.writer_version)?;

        self.write_int(header.min_reader_version)?;
        if header.format_version == 3 {
            self.write_str("UTF-8")?;
        }
        Ok(())
    }

    fn write_flags(&mut self, flags: Flag) -> Ret {
        //println!("flag : {}", flags.sexp_type);
        let mut res: i32 = flags.sexp_type as i32;
        res |= flags.level << 12;
        if flags.has_attributes {
            res |= 1 << 9;
        }

        if flags.has_tag {
            res |= 1 << 10;
        }

        if flags.obj {
            res |= 1 << 8;
        }

        self.write_int(res)
    }

    fn write_len(&mut self, len: usize) -> Ret {
        // check if len is long vec length
        if len > 2147483647 {
            self.write_int(-1)?;
            self.write_int((len / 4294967296) as i32)?;
            self.write_int((len % 4294967296) as i32)
        } else {
            //println!("len : {}", len);
            self.write_int(len as i32)
        }
    }

    fn write_intvec(&mut self, ints: &Vec<i32>) -> Ret {
        self.write_len(ints.len())?;

        for val in ints {
            self.write_int(*val)?;
        }
        Ok(())
    }

    fn write_rds(&mut self, header: RDSHeader, sexp: Sexp) -> Ret {
        self.write_header(header)?;
        self.write_item(&sexp, &mut None)?;
        Ok(())
    }

    fn write_item(&mut self, sexp: &Sexp, refs: &mut Option<RefsTable>) -> Ret {
        // when creating refs there are different rules for creating
        // flags so it must be done seperatelly
        match refs {
            // it is done this way because borrow checker
            Some(table) => {
                if let SexpKind::Sym(_) = sexp.kind {
                    let idx = table.add_ref(sexp.clone()) + 1;
                    self.write_int((idx << 8) | super::sexptype::REFSXP as i32)?;
                    return Ok(());
                }
            }
            _ => (),
        };

        let flag: Flag = sexp.into();
        if flag.sexp_type != super::sexptype::LISTSXP {
            self.write_flags(flag)?;
        }

        match &sexp.kind {
            SexpKind::Sym(sym) => {
                self.write_int(
                    super::string_format::ASCII << 12 | super::sexptype::CHARSXP as i32,
                )?;
                self.write_charsxp(sym.data.as_str())
            }
            SexpKind::List(taggedlist) => {
                self.write_listsxp(taggedlist, &sexp.metadata, flag, refs)
            }
            SexpKind::Nil => Ok(()),
            SexpKind::Closure(closure) => self.write_closxp(closure, &sexp.metadata, refs),
            SexpKind::Environment(lang::Environment::Normal(_)) => todo!(),
            SexpKind::Environment(_) => Ok(()),
            SexpKind::Promise => todo!(),
            SexpKind::Lang(_) => todo!(),
            SexpKind::Bc(bc) => {
                // TODO need to find out what reps does
                let reps = 1;
                self.write_int(reps)?;
                self.write_int(super::sexptype::INTSXP as i32)?;
                self.write_intvec(&bc.instructions)?;
                self.write_bcconsts(&bc.constpool, &mut None)
            }
            SexpKind::Char(chars) => {
                // lenght of the char vector is limited
                self.write_int(chars.len() as i32)?;

                for val in chars {
                    self.write_byte(*val as u8)?;
                }

                Ok(())
            }
            SexpKind::Logic(_) => todo!(),
            SexpKind::Real(reals) => {
                self.write_len(reals.len())?;

                for val in reals {
                    self.write_double(*val)?;
                }
                Ok(())
            }
            SexpKind::Int(ints) => self.write_intvec(ints),
            SexpKind::Complex(_) => todo!(),
            SexpKind::Str(strs) => {
                self.write_len(strs.len())?;
                for s in strs {
                    self.write_int(
                        super::string_format::ASCII << 12 | super::sexptype::CHARSXP as i32,
                    )?;
                    self.write_charsxp(s.as_str())?;
                }
                Ok(())
            }
            SexpKind::Vec(items) => {
                self.write_len(items.len())?;

                for item in items {
                    self.write_item(item, refs)?;
                }
                Ok(())
            }
            SexpKind::MissingArg => Ok(()),
        }?;
        if flag.has_attributes {
            let Some(attr) = sexp.metadata.attr.clone() else {
                unreachable!()
            };
            self.write_item(&attr, refs)?;
        }
        Ok(())
    }

    fn write_charsxp(&mut self, data: &str) -> Ret {
        self.write_int(data.len() as i32)?;
        for b in data.bytes() {
            self.write_byte(b)?;
        }
        Ok(())
    }

    // TODO implement more options for consts
    // but since I have only implemented reading
    // only for default I will do the same for writing
    fn write_bcconsts(&mut self, consts: &Vec<Sexp>, refs: &mut Option<RefsTable>) -> Ret {
        self.write_int(consts.len() as i32)?;
        for c in consts {
            // here is the place that will
            // for sure need to change to
            // allow more types
            let flags: Flag = c.into();
            if flags.sexp_type == super::sexptype::NILVALUE_SXP {
                self.write_int(super::sexptype::NILSXP as i32)?;
            } else {
                self.write_int(flags.sexp_type as i32)?
            }
            self.write_item(c, refs)?;
        }
        Ok(())
    }

    fn write_listsxp(
        &mut self,
        list: &data::List,
        metadata: &MetaData,
        flag: Flag,
        refs: &mut Option<RefsTable>,
    ) -> Ret {
        let mut flag = flag;
        if list.is_empty() {
            return self.write_int(super::sexptype::NILVALUE_SXP as i32);
        }

        for item in list {
            flag.has_tag = item.tag.is_some();
            self.write_flags(flag.clone())?;

            if let Some(tag) = &item.tag {
                let tag: lang::Sym = tag.as_str().into();
                let tag: Sexp = tag.into();
                self.write_item(&tag, refs)?;
            }

            flag = Flag {
                sexp_type: super::sexptype::LISTSXP,
                level: 0,
                has_attributes: false,
                has_tag: false,
                obj: false,
                orig: 0,
            };

            self.write_item(&item.data, refs)?;
        }
        self.write_int(super::sexptype::NILVALUE_SXP as i32)?;

        Ok(())
    }

    fn write_formals(&mut self, formals: &Vec<lang::Formal>, refs: &mut Option<RefsTable>) -> Ret {
        if formals.is_empty() {
            self.write_int(super::sexptype::NILVALUE_SXP as i32)
        } else {
            let vals: data::List = formals
                .into_iter()
                .map(|x| data::TaggedSexp::new_with_tag(*x.value.clone(), x.name.data.clone()))
                .collect();
            self.write_listsxp(
                &vals,
                &MetaData::default(),
                Flag {
                    sexp_type: super::sexptype::LISTSXP,
                    level: 0,
                    has_attributes: false,
                    has_tag: false,
                    obj: false,
                    orig: 0,
                },
                refs,
            )
        }
    }

    fn write_closxp(
        &mut self,
        closure: &lang::Closure,
        metadata: &MetaData,
        refs: &mut Option<RefsTable>,
    ) -> Ret {
        if let Some(attr) = &metadata.attr {
            self.write_item(&attr, refs)?;
        }

        self.write_item(&closure.environment.clone().into(), refs)?;
        self.write_formals(&closure.formals, refs)?;

        let mut reftable = RefsTable::default();
        for formal in closure.formals.iter() {
            reftable.add_ref(formal.name.clone().into());
        }

        self.write_item(&closure.body, &mut Some(reftable))?;

        Ok(())
    }
}

impl<W> RDSWriter for BufWriter<W> where W: std::io::Write {}
