use std::io::{BufWriter, Write};

use crate::sexp::sexp::{data, lang, MetaData, Sexp, SexpKind};

use super::Flag;

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
        println!("{byte}");
        Ok(())
    }

    fn write_int(&mut self, value: i32) -> Ret {
        let data = i32::to_be_bytes(value);
        let len = self.write(&data)?;
        if len != 4 {
            return Err(RDSWriterError::DataError("Cannot write int".into()));
        }
        println!("{value}");
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

    fn write_header(&mut self) -> Ret {
        self.write_byte(b'X')?;
        self.write_byte(b'\n')?;
        self.write_int(3)?;

        // R version
        self.write_int(263168)?;

        // version 3.5.0
        self.write_int(3 * 65536 + 5 * 256 + 0)?;
        Ok(())
    }

    fn write_flags(&mut self, flags: Flag) -> Ret {
        let mut res: i32 = flags.sexp_type as i32;
        res |= flags.level << 12;
        if flags.has_attributes {
            res |= 1 << 9;
        }

        if flags.has_tag {
            res |= 1 << 10;
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
            self.write_int(len as i32)
        }
    }

    fn write_intvec(&mut self, ints: Vec<i32>) -> Ret {
        self.write_len(ints.len())?;

        for val in ints {
            self.write_int(val)?;
        }
        Ok(())
    }

    fn write_rds(&mut self, sexp: Sexp) -> Ret {
        self.write_header()?;
        self.write_str("UTF-8")?;
        self.write_item(sexp)?;
        Ok(())
    }

    fn write_item(&mut self, sexp: Sexp) -> Ret {
        let flag: Flag = (&sexp).into();
        if flag.sexp_type != super::sexptype::LISTSXP {
            self.write_flags(flag)?;
        }
        if flag.has_attributes {
            let Some(attr) = &sexp.metadata.attr else {
                unreachable!()
            };
            self.write_item(*attr.clone())?;
        }
        match sexp.kind {
            SexpKind::Sym(sym) => {
                self.write_int(super::sexptype::CHARSXP as i32)?;
                self.write_charsxp(sym.data.as_str())
            }
            SexpKind::List(taggedlist) => self.write_listsxp(taggedlist, sexp.metadata, flag),
            SexpKind::Nil => Ok(()),
            SexpKind::Closure(closure) => self.write_closxp(closure, sexp.metadata),
            SexpKind::Environment(lang::Environment::Normal(_)) => todo!(),
            SexpKind::Environment(_) => Ok(()),
            SexpKind::Promise => todo!(),
            SexpKind::Lang(_) => todo!(),
            SexpKind::Bc(bc) => {
                // TODO need to find out what reps does
                let reps = 0;
                self.write_int(reps)?;
                self.write_int(super::sexptype::INTSXP as i32)?;
                self.write_intvec(bc.instructions)?;
                self.write_bcconsts(bc.constpool)
            }
            SexpKind::Char(_) => todo!(),
            SexpKind::Logic(_) => todo!(),
            SexpKind::Real(reals) => {
                self.write_len(reals.len())?;

                for val in reals {
                    self.write_double(val)?;
                }
                Ok(())
            }
            SexpKind::Int(ints) => self.write_intvec(ints),
            SexpKind::Complex(_) => todo!(),
            SexpKind::Str(strs) => {
                self.write_len(strs.len())?;
                for s in strs {
                    self.write_int(super::sexptype::CHARSXP as i32)?;
                    self.write_charsxp(s.as_str())?;
                }
                Ok(())
            }
            SexpKind::Vec(_) => todo!(),
            SexpKind::MissingArg => todo!(),
        }
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
    fn write_bcconsts(&mut self, consts: Vec<Sexp>) -> Ret {
        self.write_int(consts.len() as i32)?;
        for c in consts {
            // here is the place that will
            // for sure need to change to
            // allow more types
            self.write_int(0xff)?;
            self.write_item(c)?;
        }
        Ok(())
    }

    fn write_listsxp(&mut self, list: data::List, metadata: MetaData, flag: Flag) -> Ret {
        let mut flag = flag;
        if list.is_empty() {
            return self.write_item(SexpKind::Nil.into());
        }

        for item in list {
            self.write_flags(flag.clone())?;
            flag = Flag {
                sexp_type: super::sexptype::LISTSXP,
                level: 0,
                has_attributes: false,
                has_tag: false,
                orig: 0,
            };

            self.write_item(item.data.clone())?;
        }
        self.write_int(super::sexptype::NILSXP as i32)?;

        Ok(())
    }

    fn write_formals(&mut self, formals: Vec<lang::Formal>) -> Ret {
        if formals.is_empty() {
            self.write_item(SexpKind::Nil.into())
        } else {
            let vals: data::List = formals
                .into_iter()
                .map(|x| data::TaggedSexp::new_with_tag(*x.value, x.name.data))
                .collect();
            self.write_listsxp(
                vals,
                MetaData::default(),
                Flag {
                    sexp_type: super::sexptype::LISTSXP,
                    level: 0,
                    has_attributes: false,
                    has_tag: false,
                    orig: 0,
                },
            )
        }
    }

    fn write_closxp(&mut self, closure: lang::Closure, metadata: MetaData) -> Ret {
        if let Some(attr) = metadata.attr {
            self.write_item(*attr)?;
        }

        self.write_item(closure.environment.into())?;
        self.write_formals(closure.formals)?;
        self.write_item(*closure.body)?;

        Ok(())
    }
}

impl<W> RDSWriter for BufWriter<W> where W: std::io::Write {}

#[cfg(test)]
mod tests {
    use std::io::{BufReader, Cursor};

    use crate::rds::rds_reader::RDSReader;

    use super::*;

    #[test]
    fn test_intsxp() {
        let indata: Vec<u8> = vec![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0d, 0x00,
            0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
        ];

        let data = Cursor::new(indata);
        let mut reader = BufReader::new(data);
        let input = reader.read_rds().unwrap();

        println!("{input:?}");

        let outdata: Vec<u8> = vec![];
        let mut writer = BufWriter::new(outdata);
        writer.write_rds(input).unwrap();
        writer.flush().unwrap();

        assert_eq!(writer.get_ref(), reader.get_ref().get_ref());
    }
}
