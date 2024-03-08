use std::io::BufReader;

use std::io::Read;

use crate::sexp::bc::Bc;
use crate::sexp::sexp::data;
use crate::sexp::sexp::lang;
use crate::sexp::sexp::Sexp;
use crate::sexp::sexp::SexpKind;

use super::sexptype;
use super::Flag;

#[derive(Debug)]
pub enum RDSReaderError {
    DataError(String),
    WrongFlag(i32),
    IO(std::io::Error),
}

impl From<std::io::Error> for RDSReaderError {
    fn from(value: std::io::Error) -> Self {
        RDSReaderError::IO(value)
    }
}

impl From<std::string::FromUtf8Error> for RDSReaderError {
    fn from(value: std::string::FromUtf8Error) -> Self {
        RDSReaderError::DataError(format!("UTF-8 error {value:?}"))
    }
}

#[allow(unused)]
pub struct RDSHeader {
    rds_type: u8,
    format_version: i32,
}

#[derive(Default)]
pub struct RefsTable {
    data: Vec<Sexp>,
}

impl RefsTable {
    fn add_ref(&mut self, data: Sexp) {
        self.data.push(data);
    }

    fn get_ref(&mut self, index: i32) -> Result<Sexp, RDSReaderError> {
        if index < 0 || index > self.data.len() as i32 {
            Err(RDSReaderError::DataError(format!(
                "Wrong ref index {index}"
            )))
        } else {
            Ok(self.data[index as usize].clone())
        }
    }

    fn add_placeholder(&mut self) -> i32 {
        self.data.push(SexpKind::Nil.into());
        (self.data.len() - 1) as i32
    }

    fn update_ref(&mut self, index: i32, data: Sexp) -> Result<(), RDSReaderError> {
        if index < 0 || index > self.data.len() as i32 {
            Err(RDSReaderError::DataError(format!(
                "Wrong ref index {index}"
            )))
        } else {
            self.data[index as usize] = data;
            Ok(())
        }
    }
}

pub trait RDSReader: Read {
    fn read_byte(&mut self) -> Result<u8, RDSReaderError> {
        let mut buf: [u8; 1] = [0];
        self.read(&mut buf)?;
        Ok(buf[0])
    }

    fn read_int(&mut self) -> Result<i32, RDSReaderError> {
        let mut buf: [u8; 4] = [0; 4];
        let len = self.read(&mut buf)?;
        if len != 4 {
            println!(
                "{:?}",
                buf[0..len].into_iter().cloned().collect::<Vec<u8>>()
            );
            panic!();
            return Err(RDSReaderError::DataError("Cannot read int".to_string()));
        }
        Ok(i32::from_be_bytes(buf))
    }

    fn read_double(&mut self) -> Result<f64, RDSReaderError> {
        let mut buf: [u8; 8] = [0; 8];
        let len = self.read(&mut buf)?;
        if len != 8 {
            return Err(RDSReaderError::DataError("Cannot read double".to_string()));
        }
        Ok(f64::from_be_bytes(buf))
    }

    fn read_string_len(&mut self, len: i32) -> Result<String, RDSReaderError> {
        let len = len as usize;
        let mut buf: Vec<u8> = vec![];
        buf.resize(len, 0);
        let read_len = self.read(buf.as_mut_slice())?;
        if len != read_len {
            return Err(RDSReaderError::DataError(format!(
                "Cannot read string of len {len}"
            )));
        }
        Ok(String::from_utf8(buf)?)
    }

    fn read_rds(&mut self) -> Result<Sexp, RDSReaderError> {
        let _ = self.read_header()?;
        let mut refs = RefsTable::default();
        let item = self.read_item(&mut refs)?;

        Ok(item)
    }

    fn read_header(&mut self) -> Result<RDSHeader, RDSReaderError> {
        let rds_type = self.read_byte().expect("cannot open file");
        assert_eq!(rds_type, b'X');
        assert_eq!(self.read_byte()?, b'\n');
        let format_version = self.read_int()?;
        self.read_int()?;
        self.read_int()?;

        let res = RDSHeader {
            rds_type,
            format_version,
        };

        match format_version {
            2 => Ok(res),
            3 => {
                let len = self.read_int()?;
                self.read_string_len(len)?;
                Ok(res)
            }
            _ => Err(RDSReaderError::DataError(format!(
                "Wrong format : {format_version}"
            ))),
        }
    }

    fn read_item(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let flag = self.read_flags()?;
        self.read_item_flags(refs, flag)
    }

    fn read_item_flags(
        &mut self,
        refs: &mut RefsTable,
        flag: Flag,
    ) -> Result<Sexp, RDSReaderError> {
        //println!("flag : {:?}", flag.sexp_type);
        let mut sexp: Sexp = match flag.sexp_type {
            sexptype::NILVALUE_SXP | sexptype::NILSXP => SexpKind::Nil.into(),
            sexptype::REALSXP => self.read_realsxp()?,
            sexptype::INTSXP => self.read_intsxp()?,
            sexptype::LISTSXP => self.read_listsxp(refs, flag)?,
            sexptype::VECSXP => self.read_vecsxp(refs)?,
            sexptype::SYMSXP => self.read_symsxp(refs)?,
            sexptype::STRSXP => self.read_strsxp(refs)?,
            sexptype::CHARSXP => self.read_charsxp()?,
            sexptype::LANGSXP => self.read_langsxp(refs, flag)?,
            sexptype::CLOSXP => self.read_closxp(refs, flag)?,
            sexptype::ENVSXP => self.read_envsxp(refs)?,
            sexptype::GLOBALENV_SXP => {
                let tmp: SexpKind = lang::Environment::Global.into();
                tmp.into()
            }
            sexptype::EMPTYENV_SXP => {
                let tmp: SexpKind = lang::Environment::Empty.into();
                tmp.into()
            }
            sexptype::MISSINGARG_SXP => SexpKind::MissingArg.into(),
            sexptype::REFSXP => self.read_refsxp(refs, flag)?,
            sexptype::BCODESXP => self.read_bc(refs)?,
            x => {
                println!("{x}");
                todo!()
            }
        };

        // the part of the flag used in has attribute is
        // also used in values for REFSXP
        // since ENVSXP has attr every time then we skip it here
        // CLOSXP has attribute in different place
        if flag.has_attributes
            && flag.sexp_type != sexptype::REFSXP
            && flag.sexp_type != sexptype::ENVSXP
            && flag.sexp_type != sexptype::CLOSXP
        {
            sexp.set_attr(self.read_item(refs)?);
        }

        Ok(sexp)
    }

    fn read_flags(&mut self) -> Result<Flag, RDSReaderError> {
        let flag = self.read_int()?;
        let sexp_type: u8 = (flag & 255) as u8;
        let level: i32 = flag >> 12;
        let has_attributes = (flag & (1 << 9)) != 0;
        let has_tag = (flag & (1 << 10)) != 0;

        Ok(Flag {
            sexp_type,
            level,
            has_attributes,
            has_tag,
            orig: flag,
        })
    }

    fn read_len(&mut self) -> Result<usize, RDSReaderError> {
        let len = self.read_int()?;
        //println!("len : {}", len);
        if len < -1 {
            Err(RDSReaderError::DataError(format!(
                "Negative vector len {len}"
            )))
        } else if len >= 0 {
            Ok(len as usize)
        } else {
            // len == -1
            let upper = self.read_int()?;
            let lower = self.read_int()?;
            // in orignal code descriped as sanity check
            if upper > 65536 {
                return Err(RDSReaderError::DataError(format!(
                    "Invalid upper part of vector len {upper}"
                )));
            }

            Ok((upper as usize) << 32 + lower as usize)
        }
    }

    fn read_realsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_double()?)
        }

        Ok(SexpKind::Real(data).into())
    }

    fn read_intsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_int()?)
        }

        Ok(SexpKind::Int(data).into())
    }

    fn read_vecsxp(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_item(refs)?)
        }

        Ok(SexpKind::Vec(data).into())
    }

    fn read_listsxp(&mut self, refs: &mut RefsTable, flags: Flag) -> Result<Sexp, RDSReaderError> {
        // read in order attrib tag and value
        // only value is mandatory

        let mut flags = flags;
        let mut data = vec![];

        loop {
            let attr = if flags.has_attributes {
                Some(self.read_item(refs)?)
            } else {
                None
            };

            let tag = if flags.has_tag {
                Some(self.read_item(refs)?)
            } else {
                None
            };

            let mut value = self.read_item(refs)?;
            if let Some(attr) = attr {
                value.set_attr(attr);
            }

            if let Some(Sexp {
                kind: SexpKind::Sym(tag),
                ..
            }) = tag
            {
                data.push(data::TaggedSexp::new_with_tag(value.into(), tag.data));
            } else {
                data.push(value.into());
            }

            flags = self.read_flags()?;
            if flags.sexp_type != sexptype::LISTSXP {
                let last = self.read_item_flags(refs, flags)?;
                match &last.kind {
                    SexpKind::Nil => (),
                    _ => data.push(last.into()),
                }
                return Ok(SexpKind::List(data).into());
            }
        }
    }

    fn read_symsxp(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let _ = self.read_flags()?;
        let print_name = self.read_charsxp()?;
        if let SexpKind::Char(chars) = print_name.kind {
            let res: Sexp = SexpKind::Sym(lang::Sym::new(String::from_utf8(
                chars.iter().map(|x| *x as u8).collect(),
            )?))
            .into();

            refs.add_ref(res.clone());
            return Ok(res);
        }
        Err(RDSReaderError::DataError(
            "Symsxp must be created from charsxp".into(),
        ))
    }

    fn read_charsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_int()? as usize;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_byte()? as char);
        }

        Ok(SexpKind::Char(data).into())
    }

    fn read_strsxp(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;
        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            let item = self.read_item(refs)?;
            if let Sexp {
                kind: SexpKind::Char(chars),
                ..
            } = item
            {
                data.push(String::from_utf8(chars.iter().map(|x| *x as u8).collect())?);
            } else {
                return Err(RDSReaderError::DataError(
                    "Strsxp must be created from charsxp".into(),
                ));
            }
        }

        Ok(SexpKind::Str(data).into())
    }

    fn read_langsxp(&mut self, refs: &mut RefsTable, flag: Flag) -> Result<Sexp, RDSReaderError> {
        let target = self.read_item(refs)?;

        if flag.has_attributes {
            todo!()
        }

        if flag.has_tag {
            todo!()
        }

        let args = self.read_item(refs)?;

        let args = match args.kind {
            SexpKind::List(list) => list,
            SexpKind::Nil => vec![],
            x => {
                return Err(RDSReaderError::DataError(format!(
                    "Args need to be list got {x:?}"
                )))
            }
        };

        let target = match target.kind {
            SexpKind::Sym(sym) => lang::Target::Sym(sym),
            SexpKind::Lang(lang) => lang::Target::Lang(Box::new(lang)),
            _ => {
                return Err(RDSReaderError::DataError(
                    "Target needs to be either symbol or lang".to_string(),
                ))
            }
        };

        Ok(SexpKind::Lang(lang::Lang::new(target, args)).into())
    }

    fn read_closxp(&mut self, refs: &mut RefsTable, flags: Flag) -> Result<Sexp, RDSReaderError> {
        // order of the values : [attr], enviroment, formals, body
        let attr = if flags.has_attributes {
            Some(self.read_item(refs)?)
        } else {
            None
        };
        let environment = self.read_item(refs)?;
        let formals = self.read_item(refs)?;
        let body = self.read_item(refs)?;

        let mut res: Sexp = match (environment.kind, formals.kind) {
            (SexpKind::Environment(environment), SexpKind::List(formals)) => {
                let formals: Result<Vec<_>, _> =
                    formals.into_iter().map(|x| x.try_into()).collect();
                Ok(SexpKind::Closure(lang::Closure::new(formals?, body, environment)).into())
            }
            (SexpKind::Environment(environment), SexpKind::Nil) => {
                Ok(SexpKind::Closure(lang::Closure::new(vec![], body, environment)).into())
            }
            x => {
                println!("{x:?}");
                Err(RDSReaderError::DataError(
                    "Wrong format of the closure".into(),
                ))
            }
        }?;

        if let Some(attr) = attr {
            res.set_attr(attr)
        }

        Ok(res)
    }

    fn read_envsxp(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let locked = self.read_int()?;

        // just register ref so my indexes are correct
        let index = refs.add_placeholder();

        let parent = self.read_item(refs)?;
        let frame = self.read_item(refs)?;
        let hashtab = self.read_item(refs)?;
        let attr = self.read_item(refs)?;

        println!("{parent:?}");
        println!("{frame:?}");
        println!("{hashtab:?}");
        println!("{attr:?}");

        let res = match parent.kind {
            SexpKind::Environment(env) => {
                let env = lang::NormalEnv::new(Box::new(env), locked == 1);
                let env: lang::Environment = env.into();
                let env: SexpKind = env.into();
                let mut env: Sexp = env.into();
                env.set_attr(attr);
                refs.update_ref(index, env.clone())?;
                Ok(env)
            }
            _ => Err(RDSReaderError::DataError(
                "Parent of environment must be an environment".into(),
            )),
        }?;
        Ok(res)
    }

    fn read_refsxp(&mut self, refs: &mut RefsTable, flags: Flag) -> Result<Sexp, RDSReaderError> {
        let index = flags.orig >> 8;
        let index = if index == 0 { self.read_int()? } else { index } - 1;

        refs.get_ref(index)
    }

    fn read_bc(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        let reps = self.read_int()?;
        let code = self.read_item(refs)?;
        let consts = self.read_bcconsts(refs)?;
        match code.kind {
            SexpKind::Int(ints) => {
                let bc = Bc::new_init(ints, consts);
                Ok(SexpKind::Bc(bc).into())
            }
            _ => todo!(),
        }
    }

    fn read_bcconsts(&mut self, refs: &mut RefsTable) -> Result<Vec<Sexp>, RDSReaderError> {
        let n = self.read_int()?;
        let mut res = vec![];
        for _ in 0..n {
            let type_val = self.read_int()?;
            if type_val > 0xff || type_val < 0 {
                return Err(RDSReaderError::DataError(
                    "Wrong type in byte code constants".into(),
                ));
            }
            let type_val = type_val as u8;
            let tmp = match type_val {
                sexptype::BCODESXP => todo!(),
                sexptype::LANGSXP
                | sexptype::LISTSXP
                | sexptype::BCREPDEF
                | sexptype::BCREPREF
                | sexptype::ATTRLANGSXP
                | sexptype::ATTRLISTSXP => self.read_bclang(refs)?,
                _ => self.read_item(refs)?,
            };
            res.push(tmp)
        }
        Ok(res)
    }

    fn read_bclang(&mut self, refs: &mut RefsTable) -> Result<Sexp, RDSReaderError> {
        todo!()
    }
}

impl<T> RDSReader for BufReader<T> where T: Sized + std::io::Read {}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    #[macro_export]
    macro_rules! test_data {
        ( $( $x:expr ),* $(,)?) => {
            let data : Vec<u8> = vec![$($x,)*];

            let data = Cursor::new(data);
            let mut reader = BufReader::new(data);
            let res = reader.read_rds().unwrap();
            insta::assert_debug_snapshot!(res)
        }
    }

    #[test]
    fn test_header() {
        let data: Vec<u8> = vec![b'X', b'\n', 0, 0, 0, 2, 1, 1, 1, 1, 2, 2, 2, 2];
        let data = Cursor::new(data);
        let mut reader = BufReader::new(data);
        reader.read_header().unwrap();
    }

    #[test]
    fn test_real() {
        // 1
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0e, 0x00,
            0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        // c(1, 2)
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0e, 0x00,
            0x00, 0x00, 0x02, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00,
        ];
    }

    #[test]
    fn test_intsxp() {
        // as.integer(c(1, 2))
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0d, 0x00,
            0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
        ];
    }

    #[test]
    fn test_vecsxp() {
        // list(1, 2)
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x13, 0x00,
            0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x40,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
    }

    #[test]
    fn test_list_tag_01() {
        // list(a=1)
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x02, 0x13, 0x00,
            0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x6e, 0x61, 0x6d, 0x65, 0x73, 0x00, 0x00,
            0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01,
            0x61, 0x00, 0x00, 0x00, 0xfe,
        ];
    }

    #[test]
    fn test_list_tag_02() {
        // list(a=1, b=2, 1)
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x02, 0x13, 0x00,
            0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x40,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00,
            0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x02, 0x00,
            0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x6e, 0x61, 0x6d,
            0x65, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x00, 0x09,
            0x00, 0x00, 0x00, 0x01, 0x61, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x62,
            0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
        ];
    }

    #[test]
    fn test_langsxp_01() {
        // substitute(f(x))
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00,
            0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x66, 0x00, 0x00,
            0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01,
            0x78, 0x00, 0x00, 0x00, 0xfe,
        ];
    }

    #[test]
    fn test_langsxp_02() {
        // substitute(x + 1)
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00,
            0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x2b, 0x00, 0x00,
            0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01,
            0x78, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f,
            0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
        ];
    }

    #[test]
    fn test_langsxp_03() {
        // substitute(hello(1)(2))
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00,
            0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00,
            0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0e,
            0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0xfe, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01,
            0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
        ];
    }

    #[test]
    fn test_langsxp_04() {
        // substitute(x + y)
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x06, 0x00,
            0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x2b, 0x00, 0x00,
            0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01,
            0x78, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
            0x00, 0x00, 0x01, 0x79, 0x00, 0x00, 0x00, 0xfe,
        ];
    }

    #[test]
    fn test_closxp01() {
        // function(x) x
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00,
            0x00, 0x00, 0xfd, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00,
            0x09, 0x00, 0x00, 0x00, 0x01, 0x78, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x00, 0xfe,
            0x00, 0x00, 0x01, 0xff,
        ];
    }

    #[test]
    fn test_closxp02() {
        // function(x, y) x + y
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00,
            0x00, 0x00, 0xfd, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00,
            0x09, 0x00, 0x00, 0x00, 0x01, 0x78, 0x00, 0x00, 0x00, 0xfb, 0x00, 0x00, 0x04, 0x02,
            0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x79, 0x00,
            0x00, 0x00, 0xfb, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00,
            0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x2b, 0x00, 0x00, 0x00, 0x02,
            0x00, 0x00, 0x01, 0xff, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x02, 0xff, 0x00, 0x00,
            0x00, 0xfe
        ];
    }

    #[test]
    fn test_closxp03() {
        // function() NULL ## but with attributes
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x06, 0x03, 0x00,
            0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00,
            0x06, 0x73, 0x72, 0x63, 0x72, 0x65, 0x66, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00,
            0x08, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x00, 0x00, 0x17, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x17, 0x00, 0x00, 0x00,
            0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x07, 0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65,
            0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf2, 0x00, 0x00,
            0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05,
            0x6c, 0x69, 0x6e, 0x65, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x39, 0x73, 0x61, 0x76, 0x65, 0x52, 0x44, 0x53,
            0x28, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x28, 0x29, 0x20, 0x4e, 0x55,
            0x4c, 0x4c, 0x2c, 0x20, 0x66, 0x69, 0x6c, 0x65, 0x3d, 0x22, 0x74, 0x6d, 0x70, 0x2e,
            0x64, 0x61, 0x74, 0x22, 0x2c, 0x20, 0x63, 0x6f, 0x6d, 0x70, 0x72, 0x65, 0x73, 0x73,
            0x3d, 0x46, 0x41, 0x4c, 0x53, 0x45, 0x29, 0x0a, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00,
            0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x08, 0x66, 0x69, 0x6c, 0x65,
            0x6e, 0x61, 0x6d, 0x65, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04,
            0x00, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe,
            0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00,
            0x00, 0x05, 0x63, 0x6c, 0x61, 0x73, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
            0x02, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x0b, 0x73, 0x72, 0x63, 0x66, 0x69,
            0x6c, 0x65, 0x63, 0x6f, 0x70, 0x79, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x07,
            0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x04,
            0x02, 0x00, 0x00, 0x06, 0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x06, 0x73, 0x72, 0x63, 0x72, 0x65, 0x66, 0x00,
            0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfd, 0x00, 0x00, 0x00,
            0xfe, 0x00, 0x00, 0x00, 0xfe,
        ];
    }

    #[test]
    fn test_closxp04() {
        // function() NULL
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x04, 0x03, 0x00,
            0x00, 0x00, 0xfd, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe,
        ];
    }

    #[test]
    fn test_bcreader_01() {
        test_data![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x06, 0x03, 0x00,
            0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00,
            0x06, 0x73, 0x72, 0x63, 0x72, 0x65, 0x66, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00,
            0x08, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00,
            0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x07, 0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65,
            0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf2, 0x00, 0x00,
            0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05,
            0x6c, 0x69, 0x6e, 0x65, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x28, 0x66, 0x63, 0x20, 0x3c, 0x2d, 0x20, 0x63,
            0x6f, 0x6d, 0x70, 0x69, 0x6c, 0x65, 0x72, 0x3a, 0x3a, 0x63, 0x6d, 0x70, 0x66, 0x75,
            0x6e, 0x28, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x28, 0x29, 0x20, 0x4e,
            0x55, 0x4c, 0x4c, 0x29, 0x0a, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x08, 0x66, 0x69, 0x6c, 0x65, 0x6e, 0x61, 0x6d,
            0x65, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x04,
            0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x05, 0x63,
            0x6c, 0x61, 0x73, 0x73, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x02, 0x00, 0x04,
            0x00, 0x09, 0x00, 0x00, 0x00, 0x0b, 0x73, 0x72, 0x63, 0x66, 0x69, 0x6c, 0x65, 0x63,
            0x6f, 0x70, 0x79, 0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x07, 0x73, 0x72, 0x63,
            0x66, 0x69, 0x6c, 0x65, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x04, 0x02, 0x00, 0x00,
            0x06, 0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04, 0x00, 0x09,
            0x00, 0x00, 0x00, 0x06, 0x73, 0x72, 0x63, 0x72, 0x65, 0x66, 0x00, 0x00, 0x00, 0xfe,
            0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0xfd, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00,
            0x00, 0x15, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x00, 0x03,
            0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x11, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
            0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x00, 0x0d,
            0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
            0x00, 0x18, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0x18,
            0x00, 0x00, 0x00, 0x26, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
            0x04, 0x02, 0x00, 0x00, 0x02, 0xff, 0x00, 0x00, 0x03, 0xff, 0x00, 0x00, 0x04, 0x02,
            0x00, 0x00, 0x06, 0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04,
            0x00, 0x09, 0x00, 0x00, 0x00, 0x06, 0x73, 0x72, 0x63, 0x72, 0x65, 0x66, 0x00, 0x00,
            0x00, 0xfe, 0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x03,
            0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x04, 0x02, 0x00, 0x00, 0x06, 0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01,
            0x00, 0x04, 0x00, 0x09, 0x00, 0x00, 0x00, 0x10, 0x65, 0x78, 0x70, 0x72, 0x65, 0x73,
            0x73, 0x69, 0x6f, 0x6e, 0x73, 0x49, 0x6e, 0x64, 0x65, 0x78, 0x00, 0x00, 0x00, 0xfe,
            0x00, 0x00, 0x00, 0x0d, 0x00, 0x00, 0x03, 0x0d, 0x00, 0x00, 0x00, 0x03, 0x80, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x04, 0x02,
            0x00, 0x00, 0x06, 0xff, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00, 0x04,
            0x00, 0x09, 0x00, 0x00, 0x00, 0x0c, 0x73, 0x72, 0x63, 0x72, 0x65, 0x66, 0x73, 0x49,
            0x6e, 0x64, 0x65, 0x78, 0x00, 0x00, 0x00, 0xfe
        ];
    }

    #[test]
    fn test_bcreader_02() {
        test_data![
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
    }
}
