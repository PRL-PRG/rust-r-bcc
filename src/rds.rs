use std::io::BufReader;

use std::io::Read;

use crate::sexp::data;
use crate::sexp::Sexp;
use crate::sexp::SexpKind;

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

pub struct RDSHeader {
    rds_type: u8,
    format_version: i32,
}

#[allow(dead_code)]
mod sexptype {
    pub const NILSXP: u8 = 0; /* nil = NULL */
    pub const SYMSXP: u8 = 1; /* symbols */
    pub const LISTSXP: u8 = 2; /* lists of dotted pairs */
    pub const CLOSXP: u8 = 3; /* closures */
    pub const ENVSXP: u8 = 4; /* environments */
    pub const PROMSXP: u8 = 5; /* promises: [un]evaluated closure arguments */
    pub const LANGSXP: u8 = 6; /* language constructs (special lists) */
    pub const SPECIALSXP: u8 = 7; /* special forms */
    pub const BUILTINSXP: u8 = 8; /* builtin non-special forms */
    pub const CHARSXP: u8 = 9; /* "scalar" string type (internal only)*/
    pub const LGLSXP: u8 = 10; /* logical vectors */
    /* 11 and 12 were factors and ordered factors in the 1990s */
    pub const INTSXP: u8 = 13; /* integer vectors */
    pub const REALSXP: u8 = 14; /* real variables */
    pub const CPLXSXP: u8 = 15; /* complex variables */
    pub const STRSXP: u8 = 16; /* string vectors */
    pub const DOTSXP: u8 = 17; /* dot-dot-dot object */
    pub const ANYSXP: u8 = 18; /* make "any" args work.
                               Used in specifying types for symbol
                               registration to mean anything is okay  */
    pub const VECSXP: u8 = 19; /* generic vectors */
    pub const EXPRSXP: u8 = 20; /* expressions vectors */
    pub const BCODESXP: u8 = 21; /* byte code */
    pub const EXTPTRSXP: u8 = 22; /* external pointer */
    pub const WEAKREFSXP: u8 = 23; /* weak reference */
    pub const RAWSXP: u8 = 24; /* raw bytes */
    pub const OBJSXP: u8 = 25; /* object, non-vector  */
    pub const S4SXP: u8 = 25; /* same as OBJSXP, retained for back compatability */

    /* used for detecting PROTECT issues in memory.c */
    pub const NEWSXP: u8 = 30; /* fresh node created in new page */
    pub const FREESXP: u8 = 31; /* node released by GC */

    pub const FUNSXP: u8 = 99; /* Closure or Builtin or Special */

    pub const REFSXP: u8 = 255;
    pub const NILVALUE_SXP: u8 = 254;
    pub const GLOBALENV_SXP: u8 = 253;
    pub const UNBOUNDVALUE_SXP: u8 = 252;
    pub const MISSINGARG_SXP: u8 = 251;
    pub const BASENAMESPACE_SXP: u8 = 250;
    pub const NAMESPACESXP: u8 = 249;
    pub const PACKAGESXP: u8 = 248;
    pub const PERSISTSXP: u8 = 247;

    pub const CLASSREFSXP: u8 = 246;
    pub const GENERICREFSXP: u8 = 245;
    pub const BCREPDEF: u8 = 244;
    pub const BCREPREF: u8 = 243;
    pub const EMPTYENV_SXP: u8 = 242;
    pub const BASEENV_SXP: u8 = 241;
}

pub struct Flag {
    sexp_type: u8,
    level: i32,
    has_attributes: bool,
    has_tag: bool,
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

        let item = self.read_item()?;

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

    fn read_item(&mut self) -> Result<Sexp, RDSReaderError> {
        let flag = self.read_flags()?;
        self.read_item_flags(flag)
    }

    fn read_item_flags(&mut self, flag: Flag) -> Result<Sexp, RDSReaderError> {
        match flag.sexp_type {
            sexptype::NILVALUE_SXP | sexptype::NILSXP => Ok(SexpKind::Nil.into()),
            sexptype::REALSXP => self.read_realsxp(),
            sexptype::INTSXP => self.read_intsxp(),
            sexptype::LISTSXP => self.read_listsxp(flag),
            sexptype::VECSXP => self.read_vecsxp(),
            x => {
                println!("{x}");
                todo!()
            },
        }
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
        })
    }

    fn read_len(&mut self) -> Result<usize, RDSReaderError> {
        let len = self.read_int()?;
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

    fn read_vecsxp(&mut self) -> Result<Sexp, RDSReaderError> {
        let len = self.read_len()?;

        let mut data = vec![];
        data.reserve(len);

        for _ in 0..len {
            data.push(self.read_item()?)
        }

        Ok(SexpKind::Vec(data).into())
    }

    fn read_listsxp(&mut self, flags: Flag) -> Result<Sexp, RDSReaderError> {
        // read in order attrib tag and value
        // only value is mandatory

        let mut data = vec![];

        loop {
            if flags.has_attributes {
                todo!()
            }

            if flags.has_tag {
                todo!()
            }

            let value = self.read_item()?;
            data.push(value.into());

            let flags = self.read_flags()?;
            if flags.sexp_type != sexptype::LISTSXP {
                let last = self.read_item_flags(flags)?;
                data.push(last.into());
                return Ok(SexpKind::List(data).into());
            }
        }
    }
}

impl<T> RDSReader for BufReader<T> where T: Sized + std::io::Read {}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::sexp::SexpKind;

    use super::*;

    #[test]
    fn test_header() {
        let data: Vec<u8> = vec![b'X', b'\n', 0, 0, 0, 2, 1, 1, 1, 1, 2, 2, 2, 2];
        let data = Cursor::new(data);
        let mut reader = BufReader::new(data);
        reader.read_header().unwrap();
    }

    #[test]
    fn test_real() {
        let data: Vec<u8> = vec![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0e, 0x00,
            0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let data = Cursor::new(data);
        let mut reader = BufReader::new(data);
        let res = reader.read_rds().unwrap();

        assert_eq!(res, SexpKind::Real(vec![1.0]).into());

        let data: Vec<u8> = vec![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x0e, 0x00,
            0x00, 0x00, 0x02, 0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let data = Cursor::new(data);
        let mut reader = BufReader::new(data);
        let res = reader.read_rds().unwrap();

        assert_eq!(res, SexpKind::Real(vec![1.0, 2.0]).into());
    }

    #[test]
    fn test_list() {
        let data: Vec<u8> = vec![
            0x58, 0x0a, 0x00, 0x00, 0x00, 0x03, 0x00, 0x04, 0x03, 0x02, 0x00, 0x03, 0x05, 0x00,
            0x00, 0x00, 0x00, 0x05, 0x55, 0x54, 0x46, 0x2d, 0x38, 0x00, 0x00, 0x00, 0x13, 0x00,
            0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x3f, 0xf0, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00, 0x00, 0x00, 0x01, 0x40,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let data = Cursor::new(data);
        let mut reader = BufReader::new(data);
        let res = reader.read_rds().unwrap();

        assert_eq!(
            res,
            SexpKind::Vec(vec![
                SexpKind::Real(vec![1.0]).into(),
                SexpKind::Real(vec![2.0]).into(),
            ])
            .into()
        )
    }
}
