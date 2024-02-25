use std::io::BufReader;

use std::io::Read;

use crate::sexp::Sexp;

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

mod sexptype {
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
        Ok(f64::from_le_bytes(buf))
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
        match flag.sexp_type {
            sexptype::NILVALUE_SXP => todo!(),
            _ => todo!(),
        }
    }

    fn read_flags(&mut self) -> Result<Flag, RDSReaderError> {
        let flag = self.read_int()?;
        let sexp_type: u8 = (flag & 255) as u8;
        let level: i32 = flag >> 12;

        Ok(Flag { sexp_type, level })
    }
}

impl<T> RDSReader for BufReader<T> where T: Sized + std::io::Read {}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::Cursor};

    use super::*;

    #[test]
    fn test_header() {
        let data: Vec<u8> = vec![b'X', b'\n', 0, 0, 0, 2, 1, 1, 1, 1, 2, 2, 2, 2];
        let data = Cursor::new(data);
        let mut reader = BufReader::new(data);
        reader.read_header().unwrap();
    }

    impl RDSReader for File {}

    #[test]
    fn test_tmp() {
        let mut file = File::open("one.dat").unwrap();
        file.read_rds().unwrap();
    }
}
