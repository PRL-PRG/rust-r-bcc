use std::io::Write;

use crate::sexp::sexp::{Sexp, SexpKind};

use super::Flag;

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

trait RDSWriter: Write {
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

    fn write_str(&mut self, value : &str) -> Ret {
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

    fn write_flags(&mut self, flags : Flag) -> Ret {
        todo!()
    }

    fn write_rds(&mut self, sexp: Sexp) -> Ret {
        self.write_header()?;
        self.write_item(sexp)?;
        self.write_str("UTF-8")?;
        Ok(())
    }

    fn write_item(&mut self, sexp: Sexp) -> Ret {
        match sexp.kind {
            SexpKind::Sym(_) => todo!(),
            SexpKind::List(_) => todo!(),
            SexpKind::Nil => todo!(),
            SexpKind::Closure(_) => todo!(),
            SexpKind::Environment(_) => todo!(),
            SexpKind::Promise => todo!(),
            SexpKind::Lang(_) => todo!(),
            SexpKind::Bc(_) => todo!(),
            SexpKind::Char(_) => todo!(),
            SexpKind::Logic(_) => todo!(),
            SexpKind::Real(_) => todo!(),
            SexpKind::Int(ints) => {
                todo!()
            },
            SexpKind::Complex(_) => todo!(),
            SexpKind::Str(_) => todo!(),
            SexpKind::Vec(_) => todo!(),
            SexpKind::MissingArg => todo!(),
        }
    }
}
