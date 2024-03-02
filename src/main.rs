use std::{env, fs::File};

use rds::rds_reader::{RDSReader, RDSReaderError};

use crate::compiler::compiler::Compiler;

mod compiler;
mod rds;
mod server;
mod sexp;
mod sexphelpers;

#[derive(Debug)]
enum MainError {
    RDS(RDSReaderError),
    IO(std::io::Error),
    WrongArgs,
}

impl From<RDSReaderError> for MainError {
    fn from(value: RDSReaderError) -> Self {
        MainError::RDS(value)
    }
}

impl From<std::io::Error> for MainError {
    fn from(value: std::io::Error) -> Self {
        MainError::IO(value)
    }
}

impl RDSReader for File {}

fn main() -> Result<(), MainError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(MainError::WrongArgs);
    }
    let mut file = File::open(args[1].as_str())?;
    let sexp = file.read_rds()?;
    
    println!("{sexp:?}");

    let compiler = Compiler::new();
    match sexp.kind {
        sexp::sexp::SexpKind::Closure(cl) => {
            let bc = compiler.cmpfun(cl);
            println!("{bc:?}");
        },
        _ => todo!(),
    };

    Ok(())
}
