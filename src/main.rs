use std::{env, fs::File};

use rds::{
    rds_reader::{RDSReader, RDSReaderError},
    rds_writer::{RDSWriter, RDSWriterError},
};

use crate::{
    compiler::compiler::Compiler,
    rds::RDSResult,
    sexp::sexp::Sexp,
};

mod compiler;
mod rds;
mod server;
mod sexp;
mod sexphelpers;

#[derive(Debug)]
enum MainError {
    RDSRead(RDSReaderError),
    RDSWrite(RDSWriterError),
    IO(std::io::Error),
    WrongArgs,
}

impl From<RDSReaderError> for MainError {
    fn from(value: RDSReaderError) -> Self {
        MainError::RDSRead(value)
    }
}

impl From<std::io::Error> for MainError {
    fn from(value: std::io::Error) -> Self {
        MainError::IO(value)
    }
}

impl From<RDSWriterError> for MainError {
    fn from(value: RDSWriterError) -> Self {
        MainError::RDSWrite(value)
    }
}

impl RDSReader for File {}
impl RDSWriter for File {}

fn main() -> Result<(), MainError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        return Err(MainError::WrongArgs);
    }
    let mut file = File::open(args[1].as_str())?;
    let RDSResult { header, data: sexp } = file.read_rds()?;

    let compile = args[2] == "-c";

    println!("{sexp}");

    match sexp.kind {
        sexp::sexp::SexpKind::Closure(cl) if compile => {
            let mut compiler = Compiler::new();
            let bc = compiler.cmpfun(cl);
            println!("{bc}");
            println!("{:?}", compiler.warnings);
            let bc: Sexp = bc.into();

            let mut outfile = File::create("temp/compout.dat")?;
            outfile.write_rds(header, bc.into())?;
        }
        _ => {
            let mut outfile = File::create("temp/outfile.dat")?;
            outfile.write_rds(header, sexp)?;
        }
    };

    Ok(())
}
