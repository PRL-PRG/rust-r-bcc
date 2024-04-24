use std::{
    io::{BufRead, BufReader, Write},
    net::{Shutdown, TcpListener, TcpStream},
};

use crate::{compiler::compiler::Compiler, rds::{rds_reader::RDSReader, rds_writer::RDSWriter, RDSResult}, sexp::sexp::SexpKind};

pub fn run() {
    let listener = TcpListener::bind("127.0.0.1:1337").unwrap();

    for conn in listener.incoming() {
        match conn {
            Ok(stream) => handle_conn(stream),
            Err(x) => println!("{}", x),
        }
    }
}

impl RDSReader for TcpStream {}
impl RDSWriter for TcpStream {}

fn handle_conn(stream: TcpStream) {
    println!("handle start");
    let mut stream = stream;
    let RDSResult {header, data} = stream.read_rds().unwrap();
    println!("input");
    println!("{data}\n");

    let mut compiler = Compiler::new_options(0);
    let SexpKind::Closure(closure) = data.kind else {panic!()};
    let res = compiler.cmpfun(closure);

    println!("output");
    println!("{res}\n");

    stream.write_rds(header, res.into()).unwrap();
    stream.flush().unwrap();
    stream.shutdown(Shutdown::Write).unwrap();
}
