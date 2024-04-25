use std::{
    collections::HashSet,
    io::Write,
    net::{Shutdown, TcpListener, TcpStream},
};

use crate::{
    compiler::compiler::{Compiler, CompilerOptions},
    rds::{rds_reader::RDSReader, rds_writer::RDSWriter, RDSResult},
    sexp::sexp::{lang, Sexp, SexpKind},
};

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

    let RDSResult { header, data } = stream.read_rds().unwrap();

    let SexpKind::Vec(data) = data.kind else {
        panic!()
    };
    let mut data = data;

    let SexpKind::Closure(closure) = data.remove(0).kind else {
        panic!()
    };
    let options: CompilerOptions = match data.remove(0).kind {
        SexpKind::Nil => CompilerOptions::default(),
        SexpKind::List(list) => {
            let inlining = list
                .into_iter()
                .find(|x| x.tag.as_ref().is_some_and(|x| x.as_str() == "optimize"))
                .map(|x| x.data.kind);
            if let Some(SexpKind::Real(num)) = inlining {
                if num.len() != 1 {
                    CompilerOptions::default()
                } else {
                    todo!()
                }
            } else {
                CompilerOptions::default()
            }
        }
        SexpKind::Real(data) if data.len() == 1 => CompilerOptions::new(data[0] as usize),
        SexpKind::Vec(data) if data.len() == 1 => {
            if let SexpKind::Real(num) = &data[0].kind {
                if num.len() != 1 {
                    CompilerOptions::default()
                } else {
                    CompilerOptions::new(num[0] as usize)
                }
            } else {
                CompilerOptions::default()
            }
        }
        data => {
            println!("{data:?}");
            todo!()
        }
    };

    println!("len : {}", data.len());
    if data.len() == 5 {
        println!("{}", data[3]);
        println!("{}", data[4]);
    }

    let mut compiler = Compiler::new_options(options.inline_level);
    if data.len() == 3 {
        let a = data.remove(0).kind;
        let b = data.remove(0).kind;
        let c = data.remove(0).kind;
        println!("b : {b} \nc : {c}");
        let (baseenv, builtins, specials) = match (a, b, c) {
            (
                SexpKind::Environment(lang::Environment::Normal(env)),
                SexpKind::Str(builtins),
                SexpKind::Str(specials),
            ) => (env, builtins, specials),
            _ => unreachable!(),
        };
        compiler.set_baseenv(baseenv);
        compiler.builtins = HashSet::from_iter(builtins.into_iter());
        compiler.specials = HashSet::from_iter(specials.into_iter());
    }
    let res = compiler.cmpfun(closure);

    println!("output");
    println!("{res}\n");

    stream.write_rds(header, res.into()).unwrap();
    stream.flush().unwrap();
    stream.shutdown(Shutdown::Write).unwrap();
}
