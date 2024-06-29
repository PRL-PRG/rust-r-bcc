use std::{
    cell::UnsafeCell, collections::HashSet, io::Write, net::{Shutdown, TcpListener, TcpStream}
};

use bumpalo::Bump;

use crate::{
    compiler::compiler::{Compiler, CompilerOptions},
    rds::{rds_reader::RDSReader, rds_writer::RDSWriter, RDSResult},
    sexp::{sexp::{lang, Sexp, SexpKind}, sexp_alloc::Alloc},
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

//impl<'a> RDSReader<'a> for TcpStream {}
impl<'a> RDSWriter<'a> for TcpStream {}

fn handle_conn(stream: TcpStream) {
    println!("handle start");
    let stream = stream;

    let arena = Bump::new();
    let arena = Alloc::new(&arena);
    let stream = RDSReader::new(UnsafeCell::new(stream), &arena);

    let RDSResult { header, data } = stream.read_rds().unwrap();

    let SexpKind::Vec(data) = data.kind else {
        panic!()
    };
    let data = data;

    let SexpKind::Closure(closure) = &data[0].kind else {
        panic!()
    };
    let options: CompilerOptions = match &data[1].kind {
        SexpKind::Nil => CompilerOptions::default(),
        SexpKind::List(list) => {
            let inlining = list
                .into_iter()
                .find(|x| x.tag.is_some_and(|x| x.data == "optimize"))
                .map(|x| &x.data.kind);
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
        SexpKind::Real(data) if data.len() == 1 => CompilerOptions::new(data[0].data as usize),
        SexpKind::Vec(data) if data.len() == 1 => {
            if let SexpKind::Real(num) = &data[0].kind {
                if num.len() != 1 {
                    CompilerOptions::default()
                } else {
                    CompilerOptions::new(num[0].data as usize)
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

    let mut compiler = Compiler::new_options(options.inline_level, &arena);
    if data.len() == 3 {
        let a = &data[2].kind;
        let b = &data[3].kind;
        let c = &data[4].kind;
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
        compiler.builtins = HashSet::from_iter(builtins.to_vec().into_iter());
        compiler.specials = HashSet::from_iter(specials.to_vec().into_iter());
    }
    let res = compiler.cmpfun(closure);
    let res: &Sexp = arena.alloc(SexpKind::Closure(res).into());

    println!("output");
    println!("{res}\n");
    let header = header.clone();

    let mut stream = stream.restore();

    stream.write_rds(header.clone(), res, &arena).unwrap();
    stream.flush().unwrap();
    stream.shutdown(Shutdown::Write).unwrap();
}
