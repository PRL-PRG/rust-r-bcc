use std::{cell::UnsafeCell, collections::HashSet, env, fs::File, time::Instant};
use std::path::Path;
use bumpalo::Bump;
use rds::{
    rds_reader::{RDSReader, RDSReaderError},
    rds_writer::{RDSWriter, RDSWriterError},
};
use sexp::sexp_alloc::Alloc;

use crate::{
    compiler::compiler::Compiler,
    rds::RDSResult,
    server::run,
    sexp::sexp::{lang, Sexp, SexpKind},
};
use crate::misc::commands::{create_bench_envs};

mod compiler;
mod rds;
mod server;
mod sexp;
mod misc;

#[derive(Debug)]
#[allow(dead_code)]
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

//impl<'a> RDSReader<'a> for File {}
impl<'a> RDSWriter<'a> for File {}

fn bench(opt: bool, check: bool, log_errors: bool) {
    assert!(!check || (opt && !log_errors), "invalid combination of options");

    let path_env = if opt { "temp/benchenv.RDS" } else { "temp/benchenv_noopt.RDS" };
    if check || !Path::new(path_env).exists() {
        create_bench_envs(path_env);
    }

    let arena = Bump::new();
    let arena = Alloc::new(&arena);
    let file = std::fs::File::open(path_env).unwrap();
    let file = RDSReader::new(UnsafeCell::new(file), &arena);
    let RDSResult {
        header: _,
        data: baseenv,
    } = file.read_rds().unwrap();

    let file = std::fs::File::open(format!("{path_env}.specials")).unwrap();
    let file = RDSReader::new(UnsafeCell::new(file), &arena);
    let RDSResult {
        header: _,
        data: specials,
    } = file.read_rds().unwrap();

    let file = std::fs::File::open(format!("{path_env}.builtins")).unwrap();
    let file = RDSReader::new(UnsafeCell::new(file), &arena);
    let RDSResult {
        header: _,
        data: builtins,
    } = file.read_rds().unwrap();

    let file = std::fs::File::open(format!("{path_env}.internal")).unwrap();
    let file = RDSReader::new(UnsafeCell::new(file), &arena);
    let RDSResult {
        header: _,
        data: internals,
    } = file.read_rds().unwrap();

    let file = std::fs::File::open(format!("{path_env}.orig")).unwrap();
    let file = RDSReader::new(UnsafeCell::new(file), &arena);
    let RDSResult {
        header: _,
        data: orig,
    } = file.read_rds().unwrap();

    let file = std::fs::File::open(format!("{path_env}.cmp")).unwrap();
    let file = RDSReader::new(UnsafeCell::new(file), &arena);
    let RDSResult {
        header: _,
        data: cmp,
    } = file.read_rds().unwrap();

    let SexpKind::Environment(lang::Environment::Normal(env)) = baseenv.kind else {
        unreachable!()
    };

    let SexpKind::Environment(lang::Environment::Normal(orig)) = orig.kind else {
        println!("{orig}");
        unreachable!()
    };

    let SexpKind::Environment(lang::Environment::Normal(cmp)) = cmp.kind else {
        println!("{cmp}");
        unreachable!()
    };

    assert!(orig.hash_frame.data.is_some());
    let SexpKind::Str(specials) = specials.kind else {
        unreachable!()
    };

    let SexpKind::Str(builtins) = builtins.kind else {
        unreachable!()
    };

    let SexpKind::Str(internals) = internals.kind else {
        unreachable!()
    };

    let mut count = 0;
    let mut correct = 0;
    let mut correct_except_cp = 0;
    let all = orig.hash_frame.env.len();
    let mut compiler = Compiler::new(&arena);
    compiler.set_baseenv(env);
    compiler.builtins = HashSet::from_iter(builtins.to_vec().into_iter());
    compiler.specials = HashSet::from_iter(specials.to_vec().into_iter());
    compiler.internals = HashSet::from_iter(internals.to_vec().into_iter());

    let comp_start = Instant::now();
    for key in orig.hash_frame.env.keys() {
        count += 1;
        let closure = orig.hash_frame.get(&key).unwrap();
        let closure = match &closure.kind {
            SexpKind::Closure(closure) => closure,
            SexpKind::Nil => continue,
            _ => {
                println!("{closure}");
                panic!()
            }
        };
        let res = compiler.cmpfun(closure);

        if check {
            let corr_closure = cmp.hash_frame.get(&key).unwrap();
            let corr_closure = match &corr_closure.kind {
                SexpKind::Closure(closure) => closure,
                SexpKind::Nil => panic!(),
                _ => {
                    println!("{closure}");
                    panic!()
                }
            };

            if &res == corr_closure {
                correct += 1;
                correct_except_cp += 1;
            } else {
                if matches!(
                    (&res.body.kind, &corr_closure.body.kind),
                    (SexpKind::Bc(mine), SexpKind::Bc(corr))
                    if mine.instructions == corr.instructions
                ) {
                    correct_except_cp += 1;
                    eprintln!("fail {key} (only constants)");
                } else {
                    eprintln!("fail {key}");
                }
                if log_errors /*|| *key == "simpleCondition"*/ {
                    eprintln!(
                        "{}",
                        prettydiff::diff_lines(&res.to_string(), &corr_closure.to_string()),
                    );
                }
            }
        }
    }

    if check {
        assert_eq!(count, all);
        eprintln!("{correct_except_cp} / {all} (ignoring constant pool)");
        eprintln!("{correct} / {all}");
    } else {
        eprintln!("Total: {count}")
    }
    println!("{}", comp_start.elapsed().as_secs_f64());
}

fn main() -> Result<(), MainError> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 && args[1] == "-s" {
        run();
        return Ok(());
    }
    if args.len() == 2 && args[1] == "-b" {
        let opt = !env::var("NOOPT").is_ok_and(|v| v == "1" || v == "true");
        let check = !env::var("NOCHECK").is_ok_and(|v| v == "1" || v == "true");
        let log_errors = env::var("LOG_ERRORS").is_ok_and(|v| v == "1" || v == "true");
        bench(opt, check, log_errors);
        return Ok(());
    }
    if args.len() != 3 {
        return Err(MainError::WrongArgs);
    }
    let file = File::open(args[1].as_str())?;
    let alloc = Bump::new();
    let alloc = Alloc::new(&alloc);
    let file = RDSReader::new(UnsafeCell::new(file), &alloc);
    let RDSResult { header, data: sexp } = file.read_rds()?;

    let compile = args[2] == "-c";

    println!("start {sexp:#?}");

    match &sexp.kind {
        sexp::sexp::SexpKind::Closure(cl) if compile => {
            let mut compiler = Compiler::new(&alloc);
            let bc = compiler.cmpfun(cl);
            println!("{bc}");
            //println!("{:?}", compiler.warnings);
            //let bc: Sexp = bc.into();
            let bc: &Sexp = alloc.alloc(SexpKind::Closure(bc).into());

            let mut outfile = File::create("temp/compout.dat")?;
            outfile.write_rds(header, bc, &alloc)?;
        }
        _ => {
            let mut outfile = File::create("temp/outfile.dat")?;
            outfile.write_rds(header, sexp, &alloc)?;
        }
    };

    Ok(())
}
