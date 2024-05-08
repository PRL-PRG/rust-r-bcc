use std::{collections::HashSet, env, fs::File, time::Instant};

use rds::{
    rds_reader::{RDSReader, RDSReaderError},
    rds_writer::{RDSWriter, RDSWriterError},
};

use crate::{
    compiler::compiler::Compiler,
    rds::RDSResult,
    server::run,
    sexp::sexp::{lang, Sexp, SexpKind},
};

mod compiler;
mod rds;
mod server;
mod sexp;

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

fn bench() {
    let path_env = "temp/benchenv.RDS";

    // base environment
    let mut command = std::process::Command::new("./compile_base_package.R")
        .args([path_env])
        .spawn()
        .unwrap();
    assert!(command.wait().unwrap().success());

    let full_start = Instant::now();
    let mut file = std::fs::File::open(path_env).unwrap();
    let RDSResult {
        header,
        data: baseenv,
    } = file.read_rds().unwrap();

    let mut file = std::fs::File::open(format!("{path_env}.specials")).unwrap();
    let RDSResult {
        header: _,
        data: specials,
    } = file.read_rds().unwrap();

    let mut file = std::fs::File::open(format!("{path_env}.builtins")).unwrap();
    let RDSResult {
        header: _,
        data: builtins,
    } = file.read_rds().unwrap();

    let mut file = std::fs::File::open(format!("{path_env}.internal")).unwrap();
    let RDSResult {
        header: _,
        data: internals,
    } = file.read_rds().unwrap();

    let mut file = std::fs::File::open(format!("{path_env}.orig")).unwrap();
    let RDSResult {
        header: _,
        data: orig,
    } = file.read_rds().unwrap();

    let mut file = std::fs::File::open(format!("{path_env}.cmp")).unwrap();
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
    let all = orig.hash_frame.env.len();
    let mut compiler = Compiler::new();
    compiler.set_baseenv(env.clone());
    compiler.builtins = HashSet::from_iter(builtins.clone().into_iter());
    compiler.specials = HashSet::from_iter(specials.clone().into_iter());
    compiler.internals = HashSet::from_iter(internals.clone().into_iter());

    let comp_start = Instant::now();
    for (key, _) in &orig.hash_frame.env {
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
        let res = compiler.cmpfun(closure.clone());
        let corr_closure = cmp.hash_frame.get(&key).unwrap();
        let corr_closure = match &corr_closure.kind {
            SexpKind::Closure(closure) => closure,
            SexpKind::Nil => continue,
            _ => {
                println!("{closure}");
                panic!()
            }
        };

        if &res == corr_closure {
            correct += 1;
        }
    }

    eprintln!("{correct} / {all} ({count})");
    println!(
        "{}s {}ms {}s {}ms",
        full_start.elapsed().as_secs_f32(),
        full_start.elapsed().as_millis(),
        comp_start.elapsed().as_secs_f64(),
        comp_start.elapsed().as_millis()
    );
}

fn main() -> Result<(), MainError> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 && args[1] == "-s" {
        run();
        return Ok(());
    }
    if args.len() == 2 && args[1] == "-b" {
        bench();
        return Ok(());
    }
    if args.len() != 3 {
        return Err(MainError::WrongArgs);
    }
    let mut file = File::open(args[1].as_str())?;
    let RDSResult { header, data: sexp } = file.read_rds()?;

    let compile = args[2] == "-c";

    println!("start {sexp}");

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
            //let mut outfile = File::create("temp/outfile.dat")?;
            //outfile.write_rds(header, sexp)?;
        }
    };

    Ok(())
}
