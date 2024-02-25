mod server;
mod sexphelpers;
mod sexp;
mod rds;

use libR_sys::setup_Rmainloop;
use libR_sys::R_CStackLimit;
use libR_sys::Rf_initialize_R;
use std::os::raw;

use crate::sexphelpers::call;
use crate::sexphelpers::get_sym;

fn main() {
    unsafe {
        std::env::set_var("R_HOME", "/usr/lib/R");
        let arg0 = "R\0".as_ptr() as *mut raw::c_char;
        Rf_initialize_R(1, [arg0].as_mut_ptr());
        R_CStackLimit = usize::max_value();
        setup_Rmainloop();

        let cos_sq = call("^", &[&call("cos", &[&1.0]), &2.0]);
        let sin_sq = call("^", &[&call("sin", &[&1.0]), &2.0]);

        println!(
            "{} + {} = {}",
            *libR_sys::REAL(cos_sq),
            *libR_sys::REAL(sin_sq),
            *libR_sys::REAL(call("+", &[&cos_sq, &sin_sq]))
        );

        println!("{}", *libR_sys::LOGICAL(call("is.R", &[])));
        println!("{}", *libR_sys::LOGICAL(call("==", &[&1.0, &2.0])));
        println!("{}", *libR_sys::LOGICAL(call("==", &[&1.0, &1.0])));

        let fd = call("file", &[&"sexp.dat"]);
        call("open", &[&fd]);
        let data = call("unserialize", &[&fd]);
        call("close", &[&fd]);

        //let fd = call("file", &[&"envfn.dat"]);
        //call("open", &[&fd]);
        //let env = call("unserialize", &[&fd]);
        //call("close", &[&fd]);

        //let res = libR_sys::Rf_eval(data, env);
        //println!("{}", *libR_sys::REAL(res));

        //let fd = call("file", &[&"envlist.dat"]);
        //call("open", &[&fd]);
        //let env = call("unserialize", &[&fd]);
        //call("close", &[&fd]);

        //let res = libR_sys::Rf_eval(data, env);
        //println!("{}", *libR_sys::REAL(res));

        let fd = call("file", &[&"envwhole.dat"]);
        call("open", &[&fd]);
        let env = call("unserialize", &[&fd]);
        call("close", &[&fd]);

        let res = libR_sys::Rf_eval(data, env);
        println!("{}", *libR_sys::REAL(res));

        //call("<-", &[&get_sym("x"), &2.1]);
        
        //let res = libR_sys::Rf_eval(data, libR_sys::R_GlobalEnv);
        //println!("{}", *libR_sys::REAL(res));
    }

    //server::run();
}
