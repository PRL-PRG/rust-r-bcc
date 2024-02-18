use libR_sys::setup_Rmainloop;
use libR_sys::R_CStackLimit;
use libR_sys::Rf_initialize_R;
use std::os::raw;

/*
extern "C" {
    pub fn SET_PRINTNAME(x: libR_sys::SEXP, y: libR_sys::SEXP);
}
extern "C" {
    pub fn SET_SYMVALUE(x: libR_sys::SEXP, y: libR_sys::SEXP);
}
extern "C" {
    pub fn SET_INTERNAL(x: libR_sys::SEXP, y: libR_sys::SEXP);
}

extern "C" {
    pub fn Rf_StrToInternal(s : *mut raw::c_char) -> i32;
}
extern "C" {
    pub fn mkPRIMSXP(offset : i32, eval : i32) -> libR_sys::SEXP;
}*/

struct RContext {
    protect_count: usize,
}

impl Default for RContext {
    fn default() -> Self {
        Self { protect_count: 0 }
    }
}

impl Drop for RContext {
    fn drop(&mut self) {
        self.unprotect();
    }
}

impl RContext {
    pub fn protect(&mut self, sexp: libR_sys::SEXP) -> libR_sys::SEXP {
        self.protect_count += 1;
        unsafe { libR_sys::Rf_protect(sexp) }
    }

    pub fn unprotect(&mut self) {
        unsafe { libR_sys::Rf_unprotect(self.protect_count as i32) }
        self.protect_count = 0;
    }
}

trait IntoSexp {
    fn into_sexp(&self, context: &mut RContext) -> libR_sys::SEXP;
}

impl IntoSexp for f64 {
    fn into_sexp(&self, context: &mut RContext) -> libR_sys::SEXP {
        context.protect(unsafe { libR_sys::Rf_ScalarReal(*self) })
    }
}

impl IntoSexp for libR_sys::SEXP {
    fn into_sexp(&self, _: &mut RContext) -> libR_sys::SEXP {
        self.clone()
    }
}

fn create_call(name: &str, args: &[&dyn IntoSexp]) -> libR_sys::SEXP {
    let mut context = RContext::default();
    let sym =
        unsafe { libR_sys::Rf_install((name.to_string() + "\0").as_ptr() as *mut raw::c_char) };
    let sym = context.protect(sym);
    let args: Vec<libR_sys::SEXP> = args.iter().map(|x| x.into_sexp(&mut context)).collect();

    let nilval = unsafe { libR_sys::R_NilValue };
    let mut args_list = nilval;
    for arg in args.into_iter().rev() {
        let tmp = context.protect(unsafe { libR_sys::Rf_allocSExp(libR_sys::LISTSXP) });
        unsafe {
            libR_sys::SETCAR(tmp, arg);
            libR_sys::SET_TAG(tmp, nilval);
            libR_sys::SETCDR(tmp, args_list);
        }
        args_list = tmp;
    }

    let lang = context.protect(unsafe { libR_sys::Rf_allocSExp(libR_sys::LANGSXP) });
    unsafe {
        libR_sys::SETCAR(lang, sym);
        libR_sys::SET_TAG(lang, nilval);
        libR_sys::SETCDR(lang, args_list);
    }
    lang
}

fn call(name: &str, args: &[&dyn IntoSexp]) -> libR_sys::SEXP {
    let call = create_call(name, args);
    unsafe { libR_sys::Rf_eval(call, libR_sys::R_GlobalEnv) }
}

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
    }
}
