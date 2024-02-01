use libR_sys::setup_Rmainloop;
use libR_sys::R_CStackLimit;
use libR_sys::Rf_initialize_R;
use std::os::raw;

extern "C" {
    pub fn SET_PRINTNAME(x: libR_sys::SEXP, y: libR_sys::SEXP);
}
extern "C" {
    pub fn SET_SYMVALUE(x: libR_sys::SEXP, y: libR_sys::SEXP);
}
extern "C" {
    pub fn SET_INTERNAL(x: libR_sys::SEXP, y: libR_sys::SEXP);
}
fn main() {
    println!("Hello, world!");

    unsafe {
        std::env::set_var("R_HOME", "/usr/lib/R");
        let arg0 = "R\0".as_ptr() as *mut raw::c_char;
        Rf_initialize_R(1, [arg0].as_mut_ptr());
        R_CStackLimit = usize::max_value();
        setup_Rmainloop();
        //let res = libR_sys::cospi(0.5);
        let sym_value = libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::SYMSXP));
        let char_value = libR_sys::Rf_mkChar("cos\0".as_ptr() as *mut raw::c_char);
        let nil_value = libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::NILSXP));
        let buildin_value = libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::BUILTINSXP));
        SET_PRINTNAME(sym_value, char_value);
        SET_SYMVALUE(sym_value, nil_value);
        SET_INTERNAL(sym_value, buildin_value);
        libR_sys::Rf_unprotect(1);

        let cos_fn = libR_sys::Rf_findVar(sym_value, libR_sys::R_GlobalEnv);


        //println!("{res}");
    }
}

//SEXP add(SEXP a, SEXP b) {
//SEXP result = PROTECT(allocVector(REALSXP, 1));
//REAL(result)[0] = asReal(a) + asReal(b);
//UNPROTECT(1);
//
//return result;
//}
