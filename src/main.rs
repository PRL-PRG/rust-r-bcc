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

extern "C" {
    pub fn Rf_StrToInternal(s : *mut raw::c_char) -> i32;
}
extern "C" {
    pub fn mkPRIMSXP(offset : i32, eval : i32) -> libR_sys::SEXP;
}

//extern "C" {
    //pub fn R_inspect(x : libR_sys::SEXP) -> libR_sys::SEXP;
//}

fn main() {

    unsafe {
        std::env::set_var("R_HOME", "/usr/lib/R");
        let arg0 = "R\0".as_ptr() as *mut raw::c_char;
        Rf_initialize_R(1, [arg0].as_mut_ptr());
        R_CStackLimit = usize::max_value();
        setup_Rmainloop();
        //let res = libR_sys::cospi(0.5);
        //let sym_value = libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::SYMSXP));
        let sym_value = libR_sys::Rf_install("cos\0".as_ptr() as *mut raw::c_char);
        //let char_value = libR_sys::Rf_protect(libR_sys::Rf_mkChar("cos\0".as_ptr() as *mut raw::c_char));
        let nil_value = libR_sys::R_NilValue;
        //let buildin_value = /*libR_sys::R_UnboundValue;*/libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::BUILTINSXP));
        //libR_sys::SET_TYPEOF(buildin_value, libR_sys::BUILTINSXP as i32);
        //let index = Rf_StrToInternal("cos\0".as_ptr() as *mut raw::c_char);
        //let buildin_value = mkPRIMSXP(index, 1);
        

        //SET_PRINTNAME(sym_value, char_value);
        //SET_INTERNAL(sym_value, nil_value);
        //SET_SYMVALUE(sym_value, sym_value);
        let input_sexpr = libR_sys::Rf_protect(libR_sys::Rf_ScalarReal(1.0));
        let inputlist_sexpr = libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::LISTSXP));

        libR_sys::SETCAR(inputlist_sexpr, input_sexpr);
        libR_sys::SET_TAG(inputlist_sexpr, nil_value);
        libR_sys::SETCDR(inputlist_sexpr, nil_value);

        let lang_sexpr = libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::LANGSXP));

        libR_sys::SETCAR(lang_sexpr, sym_value);
        libR_sys::SET_TAG(lang_sexpr, nil_value);
        libR_sys::SETCDR(lang_sexpr, inputlist_sexpr);


        //R_inspect(lang_sexpr);
        libR_sys::R_CurrentExpression = lang_sexpr;

        
        //let cos_fn = libR_sys::Rf_findFun(sym_value, libR_sys::R_GlobalEnv);
        let res = libR_sys::Rf_eval(lang_sexpr, libR_sys::R_GlobalEnv);
        println!("{}", *libR_sys::REAL(res));
        libR_sys::Rf_unprotect(3);

    }
}



//SEXP add(SEXP a, SEXP b) {
//SEXP result = PROTECT(allocVector(REALSXP, 1));
//REAL(result)[0] = asReal(a) + asReal(b);
//UNPROTECT(1);
//
//return result;
//}
