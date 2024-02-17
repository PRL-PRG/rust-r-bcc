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


fn main() {

    unsafe {
        std::env::set_var("R_HOME", "/usr/lib/R");
        let arg0 = "R\0".as_ptr() as *mut raw::c_char;
        Rf_initialize_R(1, [arg0].as_mut_ptr());
        R_CStackLimit = usize::max_value();
        setup_Rmainloop();

        // install searches the symbol table and returns symbol
        // if the symbol is not found then it creates some default
        // non special/builtin symbol
        // R_SymbolTable is hash table of SEXPs
        let sym_value = libR_sys::Rf_install("cos\0".as_ptr() as *mut raw::c_char);

        // Nil value is singleton
        let nil_value = libR_sys::R_NilValue;
        
        let input_sexpr = libR_sys::Rf_protect(libR_sys::Rf_ScalarReal(1.0));
        let inputlist_sexpr = libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::LISTSXP));

        libR_sys::SETCAR(inputlist_sexpr, input_sexpr);
        libR_sys::SET_TAG(inputlist_sexpr, nil_value);
        libR_sys::SETCDR(inputlist_sexpr, nil_value);

        let lang_sexpr = libR_sys::Rf_protect(libR_sys::Rf_allocSExp(libR_sys::LANGSXP));

        libR_sys::SETCAR(lang_sexpr, sym_value);
        libR_sys::SET_TAG(lang_sexpr, nil_value);
        libR_sys::SETCDR(lang_sexpr, inputlist_sexpr);

        // current expression is needed when you use the 
        // findFun function but eval function sets it
        //libR_sys::R_CurrentExpression = lang_sexpr;

        let res = libR_sys::Rf_eval(lang_sexpr, libR_sys::R_GlobalEnv);
        println!("{}", *libR_sys::REAL(res));

        // number in unprotect is the number of pointers
        // that are protected that you want unprotect
        libR_sys::Rf_unprotect(3);

    }
}
