use std::os::raw;

pub struct RContext {
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

pub trait IntoSexp {
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

impl IntoSexp for &str {
    fn into_sexp(&self, context: &mut RContext) -> libR_sys::SEXP {
        context.protect(unsafe {
            libR_sys::Rf_mkString((self.to_string() + "\0").as_ptr() as *mut raw::c_char)
        })
    }
}

pub fn create_call(name: &str, args: &[&dyn IntoSexp]) -> libR_sys::SEXP {
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

pub fn call(name: &str, args: &[&dyn IntoSexp]) -> libR_sys::SEXP {
    let call = create_call(name, args);
    unsafe { libR_sys::Rf_eval(call, libR_sys::R_GlobalEnv) }
}

pub fn get_sym(name: &str) -> libR_sys::SEXP {
    unsafe { libR_sys::Rf_install((name.to_string() + "\0").as_ptr() as *mut raw::c_char) }
}
