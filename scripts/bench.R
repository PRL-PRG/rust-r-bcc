#!/usr/bin/env Rscript
pkgs <- c("base", "utils", "compiler")
orig <- do.call(c, sapply(pkgs, \(name) {
    namespace <- getNamespace(name)
    names <- ls(namespace, all.names = TRUE)
    vars <- sapply(names, \(n) get(n, envir=namespace))
    funs <- Filter(\(v) typeof(v) == "closure" && identical(environment(v), namespace), vars)
    sapply(funs, \(f) tryCatch(eval(parse(text=deparse(f))[[1]]), error = \(e) NULL))
}))

start_time = Sys.time();
x <- sapply(orig, \(x) tryCatch(compiler::cmpfun(x), error=function(e) NULL));
end_time = Sys.time();
cat("Total: ", length(orig), "\n", sep = "", file = stderr())
cat(as.double(end_time - start_time, units = "secs"), "\n", sep = "")
