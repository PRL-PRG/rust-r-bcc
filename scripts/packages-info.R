#!/usr/bin/env Rscript

library(rbytecode)

pkgs <- c("base", "utils", "compiler", "tools", "stats")
origs <- sapply(pkgs, \(name) {
    namespace <- getNamespace(name)
    names <- ls(namespace, all.names = TRUE)
    vars <- sapply(names, \(n) get(n, envir=namespace))
    funs <- Filter(\(v) typeof(v) == "closure" && identical(environment(v), namespace), vars)
    sapply(funs, \(f) tryCatch(eval(parse(text=deparse(f))[[1]]), error = \(e) NULL))
})

for(i in seq_along(pkgs)) {
    orig <- origs[[i]]

    x <- sapply(orig, \(x) tryCatch(compiler::cmpfun(x), error=function(e) NULL));

    cat("Package: ", pkgs[i], "\n", sep = "", file = stderr())
    cat("Nb functions: ", length(orig), "\n", sep = "", file = stderr())

    # Get total bytecode size 
    #total_size <- sum(sapply(x, \(f) if (is.null(f)) 0 else object.size(f)))
    total_size <- sum(sapply(x, \(f) if (is.null(f)) 0 else tryCatch(nrow(dis(f)), error  = \(e) 0)), na.rm = TRUE)
    cat("Total bytecode size: ", format(total_size, units = "auto"), "\n", sep = "", file = stderr())
}
