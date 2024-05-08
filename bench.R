#!/usr/bin/Rscript
basevars <- ls("package:base", all.names = TRUE)
types <- sapply(basevars, \(x) typeof(get(x)))

orig <- sapply(basevars[types == "closure"], \(x) {
    tryCatch(eval(parse(text=deparse(get(x)))[[1]]), error = function(e) {
        NULL
    })
})

start_time = Sys.time();
x <- sapply(orig, \(x) tryCatch(compiler::cmpfun(x), error=function(e) NULL));
end_time = Sys.time();
end_time - start_time
