#!/usr/bin/Rscript

cargs = commandArgs(trailingOnly=TRUE);

if (length(cargs) != 4) {
    stop()
}

opt <- list(optimize=0)
if (cargs[[4]] == "-opt") {
    opt <- list(optimize=2)
}

## language
val <- eval(parse(text=cargs[[1]]))
saveRDS(val, file=cargs[[2]], compress=FALSE)
saveRDS(compiler::cmpfun(val, options=opt), file=cargs[[3]], compress=FALSE)
