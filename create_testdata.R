#!/usr/bin/Rscript

cargs = commandArgs(trailingOnly=TRUE);

if (length(cargs) != 3) {
    stop()
}

## language
val <- eval(parse(text=cargs[[1]]))
saveRDS(val, file=cargs[[2]], compress=FALSE)
saveRDS(compiler::cmpfun(val), file=cargs[[3]], compress=FALSE)
