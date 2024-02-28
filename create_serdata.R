#!/usr/bin/Rscript

cargs = commandArgs(trailingOnly=TRUE);

if (length(cargs) != 3) {
    stop()
}

## language
if (cargs[[1]] == "-l") {
    saveRDS(parse(text=cargs[[2]])[[1]], file=cargs[[3]], compress=FALSE)
} else if (cargs[[1]] == "-d") {
    saveRDS(eval(parse(text=cargs[[2]])), file=cargs[[3]], compress=FALSE)
} else {
    print("you must choose either language (-l) or data (-d)")
}
