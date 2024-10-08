#!/usr/bin/Rscript

cargs = commandArgs(trailingOnly=TRUE);

if (file.exists(cargs[[1]])) return(-1)

basevars <- ls("package:base", all.names = TRUE)
types <- sapply(basevars, \(x) typeof(get(x)))

#print(types)

saveRDS(basevars[types == "special"], paste(cargs[[1]], "specials", sep="."), version = 2, compress=FALSE)
saveRDS(basevars[types == "builtin"], paste(cargs[[1]], "builtins", sep="."), version = 2, compress=FALSE)

builtin_internals <- builtins(internal = TRUE)

base_env_funs <- basevars[types == "closure" | types == "special" | types == "builtin"]
# base_env <- sapply(base_env_funs, get)
base_env <- sapply(base_env_funs, \(x) if (x %in% builtin_internals) get(x) else as.function(c(formals(get(x)), list(NULL))))
base_env <- as.environment(base_env)

#saveRDS(basevars, "basevars.RDS", version = 2, compress=FALSE)

saveRDS(base_env, cargs[[1]], version = 2, compress=FALSE)

saveRDS(builtins(internal = TRUE), paste(cargs[[1]], "internal", sep="."), version = 2, compress=FALSE)


