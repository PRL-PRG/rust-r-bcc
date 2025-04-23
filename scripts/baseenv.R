#!/usr/bin/Rscript

cargs <- commandArgs(trailingOnly = TRUE)
if (file.exists(cargs[[1]])) {
  return(-1)
}

basevars <- ls("package:base", all.names = TRUE)
types <- sapply(basevars, \(x) typeof(get(x)))

# print(types)

saveRDS(basevars[types == "special"], paste(cargs[[1]], "specials", sep = "."), version = 2, compress = FALSE)
saveRDS(basevars[types == "builtin"], paste(cargs[[1]], "builtins", sep = "."), version = 2, compress = FALSE)

builtin_internals <- unique(c(builtins(), builtins(internal = TRUE)))
builtin_internals_black_list <- c("kronecker", ".dynLibs", "body<-")
builtin_internals <- setdiff(builtin_internals, builtin_internals_black_list)
simple_builtin_internals <- builtin_internals[sapply(builtin_internals, \(x) .Internal(is.builtin.internal(as.name(x))))]
cat("saving ", length(simple_builtin_internals), " builtin internals\n")
saveRDS(simple_builtin_internals, paste(cargs[[1]], "internal", sep = "."), version = 2, compress = FALSE)

base_env_funs <- basevars[sapply(basevars, \(x) is.function(get(x)))]
base_env_funs <- setdiff(base_env_funs, builtin_internals_black_list)
base_env <- sapply(base_env_funs, \(x) if (x %in% builtin_internals) get(x) else as.function(c(formals(get(x)), list(NULL))))
base_env <- as.environment(base_env)
base_env$pi <- pi
# base_env$T <- T
# base_env$F <- F
saveRDS(base_env, cargs[[1]], version = 2, compress = FALSE)
