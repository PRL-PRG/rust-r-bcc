#!/usr/bin/Rscript
tmp <- compiler::enableJIT(0)

cargs <- commandArgs(trailingOnly = TRUE)

basevars <- ls("package:base", all.names = TRUE)
types <- sapply(basevars, \(x) typeof(get(x)))

saveRDS(basevars[types == "special"], paste(cargs[[1]], "specials", sep = "."), version = 2, compress = FALSE)
saveRDS(basevars[types == "builtin"], paste(cargs[[1]], "builtins", sep = "."), version = 2, compress = FALSE)
saveRDS(basevars[types == "closure"], paste(cargs[[1]], "closures", sep = "."), version = 2, compress = FALSE)

builtin_internals <- unique(c(builtins(), builtins(internal = TRUE)))
builtin_internals_black_list <- c("kronecker", ".dynLibs", "body<-")
builtin_internals <- setdiff(builtin_internals, builtin_internals_black_list)
builtin_internals <- builtin_internals[sapply(builtin_internals, \(x) .Internal(is.builtin.internal(as.name(x))))]
saveRDS(builtin_internals, paste(cargs[[1]], "internal", sep = "."), version = 2, compress = FALSE)

base_env_funs <- basevars[sapply(basevars, \(x) is.function(get(x)))]
base_env_funs <- setdiff(base_env_funs, builtin_internals_black_list)
base_env <- sapply(base_env_funs, \(x) if (x %in% builtin_internals) get(x) else as.function(c(formals(get(x)), list(NULL))))
base_env <- as.environment(base_env)
base_env$pi <- pi
# base_env$T <- T
# base_env$F <- F

# saveRDS(basevars, "basevars.RDS", version = 2, compress=FALSE)

saveRDS(base_env, cargs[[1]], version = 2, compress = FALSE)

orig <- sapply(basevars[types == "closure"], \(x) {
  tryCatch(eval(parse(text = deparse(get(x)))[[1]]), error = function(e) {
    NULL
  })
})
compiled_no_opt <- sapply(basevars[types == "closure"], \(x) {
  tryCatch(compiler::cmpfun(eval(parse(text = deparse(get(x)))[[1]]), options = list(optimize = 0)), error = function(e) {
    NULL
  })
})
compiled <- sapply(basevars[types == "closure"], \(x) {
  tryCatch(compiler::cmpfun(eval(parse(text = deparse(get(x)))[[1]]), options = list(optimize = 3)), error = function(e) {
    NULL
  })
})

compiled_env <- as.environment(compiled)
compiled_env_no_opt <- as.environment(compiled_no_opt)
orig <- as.environment(orig)

saveRDS(compiled_env, paste(cargs[[1]], "cmp", sep = "."), version = 2, compress = FALSE)
saveRDS(compiled_env_no_opt, paste(cargs[[1]], "cmp_no_opt", sep = "."), version = 2, compress = FALSE)
saveRDS(orig, paste(cargs[[1]], "orig", sep = "."), version = 2, compress = FALSE)
