createEnv <- function(env) {
    res <- as.environment(as.list(env, all.names=TRUE));
    env <- parent.env(env);
    tmp <- res
    while(!identical(env, emptyenv())) {
        parent.env(tmp) <- as.environment(as.list(env, all.names=TRUE));
        tmp <- parent.env(tmp)
        env <- parent.env(env);
    }
    res
}

cmpfuncon <- function(f, incon, outcon, options=NULL) {
    e <- createEnv(environment(f));
    data <- list(e, options);
    serialize(data, incon);
    flush(incon);
    unserialize(outcon)
}

cmpfun <- function(f, options=NULL) {
    incon <- socketConnection(1337, blocking=TRUE);
    outcon <- socketConnection(1338, blocking=TRUE);
    cmpfuncon(f, incon, outcon, options)
}

f <- function(x) {
    x + 1
}

f(2);
fc <- cmpfun(f)
fc(2)
