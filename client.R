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

cmpfuncon <- function(f, con, options=NULL) {
    e <- createEnv(environment(f));
    #e <- new.env();
    data <- list(f, e);
    serialize(e, con);
    write('\n', con);
    print("sent");
    flush(con);
    unserialize(con)
}

cmpfun <- function(f, options=NULL) {
    con <- socketConnection(port=1337, blocking=FALSE, open="a+");
    res <- cmpfuncon(f, con, options);
    close(con);
    res
}

f <- function(x) {
    x + 1
}

f(2);
fc <- cmpfun(f)
fc
