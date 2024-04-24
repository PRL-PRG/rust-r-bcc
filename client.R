createEnv <- function(env) {
    res <- as.environment(as.list(env, all.names=TRUE));
    env <- parent.env(env);
    tmp <- res
    while(!identical(env, emptyenv())) {
        parent.env(tmp) <- as.environment(as.list(env, all.names=TRUE));
        tmp <- parent.env(tmp)
        env <- parent.env(env);
    }
    parent.env(tmp) <- emptyenv();
    res
}

cmpfuncon <- function(f, con, options=NULL) {
    #e <- createEnv(environment(f));
    #e <- new.env();
    #data <- list(f, e);
    serialize(f, con, ascii=FALSE, version=2);
    print("sent");
    flush(con);
    unserialize(con)
}

cmpfun <- function(f, options=NULL) {
    con <- socketConnection(port=1337, blocking=TRUE);
    res <- cmpfuncon(f, con, options);
    close(con);
    res
}

f <- function(x) {
    x + 2
}

fib <- function(n) {
    a <- 0;
    b <- 1;
    while (n >=0) {
        tmp <- b;
        b <- a + b;
        a <- tmp;
        n <- b - 1;
    }
    a
}

f(2);
fc <- cmpfun(fib)
fc
