#createEnv <- function(env) {
    #res <- as.environment(as.list(env, all.names=TRUE));
    #env <- parent.env(env);
    #tmp <- res
    #while(!identical(env, emptyenv())) {
        #parent.env(tmp) <- as.environment(as.list(env, all.names=TRUE));
        #tmp <- parent.env(tmp)
        #env <- parent.env(env);
    #}
    #parent.env(tmp) <- emptyenv();
    #res
#}
#
#cmpfuncon <- function(f, con, options=NULL) {
    ##e <- createEnv(environment(f));
    ##e <- new.env();
    ##data <- list(f, e);
    #serialize(f, con, ascii=FALSE, version=2);
    #print("sent");
    #flush(con);
    #unserialize(con)
#}
#
#cmpfun <- function(f, options=NULL) {
    #con <- socketConnection(port=1337, blocking=TRUE);
    #res <- cmpfuncon(f, con, options);
    #close(con);
    #res
#}

compiler::enableJIT(0);

f1 <- function(x) x + 2

f2 <- function(x) {
    x + 2
}

f3 <- function(x, y) x + x + x + y

fib <- function(n) {
    a <- 0;
    b <- 1;
    while (n > 0) {
        tmp <- a + b;
        a <- b;
        b <- tmp;
        n <- n - 1;
    }
    a
}

fib_rec <- function(n) {
    if (n <= 1) return (n);
    fib_rec(n - 1) + fib_rec(n - 2)
}

f <- fib

fib_rec(5);
fc <- argtracer::server_cmpfun(fib_rec)
print("res");
fc(5)
