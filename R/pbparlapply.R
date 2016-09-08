pbparlapply <-
function (X, FUN, ..., cl = NULL)
{
    if (is.null(cl)) {
        ## detect cores if cl=NULL
        cl <- detectCores(logical = TRUE)
        ## stop if unknown
        if (is.na(cl))
            stop("Cannot detect the number of CPU cores, specify cl argument")
        ## make cluster
        cl <- parallel::makeCluster(cl)
        ## stop cluster upon exit
        on.exit(stopCluster(cl), add = TRUE)
    }
    ## check class of cl arg
    if (!inherits(cl, "cluster"))
        stop("cl argument must be of class 'cluster'")
    ## this check is not necessary
    ## pbparlapply can be combined with pblapply anyways ???
    if (length(cl) < 2L)
        stop("cluster length must be > 1")


    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    if (!(interactive() && dopb() && length(X) >= 1))
        return(parallel::parLapply(cl, X, FUN, ...))

    ## define split here and use that for counter
    Split <- splitpb(length(X), length(cl))
    B <- length(Split)
    pb <- startpb(0, B)
    on.exit(closepb(pb), add = TRUE)
    rval <- vector("list", B)
    for (i in seq_len(B)) {
        ## use parLapply source code to go over splits
        rval[i] <- list(parallel::parLapply(cl, X[Split[[i]]], FUN, ...))
        setpb(pb, i)
    }
    ## assemble output list
    rval <- do.call(c, rval, quote = TRUE)
    names(rval) <- names(X)
    rval
}


splitpb <- function(nx, ncl) {
    i <- seq_len(nx)
    if (ncl == 0L)
        return(list())
    if (ncl == 1L || nx == 1L)
        list(i) else structure(split(i, 1 + (i-1) %/% ncl), names = NULL)
}

if (FALSE) {

set.seed(1234)
n <- 5000
x <- rnorm(n)
y <- rnorm(n, model.matrix(~x) %*% c(0,1), sd=0.5)
d <- data.frame(y, x)
## model fitting and bootstrap
mod <- lm(y~x, d)
ndat <- model.frame(mod)
B <- 100
bid <- sapply(1:B, function(i) sample(nrow(ndat), nrow(ndat), TRUE))
fun <- function(z) {
    if (missing(z))
        z <- sample(nrow(ndat), nrow(ndat), TRUE)
    coef(lm(mod$call$formula, data=ndat[z,]))
}

## standard '*apply' functions
system.time(res1 <- pblapply(1:B, function(i) fun(bid[,i])))

cl <- makeCluster(cl)
clusterExport(cl, c("fun", "mod", "ndat", "bid"))
system.time(res2 <- pbparlapply(1:B, function(i) fun(bid[,i]), cl = cl))
stopCluster(cl)

## todo:
## - use on.exit(closepb(pb)) in other functions
## - ise seq_len(B) in for (i in 1:B)
## - remind folks that auto defined cl might not work (see example)
## - remind folks that safe RNG is not set up automagically

}
