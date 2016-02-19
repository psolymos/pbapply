pblapply <-
function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    B <- length(X)
    if (!(interactive() && dopb() && B >= 1)) 
        return(lapply(X, FUN, ...))
    pb <- startpb(0, B)
    rval <- vector("list", B)
    for (i in 1:B) {
        rval[i] <- list(FUN(X[[i]], ...))
        setpb(pb, i)
    }
    closepb(pb)
    names(rval) <- names(X)
    rval
}
