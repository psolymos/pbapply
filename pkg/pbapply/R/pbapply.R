pbapply <-
function (X, MARGIN, FUN, ...)
{
    if (is.null(dim(X)) || length(dim(X)) != 2)
        stop("X must have 2 dimensions")
    if (MARGIN == 2)
        X <- t(X) 
    Z <- lapply(1:NROW(X), function(i) X[i,])
    names(Z) <- rownames(X)
    FUN <- match.fun(FUN)
    pbsapply(Z, FUN, ...)
}

