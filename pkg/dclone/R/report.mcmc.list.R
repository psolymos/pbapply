report.mcmc.list <-
function(x, FUN, ...)
{
    FUN <- match.fun(FUN)
    nc <- nchain(x)
    nv <- nvar(x)
    ni <- nrow(x[[1]])
    if (nv == 1) {
        y <- matrix(unlist(x), ncol=1)
    } else {
        if (nc > 1) {
            y <- vector("list", nv)
            for (j in 1:nv) {
                y[[j]] <- matrix(0, ni, nc)
                for (i in 1:nc) {
                    y[[j]][, i] <- x[[i]][, j]
                }
            }
            y <- sapply(y, array)
        } else y <- x[[1]]
        colnames(y) <- varnames(x)
    }
    apply(y, 2, FUN, ...)
}

