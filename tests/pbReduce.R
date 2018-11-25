if (FALSE) {

pbReduce <-
function(f, x, init, right = FALSE, accumulate = FALSE)
{
    mis <- missing(init)
    len <- length(x)
    if (len == 0L)
        return(if (mis) NULL else init)
    f <- match.fun(f)
    if (!is.vector(x) || is.object(x))
        x <- as.list(x)
    ind <- seq_len(len)
    pb <- startpb(0, len)
    on.exit(closepb(pb))
    j <- 0L
    if (mis) {
        if (right) {
            init <- x[[len]]
            ind <- ind[-len]
        } else {
            init <- x[[1L]]
            ind <- ind[-1L]
        }
    }
    if (!accumulate) {
        if (right) {
            for (i in rev(ind))
                #setpb(pb, j)
                #init <- forceAndCall(2, f, x[[i]], init)
                init <- f(x[[i]], init)
                #j <- j + 1L
        } else {
            for (i in ind)
                #setpb(pb, j)
                #init <- forceAndCall(2, f, init, x[[i]])
                init <- f(init, x[[i]])
                j <- j + 1L
        }
        init
    } else {
        len <- length(ind) + 1L
        out <- vector("list", len)
        if (mis) {
            if (right) {
                out[[len]] <- init
                for (i in rev(ind)) {
                    #setpb(.e$pb, .e$.j)
                    #init <- forceAndCall(2, f, x[[i]], init)
                    init <- f(x[[i]], init)
                    out[[i]] <- init
                    #.e$.j <- .e$.j + 1L
                }
            } else {
                out[[1L]] <- init
                for (i in ind) {
                    #setpb(.e$pb, .e$.j)
                    #init <- forceAndCall(2, f, init, x[[i]])
                    init <- f(init, x[[i]])
                    out[[i]] <- init
                    #.e$.j <- .e$.j + 1L
                }
            }
        } else {
            if (right) {
                out[[len]] <- init
                for (i in rev(ind)) {
                    #setpb(.e$pb, .e$.j)
                    #init <- forceAndCall(2, f, x[[i]], init)
                    init <- f(x[[i]], init)
                    out[[i]] <- init
                    #.e$.j <- .e$.j + 1L
                }
            } else {
                for (i in ind) {
                    #setpb(.e$pb, .e$.j)
                    out[[i]] <- init
                    #init <- forceAndCall(2, f, init, x[[i]])
                    init <- f(init, x[[i]])
                    #.e$.j <- .e$.j + 1L
                }
                out[[len]] <- init
            }
        }
        if (all(lengths(out) == 1L))
            out <- unlist(out, recursive = FALSE)
        out
    }
}

library(pbapply)
sum(1:10^4) # 50005000
Reduce("+", 1:10^4) # 50005000
pboptions(type="none")
pbReduce("+", 1:10^4) # 10001


}

