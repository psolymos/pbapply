dcdiag.default <-
function(x, ...) 
{
    dct <- is.null(attr(x, "dcdiag"))
    ll <- length(list(...))
    if (ll || (!ll && dct)) {
        obj <- list(x, ...)
        vnams <- lapply(obj, varnames)
        if (!setequal(vnams[[1]], unique(unlist(vnams))))
            stop("parameter names in each model should be identical")
        k <- sapply(obj, nclones)
        k[sapply(k, is.null)] <- 1
        k <- unlist(k)
        ord <- order(k)
        obj <- obj[ord]
        k <- k[ord]
        dctmp <- t(sapply(obj, dclone:::extractdcdiag.default))
        Call <- match.call()
        mnam <- as.character(Call[-1])
        rval <- as.data.frame(dctmp)
        rownames(rval) <- mnam[ord]
    } else {
        rval <- attr(x, "dcdiag")
    }
    colnames(rval) <- c("n.clones", "lambda.max", "ms.error", "r.squared", "r.hat")
    class(rval) <- c("dcdiag", class(rval))
    rval
}

