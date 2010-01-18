dcdiag.default <-
function(x, probs = c(0, 1), ...) {
    dct <- is.null(attr(x, "dcdiag"))
    ll <- length(list(...))
    if (ll || (!ll && dct)) {
        obj <- list(x, ...)
        k <- sapply(obj, nclones)
        k[sapply(k, is.null)] <- 1
        k <- unlist(k)
        ord <- order(k)
        obj <- obj[ord]
        k <- k[ord]
        dctmp <- t(sapply(obj, extractdcdiag.default, probs=probs))
        Call <- match.call()
        mnam <- as.character(Call[-1])
        rval <- as.data.frame(dctmp)
        rownames(rval) <- mnam[ord]
    } else {
        rval <- attr(x, "dcdiag")
    }
    class(rval) <- c("dcdiag", class(rval))
    rval
}

