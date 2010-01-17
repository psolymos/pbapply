dcdiag.default <-
function(x, ...) {
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
        dctmp <- t(sapply(obj, extractdcdiag.default))
        Call <- match.call()
        mnam <- as.character(Call[-1])
            rval <- data.frame(n.clones=k, dctmp)
    } else {
        rval <- attr(x, "dcdiag")
    }
    class(rval) <- "dcdiag"
    rval
}
