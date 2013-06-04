dctable.default <-
function (x, ...) 
{
    dct <- is.null(attr(x, "dctable"))
    ll <- length(list(...))
    if (ll || (!ll && dct)) {
        obj <- list(x, ...)
        k <- sapply(obj, nclones)
        k[sapply(k, is.null)] <- 1
        k <- unlist(k)
        ord <- order(k)
        obj <- obj[ord]
        k <- k[ord]
        dctmp <- lapply(obj, extractdctable.default)
        rnam <- lapply(dctmp, rownames)
        nam <- rnam[[1]]
        if (!setequal(nam, unique(unlist(rnam)))) 
            stop("parameter names in each model should be identical")
        rval <- vector("list", length(nam))
        names(rval) <- rownames(dctmp[[1]])
        Call <- match.call()
        mnam <- as.character(Call[-1])
        for (i in 1:length(nam)) {
            rval[[i]] <- cbind(n.clones = k, t(sapply(dctmp, 
                function(z) z[i, ])))
            if (length(mnam) > 1) 
                rownames(rval[[i]]) <- mnam[ord]
            else rownames(rval[[i]]) <- deparse(substitute(x))
        }
        rval <- lapply(rval, function(z) as.data.frame(z))
    }
    else {
        rval <- attr(x, "dctable")
    }
    class(rval) <- "dctable"
    rval
}
