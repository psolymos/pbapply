nclones.list <-
function(x, ...)
{
    if (!is.null(attr(x, "n.clones"))) {
        nc <- attr(x, "n.clones")
    } else {
#        nc <- unique(unlist(lapply(x, nclones.default)))
        klist <- lapply(x, nclones.default)
        nc <- unique(unlist(klist))
        if (is.null(nc))
            return(NULL)
        if (length(nc) > 1)
            stop("non unique value")
        method <- lapply(klist, function(z) attr(z, "method"))
        method[sapply(method, is.null)] <- NA
        attr(nc, "method") <- unlist(method)
    }
    nc
}

