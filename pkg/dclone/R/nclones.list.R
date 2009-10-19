nclones.list <-
function(x, ...)
{
    if (!is.null(attr(x, "n.clones"))) {
        return(attr(x, "n.clones"))
    } else {
        nc <- unique(unlist(lapply(x, nclones.default)))
        if (length(nc) > 1)
            stop("non unique value")
        return(nc)
    }
}

