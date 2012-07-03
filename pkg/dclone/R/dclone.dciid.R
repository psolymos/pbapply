dclone.dciid <-
function(x, n.clones = 1, attrib = TRUE, ...)
{
    iid <- attr(x, "iid")
    if (is.null(iid)) {
        rval0 <- dclone.default(x, n.clones=n.clones, attrib=attrib, ...)
        if (n.clones < 2)
            return(rval0)
        val <- x
        tmp <- t(sapply(val, "+", (1:n.clones*max(val)) - max(val)))
        rval <- as.numeric(tmp)
        iid <- character(0)
        attributes(rval) <- attributes(rval0)
    } else {
        rval <- dclone.default(x, n.clones=n.clones, attrib=attrib, ...)
        if (n.clones < 2)
            return(rval)
        for (i in iid) {
            val <- x[,i]
            tmp <- t(sapply(val, "+", (1:n.clones*max(val)) - max(val)))
            rval[,i] <- as.numeric(tmp)
        }
    }
    attr(attr(rval, "n.clones"), "method") <- "iid"
    attr(attr(attr(rval, "n.clones"), "method"), "iid") <- iid
    rval
}
