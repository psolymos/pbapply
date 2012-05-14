dclone.dciid <-
function(x, n.clones = 1, attrib = TRUE, ...)
{
    rval <- dclone.default(x, n.clones=n.clones, attrib=attrib, ...)
    iid <- attr(x, "iid")
    for (i in iid) {
        val <- x[,i]
        tmp <- t(sapply(val, "+", (1:n.clones*max(val)) - max(val)))
        rval[,i] <- as.numeric(tmp)
    }
    attr(attr(rval, "n.clones"), "method") <- "iid"
    attr(attr(attr(rval, "n.clones"), "method"), "iid") <- iid
    rval
}
