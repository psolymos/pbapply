dclone.dcinla <-
function(x, n.clones = 1, attrib = TRUE, ...)
{
    rval <- dclone.default(x, n.clones=n.clones, attrib=attrib, ...)
    for (i in attr(x, "iid")) {
        val <- x[,i]
        tmp <- t(sapply(val, "+", (1:n.clones*max(val)) - max(val)))
        rval[,i] <- as.numeric(tmp)
    }
    rval
}
