dclone.default <-
function(x, n.clones=1, attrib=TRUE, ...)
{
    if (n.clones==1)
        return(x)
    if (!is.null(dim(x)) || is.list(x)) {
        NAMES <- row.names(x)
        rval <- lapply(as.data.frame(x), function(z) rep(z, n.clones))
        rval <- as.data.frame(rval)
        if (!is.null(NAMES))
            row.names(rval) <- paste(NAMES, rep(1:n.clones, each=length(NAMES)), sep="_")
        if (is.matrix(x)) {
            rval <- as.matrix(rval)
            colnames(rval) <- colnames(x)
        }
        if (is.list(x) && !is.data.frame(x)) {
            rval <- as.list(rval)
            attr(rval,"row.names") <- NULL
        }
    } else {
        NAMES <- names(x)
        rval <- rep(array(x), n.clones)
        if (!is.null(NAMES))
            names(rval) <- paste(NAMES, rep(1:n.clones, each=length(NAMES)), sep="_")
    }
    if (attrib) {
        attr(rval, "n.clones") <- n.clones
        attr(attr(rval, "n.clones"), "method") <- "rep"
    }
    rval
}
