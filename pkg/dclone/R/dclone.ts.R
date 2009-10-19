dclone.ts <- function(x, n.clones=1, attrib=TRUE, ...)
{
    if (n.clones == 1)
        return(x)
    n <- length(x)
    out <- matrix(x, n, n.clones)
    rownames(out) <- names(x)
    colnames(out) <- paste("clone", 1:n.clones, sep=".")
    if (attrib) {
        attr(out, "n.clones") <- n.clones
        attr(attr(out, "n.clones"), "method") <- "rep"
    }
    out
}
