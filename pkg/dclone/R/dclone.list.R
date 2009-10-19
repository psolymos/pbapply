dclone.list <- function(x, n.clones=1, 
multiply=NULL, unchanged=NULL, attrib=TRUE, ...)
{
    if (n.clones == 1)
        return(x)
    out <- lapply(x, dclone, n.clones=n.clones, attrib=attrib, ...)
    if (!is.null(multiply))
        for (i in 1:length(multiply)) {
            tmp <- x[[multiply[i]]] * n.clones
            if (attrib) {
                attr(tmp, "n.clones") <- n.clones
                attr(attr(tmp, "n.clones"), "method") <- "multi"
            }
            out[[multiply[i]]] <- tmp
        }
    if (!is.null(unchanged))
        out[unchanged] <- x[unchanged]
    out
}
