dclone.list <- function(x, n.clones=1, 
multiply=NULL, unchanged=NULL, attrib=TRUE, ...)
{
    if (n.clones == 1)
        return(x)
    if (!is.null(multiply)) {
        isMult <- multiply %in% names(x)
        if (!all(isMult)) {
            warning("unused elements in 'multiply'")
            multiply <- multiply[isMult]
        }
    }
    if (!is.null(unchanged)) {
        isUnch <- unchanged %in% names(x)
        if (!all(isUnch)) {
            warning("unused elements in 'unchanged'")
            unchanged <- unchanged[isUnch]
        }
    }
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
