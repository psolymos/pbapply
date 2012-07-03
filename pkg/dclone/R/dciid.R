dciid <-
function(x, iid = character(0))
{
    if (!is.null(dim(x))) {
        if (length(dim(x)) != 2)
            stop("'x' must have 2 dimensions")
        if (is.null(colnames(x)))
            stop("'x' must have column names")
        if (is.numeric(iid) || is.logical(iid))
            iid <- colnames(x)[iid]
        cn <- intersect(colnames(x), iid)
        if (length(cn) != length(iid))
            warning("elements of 'iid' not in colnames were dropped")
        if (!all(sapply(x, mode)[iid] == "numeric"))
            stop("'iid' columns must be numeric")
        attr(x, "iid") <- cn
    }
    class(x) <- c("dciid", class(x))
    x
}
