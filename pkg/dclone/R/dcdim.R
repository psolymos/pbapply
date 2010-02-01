dcdim <-
function(x, drop=TRUE, perm=NULL)
{
    if (!is.null(perm)) {
        if (!drop)
            stop("'drop' must be 'TRUE' to set 'perm'")
        if (is.null(dim(x)))
            stop("'x' must be an array to set 'perm'")
        if (dim(x)[perm] != 1)
            stop("subscript for 'perm' must be 1")
        if (perm > length(dim(x)))
            stop("'perm' must <= 'length(dim(x))'")
        if (perm == length(dim(x)))
            perm <- NULL
        attr(x, "perm") <- perm
    }
    if (!is.null(dim(x)))
        attr(x, "drop") <- drop
    class(x) <- c("dcdim", class(x))
    x
}
