splitpb <-
function(nx, ncl, nout = NULL)
{
    i <- seq_len(nx)
    if (ncl == 0L)
        return(list())
    if (is.null(nout)) {
        k <- 1L
    } else {
        if (nout < 1L)
            stop("nout must be > 0")
        k <- max(1L, ceiling(ceiling(nx / ncl) / nout))
    }
    g <- 1L + (i - 1L) %/% as.integer(ncl * k)
    structure(split(i, g), names = NULL)
}
