splitpb <-
function(nx, ncl, nout = NULL)
{
    i <- seq_len(nx)
    if (ncl == 0L)
        return(list())
    k <- if (is.null(nout))
        1L else i[which.min(abs(ceiling(nx / (i*ncl)) - nout))]
    g <- 1L + (i - 1L) %/% as.integer(ncl * k)
    structure(split(i, g), names = NULL)
}
