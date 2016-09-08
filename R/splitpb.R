splitpb <-
function(nx, ncl)
{
    i <- seq_len(nx)
    if (ncl == 0L)
        return(list())
    if (ncl == 1L || nx == 1L)
        list(i) else structure(split(i, 1 + (i-1) %/% ncl), names = NULL)
}
