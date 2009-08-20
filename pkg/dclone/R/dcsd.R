dcsd <-
function(x, na.rm = FALSE)
{
    ncl <- nclones(x)
    if (is.null(ncl))
        ncl <- 1
    sd(x, na.rm=na.rm) * sqrt(ncl)
}
