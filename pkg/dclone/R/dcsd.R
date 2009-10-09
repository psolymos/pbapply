dcsd <-
function(x, na.rm = FALSE)
{
    ncl <- nclones(x)
    if (is.null(ncl))
        ncl <- 1
    mcmcapply(x, sd, na.rm=na.rm) * sqrt(ncl)
}
