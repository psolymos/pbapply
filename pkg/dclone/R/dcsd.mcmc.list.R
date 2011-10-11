dcsd.mcmc.list <-
function(object, ...)
{
    ncl <- nclones(object)
    if (is.null(ncl))
        ncl <- 1
    mcmcapply(object, sd, ...) * sqrt(ncl)
}
