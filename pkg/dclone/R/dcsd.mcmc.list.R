dcsd.mcmc.list <-
function(object, ...)
{
    ncl <- nclones(object)
    if (is.null(ncl))
        ncl <- 1
    rval <- mcmcapply(object, sd) * sqrt(ncl)
    names(rval) <- varnames(object, allow.null=FALSE)
    rval
}
