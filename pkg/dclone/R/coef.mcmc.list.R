coef.mcmc.list <-
function (object, ...)
{
    rval <- mcmcapply(object, mean, ...)
    names(rval) <- varnames(object, allow.null=FALSE)
    rval
}
