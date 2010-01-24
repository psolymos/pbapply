vcov.mcmc.list.dc <-
function(object, invfisher=TRUE, ...)
{
    rval <- cov(mcmcapply(object, array))
    if (invfisher)
        rval <- rval * nclones(object)
    rval
}
