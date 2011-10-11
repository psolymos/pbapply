vcov.mcmc.list.dc <-
function(object, invfisher=TRUE, ...)
{
    rval <- cov(as.matrix(x), ...)
    if (invfisher)
        rval <- rval * nclones(object)
    rval
}
