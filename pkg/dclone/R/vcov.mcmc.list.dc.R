vcov.mcmc.list.dc <-
function(object, invfisher=TRUE, ...)
{
    rval <- cov(as.matrix(object), ...)
    if (invfisher)
        rval <- rval * nclones(object)
    rval
}
