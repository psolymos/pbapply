coef.mcmc.list <-
function (object, ...)
{
    mcmcapply(object, mean, ...)
}
