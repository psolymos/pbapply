vcov.mcmc.list.dc <-
function(object, ...)
    cov(mcmcapply(object, array))
