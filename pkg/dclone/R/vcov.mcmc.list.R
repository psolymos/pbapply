vcov.mcmc.list <-
function(object, ...)
    cov(mcmcapply(object, array))
