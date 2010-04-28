quantile.mcmc.list <-
function(x, ...)
    apply(mcmcapply(x, array), 2, quantile, ...)
