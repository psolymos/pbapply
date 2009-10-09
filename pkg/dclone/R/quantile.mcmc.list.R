quantile.mcmc.list <-
function(x, ...)
    apply(report(x, array), 2, quantile, ...)

