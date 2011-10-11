quantile.mcmc.list <-
function(x, ...)
{
    apply(as.matrix(x), 2, quantile, ...)
}
