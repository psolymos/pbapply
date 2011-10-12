mcmcapply <-
function(x, FUN, ...)
{
    if (!inherits(x, "mcmc.list"))
        stop("object class is not 'mcmc.list'")
    y <- as.matrix(x)
    if (missing(FUN))
        y else apply(y, 2, match.fun(FUN), ...)
}
