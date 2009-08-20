summary.mcmc.list.dc <-
function(object, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...)
{
    class(object) <- "mcmc.list"
    out <- summary(object, quantiles = quantiles, ...)
    k <- nclones(object)
    out$nclones <- k
    out$converged <- attr(object, "converged")
    nch <- nchain(object)
    n <- niter(object)
    allch <- report(object, array)
    SD <- sqrt(k) * apply(allch, 2, sd)
    out$statistics <- cbind(out$statistics[,1:2],
        "DC SD"=SD, out$statistics[,3:4])
    if (nch > 1) {
        rhat <- gelman.diag(object)$psrf[,1]
        out$statistics <- cbind(out$statistics, "R hat" = rhat)
    }
    class(out) <- "summary.mcmc.dc"
    out
}
