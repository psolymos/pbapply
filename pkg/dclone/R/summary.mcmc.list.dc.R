summary.mcmc.list.dc <-
function(object, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...)
{
    class(object) <- "mcmc.list"
    out <- summary(object, quantiles = quantiles, ...)
    out$nclones <- nclones(object)
    nch <- nchain(object)
    out$statistics <- cbind(out$statistics[,1:2],
        "DC SD"=dcsd(object), out$statistics[,3:4])
    if (nch > 1) {
        abin <- getOption("dclone")$r.hat$autoburnin
        rhat <- gelman.diag(object, autoburnin = abin)$psrf[,1]
        out$statistics <- cbind(out$statistics, "R hat" = rhat)
    }
    class(out) <- "summary.mcmc.dc"
    out
}
