summary.mcmc.list.dc <-
function(object, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975), ...)
{
    class(object) <- "mcmc.list"
    out <- summary(object, quantiles = quantiles, ...)
    out$nclones <- nclones(object)
    if (nchain(object) > 1) {
        abin <- getOption("dcoptions")$autoburnin
        rhat <- gelman.diag(object, autoburnin = abin)$psrf[,1]
    } else rhat <- NA
    if (nvar(object) > 1) {
        out$statistics <- cbind(out$statistics[,1:2], "DC SD"=dcsd(object), out$statistics[,3:4], "R hat" = rhat)
    } else {
        tmp <- c(out$statistics[1:2], "DC SD"=dcsd(object), out$statistics[3:4], "R hat" = rhat)
        out$statistics <- matrix(tmp, nrow=1)
        dimnames(out$statistics) <- list(varnames(object), names(tmp))
    }
    class(out) <- "summary.mcmc.dc"
    out
}
