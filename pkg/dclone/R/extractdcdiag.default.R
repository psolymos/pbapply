extractdcdiag.default <- 
function(x, ...)
{
    quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975)
    y <- mcmcapply(x, array)
    Mean <- apply(y, 2, mean)
    Sd <- apply(y, 2, sd)
    rhat <- if (nchain(x) > 1)
        gelman.diag(x)$psrf[,1] else NA
    if (!is.null(quantiles)) {
        Qa <- apply(y, 2, quantile, probs=quantiles)
        rval <- rbind(mean = Mean, sd = Sd, Qa, r.hat=rhat)
    } else {
        rval <- rbind(mean = Mean, sd = Sd, r.hat=rhat)
    }
    t(rval)
}
