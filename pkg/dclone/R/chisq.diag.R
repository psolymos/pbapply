chisq.diag <- 
function(x)
{
    mcmc <- as.matrix(x)
    mn <- coef(x)
    npar <- length(mn)
    ## inverse of the var-cov matrix of the posterior
    vci <- solve(var(mcmc))
    ## theoretical quantiles for the Chi-square distribution
    n <- nrow(mcmc)
    pval <- (seq_len(n) - 0.5) / n
    qchi <- qchisq(pval, npar)
    ## empirical squared generalized distances
    sdsq <- sort(apply(mcmc, 1, function(z) (t(z-mn)) %*% vci %*% (z-mn)))
    rval <- list(
        quantiles = list(
            theoretical=qchi,
            empirical=sdsq),
        statistics = list(
            ms.error = mean((sdsq - qchi)^2), # mean squared error
            r.squared = 1 - cor(qchi, sdsq)^2)) # r^2
    class(rval) <- "chisq.diag"
    attr(rval, "npar") <- npar
    attr(rval, "n.clones") <- nclones(x)
    rval
}
