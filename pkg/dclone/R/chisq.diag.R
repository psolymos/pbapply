chisq.diag <- function(x) {
    mcmc <- mcmcapply(x, array)
    mn <- coef(x)
    ## inverse of the var-cov matrix of the posterior
    vci <- solve(var(mcmc))
    ## theoretical quantiles for the Chi-square distribution
    qchi <- qchisq(((1:nrow(mcmc)) - 0.5) / nrow(mcmc), ncol(mcmc))
    ## empirical squared generalized diatsnces
    sdsq <- sort(apply(mcmc, 1, function(z) (t(z-mn)) %*% vci %*% (z-mn)))
    rval <- list(quantiles = list(
            theoretical=qchi,
            empirical=sdsq),
        statistics = list(
            ## mean squared error
            ms.error = mean((sdsq - qchi)^2),
            ## r^2
            r.squared = 1 - cor(qchi, sdsq)^2))
    class(rval) <- "chisq.diag"
    attr(rval, "npar") <- length(mn)
    rval
}
