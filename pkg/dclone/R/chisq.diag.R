chisq.diag <- function(x)
{
#    probs <- c(0, 1)
    mcmc <- mcmcapply(x, array)
    mn <- coef(x)
    npar <- length(mn)
    ## inverse of the var-cov matrix of the posterior
    vci <- solve(var(mcmc))
    ## theoretical quantiles for the Chi-square distribution
    pval <- ((1:nrow(mcmc)) - 0.5) / nrow(mcmc)
    qchi <- qchisq(pval, npar)
    ## empirical squared generalized diatsnces
    sdsq <- sort(apply(mcmc, 1, function(z) (t(z-mn)) %*% vci %*% (z-mn)))
#    id <- pval >= min(probs) & pval <= max(probs)
#    qchi <- qchi[id]
#    sdsq <- sdsq[id]
    rval <- list(quantiles = list(
            theoretical=qchi,
            empirical=sdsq),
        statistics = list(
            ## mean squared error
            ms.error = mean((sdsq - qchi)^2),
            ## r^2
            r.squared = 1 - cor(qchi, sdsq)^2))
    class(rval) <- "chisq.diag"
    attr(rval, "npar") <- npar
#    attr(rval, "probs") <- probs
    rval
}
