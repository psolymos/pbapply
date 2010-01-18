chisq.diag <- function(x) {
    mcmc <- mcmcapply(x, array)
    mn <- coef(x)
    vci <- solve(var(mcmc))
    qchi <- qchisq(((1:nrow(mcmc)) - 0.5) / nrow(mcmc), ncol(mcmc))
    sdsq <- sort(apply(mcmc, 1, function(z) (t(z-mn)) %*% vci %*% (z-mn)))
    rval <- list(quantiles = list(
            theoretical=qchi,
            empirical=sdsq),
        statistics = list(
            error = mean((sdsq - qchi)^2),
            correlation = cor(qchi, sdsq)))
    class(rval) <- "chisq.diag"
    rval
}
