extractdcdiag.default <- 
function(x, ...)
{
    lmax <- try(lambdamax.diag(x))
    if (inherits(lmax, "try-error"))
            lmax <- NA
    chisq <- try(chisq.diag(x))
    if (inherits(chisq, "try-error")) {
        ms.error <- NA
        r.squared <- NA
    } else {
        ms.error <- chisq$statistics$ms.error
        r.squared <- chisq$statistics$r.squared
    }
    n.clones <- ifelse(is.null(nclones(x)), 1, nclones(x))
    if (nchain(x) > 1) {
        abin <- getOption("dcoptions")$autoburnin
        if (nvar(x) > 1) {
            rhat <- try(gelman.diag(x, autoburnin=abin)$mpsrf)
        } else {
            rhat <- try(gelman.diag(x, autoburnin=abin)$psrf[,1])
        }
        if (inherits(rhat, "try-error"))
            rhat <- NA
    } else rhat <- NA
    rval <- c(n.clones = n.clones,
#        n.params = nvar(x),
        lambda.max = lmax,
        ms.error = ms.error,
        r.squared = r.squared,
        r.hat = rhat)
    rval
}
