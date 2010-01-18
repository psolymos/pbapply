extractdcdiag.default <- 
function(x, probs = c(0, 1), ...)
{
    chisq <- chisq.diag(x, probs=probs)$statistics
    n.clones <- ifelse(is.null(nclones(x)), 1, nclones(x))
    abin <- getOption("dclone")$r.hat$autoburnin
    if (nchain(x) > 1) {
        abin <- getOption("dclone")$r.hat$autoburnin
        rhat <- gelman.diag(x, autoburnin=abin)$mpsrf
    } else rhat <- NA
    rval <- c(n.clones = n.clones,
#        n.params = nvar(x),
        lambda.max = lambdamax.diag(x),
        ms.error = chisq$ms.error,
        r.squared = chisq$r.squared,
        r.hat = rhat)
    rval
}
