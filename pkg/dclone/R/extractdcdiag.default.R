extractdcdiag.default <- 
function(x, ...)
{
    chisq <- chisq.diag(x)$statistics
    n.clones <- ifelse(is.null(nclones(x)), 1, nclones(x))
    if (nchain(x) > 1) {
        abin <- getOption("dclone.autoburnin")
        rhat <- if (nvar(x) > 1) 
            gelman.diag(x, autoburnin=abin)$mpsrf
            else gelman.diag(x, autoburnin=abin)$psrf[,1]
    } else rhat <- NA
    rval <- c(n.clones = n.clones,
#        n.params = nvar(x),
        lambda.max = lambdamax.diag(x),
        ms.error = chisq$ms.error,
        r.squared = chisq$r.squared,
        r.hat = rhat)
    rval
}
