extractdcdiag.default <- 
function(x, ...)
{
    chisq <- chisq.diag(x)$statistics
    n.clones <- ifelse(is.null(nclones(x)), 1, nclones(x))
    abin <- getOption("dclone")$r.hat$autoburnin
    rval <- c(n.clones = n.clones,
        n.params = nvar(x),
        lambda.max = lambdamax.diag(x),
        ms.error = chisq$ms.error,
        r.squared = chisq$r.squared,
        r.hat = gelman.diag(x, autoburnin=abin)$mpsrf)
    rval
}
