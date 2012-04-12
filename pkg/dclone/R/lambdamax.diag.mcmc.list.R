lambdamax.diag.mcmc.list <-
function(x, ...)
{
    rval <- max(eigen(var(as.matrix(x)), symmetric=TRUE, only.values=TRUE)$values)
    class(rval) <- "lambdamax.diag"
    attr(rval, "npar") <- nvar(x)
    attr(rval, "n.clones") <- nclones(x)
    rval
}
