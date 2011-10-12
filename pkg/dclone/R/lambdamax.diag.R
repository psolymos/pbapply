lambdamax.diag <-
function(x)
{
#    npar <- nvar(x)
#    y <- as.matrix(x) # mcmcapply(x, array)
#    rval <- if (npar == 1) {
#        sd(y)^2
#    } else {
#        max(eigen(var(y), symmetric=TRUE, only.values=TRUE)$val)
#    }
    rval <- max(eigen(var(as.matrix(x)), symmetric=TRUE, only.values=TRUE)$values)
    class(rval) <- "lambdamax.diag"
#    attr(rval, "npar") <- npar
    attr(rval, "npar") <- nvar(x)
    attr(rval, "n.clones") <- nclones(x)
    rval
}
