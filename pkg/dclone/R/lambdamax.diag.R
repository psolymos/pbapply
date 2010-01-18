lambdamax.diag <-
function(x)
{
    npar <- nvar(x)
    y <- mcmcapply(x, array)
    rval <- if (npar == 1) {
        sd(y)
    } else {
        max(eigen(var(y), symmetric=TRUE, only.values=TRUE)$val)
    }
    class(rval) <- "lambdamax.diag"
    attr(rval, "npar") <- npar
    rval
}
