lambdamax.diag <-
function(x)
{
    y <- report(x, array)
    rval <- if (nvar(x) == 1) {
        sd(y)
    } else {
        max(eigen(var(y), symmetric=TRUE, only.values=TRUE)$val)
    }
    rval
}
