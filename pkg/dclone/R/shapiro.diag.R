shapiro.diag <-
function(x)
{
    nch <- nchain(x)
    n <- niter(x)
    mcp <- mcpar(x[[1]])
    vmax <- 5000
    allch <- if (nch * n > vmax) {
        mcmcapply(window(x, mcp[1] + n - trunc(vmax / nch), mcp[2]), array)
    } else mcmcapply(x, array)
#    if (standardize) {
#        vcinv <- solve(var(allch))
#        vceig <- eigen(vcinv)
#        vcinvsqrt <- vceig$vectors %*% diag(sqrt(vceig$values)) %*% solve(vceig$vectors)
#        allch <- vcinvsqrt %*% t(scale(allch, center=TRUE, scale=FALSE))
#    } else {
#        allch <- t(allch)
#    }
    if (nvar(x) == 1) {
        rval <- shapiro.test(allch)
    } else {
        require(mvnormtest)
        rval <- mshapiro.test(t(allch))
    }
    rval
}
