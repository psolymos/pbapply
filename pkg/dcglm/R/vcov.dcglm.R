vcov.dcglm <-
function(object, ...) {
    rval <- vcov(object$mcmc, ...)
    rownames(rval) <- colnames(rval) <- names(coef(object))
    rval
}

