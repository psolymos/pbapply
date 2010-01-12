confint.dcglm <-
function(object, parm, level = 0.95, ...) {
    rval <- confint(object$mcmc, parm, level, ...)
    rownames(rval) <- names(coef(object))
    rval
}

