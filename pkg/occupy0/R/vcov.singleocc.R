vcov.singleocc <-
function (object, model = c("full", "occ", "det"), ...)
{
    model <- match.arg(model)
    if (object$penalized) {
        rval <- -solve(object$results$pmle$hessian)
    } else {
        if (object$method=="optim") {
            rval <- -solve(object$results$mle$hessian)
        }
        if (object$method=="dc") {
            rval <- vcov(object$results$mle)
        }
    }
    cf <- coef(object, model)
    wi <- seq(along = object$coefficients$occ)
    if (model == "occ")
        rval <- rval[wi, wi]
    if (model == "det")
        rval <- rval[-wi, -wi]
    colnames(rval) <- rownames(rval) <- names(cf)
    return(rval)
}

