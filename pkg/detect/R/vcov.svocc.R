vcov.svocc <-
function (object, model = c("full", "sta", "det"), type, ...)
{
    model <- match.arg(model)
    boot <- extractBOOT(object)
    if (missing(type)) {
        type <- "mle"
        if (object$penalized && !is.null(boot))
            type <- "boot"
    }
    type <- match.arg(type, c("mle", "boot"))
    if (type == "mle" && object$penalized)
        stop("refit model with 'penalized=FALSE' to get MLE")
    if (type == "boot" && is.null(boot))
        stop("no bootstrap results found")
    if (type != "boot")
        boot <- NULL

    if (type == "boot") {
        rval <- boot$vcov
    } else {
        if (object$method=="optim") {
            rval <- -solve(object$results$mle$hessian)
        }
        if (object$method=="dc") {
            rval <- vcov(object$results$mle)
        }
    }
    cf <- coef(object, model)
    wi <- seq(along = object$coefficients$sta)
    if (model == "sta")
        rval <- rval[wi, wi]
    if (model == "det")
        rval <- rval[-wi, -wi]
    colnames(rval) <- rownames(rval) <- names(cf)
    return(rval)
}

