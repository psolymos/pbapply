extractMLE.svocc <-
function(object, model = c("full", "sta", "det"), ...)
{
    model <- match.arg(model)
    if (object$method=="optim") {
        cfs <- object$results$mle$par
        ses <- sqrt(-diag(solve(object$results$mle$hessian)))
        vcv <- -solve(object$results$mle$hessian)
    }
    if (object$method=="dc") {
        cfs <- coef(object$results$mle)
        ses <- dcsd(object$results$mle)
        vcv <- vcov(object$results$mle)
    }
    cf <- coef(object, model)
    wi <- seq(along = object$coefficients$sta)
    if (model == "sta") {
        cfs <- cfs[wi]
        ses <- ses[wi]
        vcv <- vcv[wi, wi]
    }
    if (model == "det") {
        cfs <- cfs[-wi]
        ses <- ses[-wi]
        vcv <- vcv[-wi, -wi]
    }
    names(cfs) <- names(cfs) <- names(cf)
    names(ses) <- names(ses) <- names(cf)
    colnames(vcv) <- rownames(vcv) <- names(cf)
    list(coefficients=cfs, std.error=ses, vcov=vcv)
}

