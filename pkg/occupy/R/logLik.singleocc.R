`logLik.singleocc` <-
function (object, ...)
{
    structure(object$loglik,
        df = object$n - object$df.residual,
        nobs = object$n,
        class = "logLik")
}

