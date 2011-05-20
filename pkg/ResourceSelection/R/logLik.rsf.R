logLik.rsf <-
function (object, ...)
{
    structure(object$loglik,
        df = object$np,
#        nobs = sum(object$y),
        class = "logLik")
}

