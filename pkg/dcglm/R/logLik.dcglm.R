logLik.dcglm <-
function (object, ...)
    structure(object$loglik,
        df = object$df.null + 1 - object$df.residual,
        nobs = object$df.null + 1,
        class = "logLik")

