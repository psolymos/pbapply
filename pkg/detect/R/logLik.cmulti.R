logLik.cmulti <- 
function (object, ...) 
{
    structure(object$loglik, df = object$nobs - object$df.residual, 
        nobs = object$nobs, class = "logLik")
}
