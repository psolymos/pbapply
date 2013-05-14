fitted.cmulti <-
function(object, ...){
    type <- object$type
    if (type == "mix")
        stop("fitted values are not available for finite mixture model")
    X <- model.matrix(object$formula, object$model)
    poisson("log")$linkinv(drop(X %*% coef(object)))
}
