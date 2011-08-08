model.matrix.svocc <-
function (object, model = c("sta", "det"), ...)
{
    model <- match.arg(model)
    if (!is.null(object$x))
        rval <- object$x[[model]]
    else if (!is.null(object$model))
        rval <- model.matrix(object$terms[[model]], object$model,
            contrasts = object$contrasts[[model]])
    else stop("not enough information in fitted model to return model.matrix")
    return(rval)
}

