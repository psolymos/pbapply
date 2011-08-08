model.matrix.svabu <-
function (object, model = c("sta", "det", "zif"), ...)
{
    model <- match.arg(model)
    if (!is.null(object$x))
        rval <- object$x[[model]]
    else if (!is.null(object$model)) {
        rval <- if (model == "zif" && length(coef(object, "zif")) == 1) {
            matrix(1, object$nobs, 1)
        } else model.matrix(object$terms[[model]], object$model,
            contrasts = object$contrasts[[model]])
        if (model == "zif") {
            cn <- colnames(rval)
            cn <- gsub("(.*)(zif\\()", "\\1", cn)
            cn[-1] <- gsub("(.*)(\\))", "\\1", cn[-1])
            colnames(rval) <- cn
        }
    } else stop("not enough information in fitted model to return model.matrix")
    return(rval)
}
