coef.svocc <-
function (object, model = c("full", "sta", "det"), ...)
{
    model <- match.arg(model)
    rval <- object$coefficients
    rval <- switch(model,
        full = structure(c(rval$sta, rval$det),
            .Names = c(paste("sta", names(rval$sta), sep = "_"),
            paste("det", names(rval$det), sep = "_"))),
        sta = rval$sta,
        det = rval$det)
    return(rval)
}

