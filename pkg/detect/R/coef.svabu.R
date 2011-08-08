coef.svabu <-
function (object, model = c("full", "sta", "det", "zif"), ...)
{
    model <- match.arg(model)
    rval <- object$coefficients
    rval <- switch(model,
        full = if (object$zeroinfl) {
                structure(c(rval$sta, rval$det, rval$zif),
                .Names = c(paste("sta", names(rval$sta), sep = "_"),
                paste("det", names(rval$det), sep = "_"),
                paste("zif", names(rval$zif), sep = "_")))
            }else {
                structure(c(rval$sta, rval$det),
                .Names = c(paste("sta", names(rval$sta), sep = "_"),
                paste("det", names(rval$det), sep = "_")))
            },
        sta = rval$sta,
        det = rval$det,
        zif = rval$zif)
    return(rval)
}

