`coef.singleocc` <-
function (object, model = c("full", "occ", "det"), ...)
{
    model <- match.arg(model)
    rval <- object$coefficients
    rval <- switch(model,
        full = structure(c(rval$occ, rval$det),
            .Names = c(paste("occ", names(rval$occ), sep = "_"),
            paste("det", names(rval$det), sep = "_"))),
        occ = rval$occ,
        det = rval$det)
    return(rval)
}

