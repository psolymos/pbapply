vcov.svabu <-
function (object, model = c("full", "sta", "det", "zif"), type, ...)
{
    model <- match.arg(model)
    if (!object$zeroinfl && model == "zif")
        stop("not ZIF component found")
    boot <- extractBOOT(object)
    if (missing(type)) {
        type <- if (is.null(boot))
        "cmle" else "boot"
    }
    type <- match.arg(type, c("cmle", "boot"))
    if (type == "boot" && is.null(boot))
        stop("no bootstrap results found")
#    if (model == "phi" && type == "cmle")
#        stop("bootstrap is required to provide values for 'phi'")
    nps <- sapply(object$coefficients, length)

    if (type == "boot") {
        rval <- boot$vcov
    } else {
        rval <- matrix(NA, sum(nps), sum(nps))
        rval[1:sum(nps[1:2]), 1:sum(nps[1:2])] <- solve(object$results$count$hessian)
        if (object$zeroinfl)
            rval[((sum(nps[1:2])+1):sum(nps)), ((sum(nps[1:2])+1):sum(nps))] <- solve(object$results$zero$hessian)
    }

#    if (model == "full" && object$zeroinfl && type == "cmle")
#        cf <- cf[-length(cf)]
#    wi <- seq(along = object$coefficients$sta)
#    if (model == "sta")
#        rval <- data.matrix(rval[wi, wi])
#    if (model == "det") {
#        wii <- if (type == "cmle")
#            wi else c(wi, ncol(rval))
#        rval <- data.matrix(rval[-wii, -wii])
#    }
#    if (model == "phi")
#        rval <- data.matrix(rval[ncol(rval), ncol(rval)])

    id <- switch(model,
        full = 1:sum(nps),
        sta = 1:nps[1],
        det = (nps[1]+1):sum(nps[1:2]),
        zif = (sum(nps[1:2])+1):sum(nps))
    rval <- data.matrix(rval[id, id])
    cf <- coef(object, model)
    colnames(rval) <- rownames(rval) <- names(cf)
    return(rval)
}

