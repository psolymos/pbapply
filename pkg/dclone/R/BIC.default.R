BIC.default <-
function(object, ...) {
    if (length(list(...))) {
        obj <- list(object, ...)
        nobs <- nobservations(obj[[1]])
        if (!all(nobs == sapply(obj[-1], nobservations)))
            stop("numbers of observations are not equal")
        rval <- AIC(object, ..., k=log(nobs))
        colnames(rval)[2] <- "BIC"
        Call <- match.call()
        rownames(rval) <- as.character(Call[-1])
        rval
    } else AIC(object, ..., k=log(nobservations(object)))
}
