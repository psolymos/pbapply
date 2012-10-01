CAIC.default <-
function (object, ..., alpha)
{
    if (missing(alpha))
        alpha <- getOption("CAIC_alpha")
    if (is.null(alpha))
        alpha <- 0.5
    if (alpha < 0 || alpha > 1)
        stop("alpha must be in [0, 1]")

    ll <- length(list(...))
    if (ll) {
        obj <- list(object, ...)
        caic <- sapply(obj, function(z) {
            alpha * AIC(z) + (1 - alpha) * BIC(z)
        })
        df <- sapply(obj, function(z) {
            attributes(logLik(z))$df
        })
        rval <- data.frame(df=df, CAIC=caic)
        Call <- match.call()
        Call["alpha"] <- NULL
        rownames(rval) <- as.character(Call[-1])
    }
    else {
        rval <- alpha * AIC(object) + 
            (1 - alpha) * BIC(object)
    }
    rval
}
