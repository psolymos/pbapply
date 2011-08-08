confint.svocc <-
function (object, parm, level = 0.95, model = c("full", "sta", "det"), type, ...)
{
    model <- match.arg(model)
    boot <- attr(object, "bootstrap")
    if (missing(type)) {
        type <- "mle"
        if (object$penalized && !is.null(boot))
            type <- "boot"
    }
    type <- match.arg(type, c("mle", "pmle", "boot"))
    if (type == "mle" && object$penalized)
        stop("refit model with 'penalized=FALSE' to get MLE")
    if (type == "boot" && is.null(boot))
        stop("no bootstrap results found")
    if (type != "boot")
        boot <- NULL
    cf <- coef(object, model)
    pnames <- names(cf)
    if (missing(parm)) {
        parm <- pnames
    } else {
        if (is.numeric(parm))
            parm <- pnames[parm]
    }
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%", sep="")
    ci <- array(NA, dim = c(length(parm), 2), dimnames = list(parm, pct))
    if (type == "mle") {
#        fac <- qt(a, object$df.residual)
        fac <- qnorm(a)
        ses <- sqrt(diag(vcov(object, model, type)))[parm]
        ci[] <- cf[parm] + ses %o% fac
    } else {
        ip <- switch(model,
            "full" = 1:length(coef(object, "full")),
            "sta" = 1:length(coef(object, "sta")),
            "det" = (length(coef(object, "sta")) + 1):length(coef(object, "full")))
        cii <- t(apply(data.matrix(boot[ip, ]), 1, quantile, probs=a))
        rownames(cii) <- pnames
        ci[] <- cii[parm,]
    }
    return(ci)
}

