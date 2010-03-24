confint.singleocc <-
function (object, parm, level = 0.95, model = c("full", "occ", "det"), ...)
{
    model <- match.arg(model)
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
    fac <- qt(a, object$df.residual)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    ci <- array(NA, dim = c(length(parm), 2), dimnames = list(parm, pct))
    ses <- sqrt(diag(vcov(object, model)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    return(ci)
}
