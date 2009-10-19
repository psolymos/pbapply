confint.mcmc.list.dc <-
function(object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qnorm(a)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
    ses <- dcsd(object)[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}
