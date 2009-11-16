confint.mcmc.list.dc <-
function(object, parm, level = 0.95, ...)
{
    ## retrieve posterior means
    cf <- coef(object)
    ## handling names and subsetting according to parm
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    ## confidence levels based on level arg
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    ## Normal quantiles
    fac <- qnorm(a)
    ## scientific formatting
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    ## empty array to fill up with values
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
    ## DC SD calculation
    ses <- dcsd(object)[parm]
    ## calculates actual CIs
    ci[] <- cf[parm] + ses %o% fac
    ci
}
