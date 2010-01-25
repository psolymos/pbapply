confint.mcmc.list.dc <-
function(object, parm, level = 0.95, type = c("wald", "quantile"), ...)
{
    type <- match.arg(type)
    ## handling names and subsetting according to parm
    pnames <- varnames(object)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    ## confidence levels based on level arg
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    ## scientific formatting
    np <- length(parm)
    pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
    ## empty array to fill up with values
    ci <- array(NA, dim = c(np, 2L), dimnames = list(parm, pct))
    if (type == "wald") {
        ## retrieve posterior means
        cf <- coef(object)
        ## Normal quantiles
        fac <- qnorm(a)
        ## DC SD calculation
        ses <- dcsd(object)[parm]
        ## calculates actual CIs
        ci[] <- cf[parm] + ses %o% fac
    } else {
        ## quantiles
        ci[] <- t(mcmcapply(object[,parm], quantile, probs=a))
    }
    ci
}
