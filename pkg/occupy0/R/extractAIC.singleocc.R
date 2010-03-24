`extractAIC.singleocc` <-
function (fit, scale = NULL, k = 2, ...)
{
    return(c(length(coef(fit, "full")), AIC(fit, k = k)))
}

