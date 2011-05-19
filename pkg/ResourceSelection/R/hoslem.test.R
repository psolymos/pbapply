hoslem.test <-
function(x, y, g=10) {
    DNAME <- paste(deparse(substitute(x)), deparse(substitute(y)), sep=", ")
    METHOD <- "Hosmer and Lemeshow goodness of fit (GOF) test"
    yhat <- y
    y <- x
    cutyhat <- cut(yhat,
        breaks = quantile(yhat, probs=seq(0,
        1, 1/g)), include.lowest = TRUE)
    observed <- xtabs(cbind("y0"=1 - y, "y1"=y) ~ cutyhat)
    expected <- xtabs(cbind("yhat0"=1 - yhat, "yhat1"=yhat) ~ cutyhat)
    chisq <- sum((observed - expected)^2 / expected)
    PVAL = 1 - pchisq(chisq, g - 2)
    PARAMETER <- g - 2
    names(chisq) <- "X-squared"
    names(PARAMETER) <- "df"
    structure(list(statistic = chisq, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME, observed = observed, 
        expected = expected), class = "htest")
}

