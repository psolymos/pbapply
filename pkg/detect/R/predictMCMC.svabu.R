predictMCMC.svabu <-
function(object, se.fit = FALSE, n.iter = 1000, 
raw = FALSE, vcov.type, ...)
{
    require(dclone)
    predfun <- c("model {", 
        "    for (i in 1:n) {", 
        "        N[i] ~ dpois(lambda[i])", 
        "        Y[i] ~ dbin(p[i], N[i])", 
        "    }",
        "    tmp[1:(np1+np2)] ~ dmnorm(param[], prec[,])", 
        "    beta[1:np1] <- tmp[1:np1]", 
        "    theta[(np1+1):(np1+np2)] <- tmp[(np1+1):(np1+np2)]", 
        "}")
    class(predfun) <- "custommodel"
    ## only sta and det not zif: if present only
    nps <- sapply(object$coefficients, length)
    np <- sum(nps[1:2])
    vcv <- vcov(object, model="full", vcov.type)[1:np, 1:np]
    prec <- make.symmetric(solve(vcv))
    param <- coef(object)[1:np]
    inits <- list(N = object$y + 1)
    prdat <- list(Y=object$y, n=length(object$y),
        lambda=fitted(object), p=object$detection.probabilities,
        np1=nps[1], np2=nps[2],
        param = param, prec = prec)
    prval <- jags.fit(prdat, "N", predfun, inits, 
        n.chains=1, n.iter=n.iter, ...)
    rval <- if (se.fit) {
        list(fit = coef(prval), se.fit = dcsd(prval))
    } else coef(prval)
    if (raw)
        return(prval) else return(rval)
}
