library(stats4)
require(dclone)
## virtual classes for S3 objects
setClass("custommodel", representation("VIRTUAL"))
setClass("mcmc", representation("VIRTUAL"))
setClass("mcmc.list", representation("VIRTUAL"))
setClass("mcmc.list.dc", representation("VIRTUAL"))
setClass("dctable", representation("VIRTUAL"))
setClass("dcdiag", representation("VIRTUAL"))

setClassUnion("nClones", c("NULL", "numeric"))
setClassUnion("dcDiag", c("NULL", "dcdiag"))
setClassUnion("dcTable", c("NULL", "dctable"))
setClassUnion("MCMClist", c("mcmc", "mcmc.list", "mcmc.list.dc"))
setClassUnion("dcArgs", c("NULL", "character"))
setClassUnion("dcFunction", c("NULL", "function"))
setClassUnion("dcInits", c("NULL", "list", "function"))
setClassUnion("dcModel", c("function", "character", "custommodel"))

setClass("dcBugs", 
    representation(
        data = "list",
        inits = "dcInits",
        model = "dcModel",
        params = "dcArgs"),
    prototype = list(
        data = list(),
        inits = NULL,
        model = character(0)))
setClass("dcFit",
    representation(
        multiply = "dcArgs",
        unchanged = "dcArgs",
        update = "dcArgs",
        updatefun = "dcFunction",
        initsfun = "dcFunction",
        flavour = "character"),
    contains = "dcBugs",
    prototype = list(
        params = NULL,
        multiply = NULL,
        unchanged = NULL,
        update = NULL,
        updatefun = NULL,
        initsfun = NULL,
        flavour = "jags"))
setClass("dcMle", 
    representation(
        mcmc = "MCMClist",
        summary = "matrix",
        dctable = "dcTable",
        dcdiag = "dcDiag",
        start = "numeric",
        end = "numeric",
        thin = "numeric",
        n.chains = "numeric",
        n.clones = "nClones"),
    prototype = list(
        mcmc = as.mcmc(matrix(0,0,0)),
        summary = matrix(0,0,0),
        diag = NULL,
        start = numeric(0),
        end = numeric(0),
        thin = numeric(0),
        n.chains = numeric(0),
        n.clones = NULL))
setAs(from = "dcBugs", to = "dcFit", def = function(from) {
    out <- new("dcFit")
    out@data <- from@data
    out@model <- from@model
    out@inits <- from@inits
    out@params <- from@params
    out
})
setAs(from = "MCMClist", to = "dcMle", def = function(from) {
    rval <- new("dcMle")
    if (!is.null(nclones(from))) {
        coefs <- coef(from)
        se <- dcsd(from)
        zstat <- coefs/se
        pval <- 2 * pnorm(-abs(zstat))
        coefs <- cbind(coefs, se, zstat, pval)
        colnames(coefs) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
        rval@summary <- coefs
    }
    dcd <- try(dcdiag(from), silent=TRUE)
    if (inherits(dcd, "try-error")) {
        dcd <- NULL
    } else {
        rownames(dcd) <- NULL
    }
    dct <- try(dctable(from), silent=TRUE)
    if (inherits(dct, "try-error")) {
        dct <- NULL
    } else {
        dct <- lapply(dct, function(z) {
            rownames(z) <- NULL
            z
        })
        class(dct) <- "dctable"
    }
    rval@mcmc <- from
    attr(rval@mcmc, "dcdiag") <- NULL
    attr(rval@mcmc, "dctable") <- NULL
    rval@dcdiag <- dcd
    rval@dctable <- dct
    rval@start <- start(from)
    rval@end <- end(from)
    rval@thin <- thin(from)
    rval@n.chains <- length(from)
    rval@n.clones <- nclones(from)
    rval
})
setAs(from = "dcMle", to = "MCMClist", def = function(from) {
    out <- from@mcmc
    attr(out, "dcdiag") <- from@dcdiag
    attr(out, "dctable") <- from@dctable
    out
})
dcmle <- function(x, params, n.clones=1, cl=NULL, ...) {
    x <- as(x, "dcFit")
    if (missing(params))
        params <- x@params
    if (length(n.clones) == 1) {
        inits <- x@inits
        if (n.clones > 1 && !is.null(x@initsfun)) {
            initsfun <- match.fun(x@initsfun)
            ARGS <- names(as.list(args(initsfun)))
            ARGS <- ARGS[1:max(2, length(ARGS)-1)]
            if (length(ARGS)==2)
                eval(parse(text = paste("inits <- ", 
                    deparse(substitute(initsfun)), "(", 
                    ARGS[2], "=n.clones)", sep = "")))
        }
        dat <- dclone(x@data, n.clones, x@multiply, x@unchanged)
        if (x@flavour == "jags" && is.null(cl))
            out <- jags.fit(dat, params, x@model, inits, ...)
        if (x@flavour == "bugs" && is.null(cl))
            out <- bugs.fit(dat, params, x@model, inits, ...)
        if (x@flavour == "jags" && !is.null(cl))
            out <- jags.parfit(cl, dat, params, x@model, inits, ...)
        if (x@flavour == "bugs" && !is.null(cl))
            stop("parallel chains with flavour='bugs' not supported")
    } else {
        if (is.null(cl)) {
            out <- dc.fit(x@data, params, x@model, x@inits, 
                n.clones = n.clones, 
                multiply = x@multiply, 
                unchanged = x@unchanged,
                update = x@update,
                updatefun = x@updatefun,
                initsfun = x@initsfun,
                flavour = x@flavour, ...)
        } else {
            out <- dc.parfit(cl, x@data, params, x@model, x@inits, 
                n.clones = n.clones, 
                multiply = x@multiply, 
                unchanged = x@unchanged,
                update = x@update,
                updatefun = x@updatefun,
                initsfun = x@initsfun,
                flavour = x@flavour, ...)
        }
    }
    as(out, "dcMle")
}
setMethod("show", "dcMle", function(object) {
    k <- object@n.clones
    if (is.null(k)) {
        print(summary(object@mcmc))
    } else {
        attributes(k) <- NULL
        n <- data.frame(start=object@start, end=object@end, thin=object@thin,
            n.iter=object@end-object@start+1,
            n.chains=object@n.chains, n.clones=k)
        digits <- max(3, getOption("digits") - 3)
        cat("Object of class \"", class(object)[1L], "\"\n\n", sep="")
        print(n, digits=digits, row.names=FALSE)
        cat("\n")
        printCoefmat(object@summary, digits = digits, signif.legend = TRUE)
        cat("\n")
        print(object@dcdiag, digits=digits, row.names=FALSE)
        cat("\n")
    }
    invisible(object)
})
setMethod("quantile", "dcMle", function(x, ...) quantile(x@mcmc, ...))
setMethod("dcdiag", "dcMle", function(x, ...) x@dcdiag)
setMethod("dctable", "dcMle", function(x, ...) x@dctable)
#setMethod("dcdiag", "dcMle", function(x, ...) {
#    assign(deparse(substitute(x)), x@mcmc)
#    dcdiag(x, ...)
#})
setMethod("dcsd", "dcMle", function(object, ...) dcsd(object@mcmc, ...))
setMethod("coef", "dcMle", function(object, ...) coef(object@mcmc, ...))
setMethod("vcov", "dcMle", function(object, ...) vcov(object@mcmc, ...))
setMethod("confint", "dcMle", function(object, parm, level = 0.95, ...) {
    if (!inherits(object@mcmc, "mcmc.list.dc"))
        stop("'confint' method not defined for k=1")
    confint(object@mcmc, parm, level, ...)
})
setMethod("nclones", "dcMle", function(x, ...) x@n.clones)


rats0 <- list(
    data = list(N = 30, T = 5, 
        Y = structure(c(151, 145, 147, 155, 135, 159, 141, 159, 177, 134, 
        160, 143, 154, 171, 163, 160, 142, 156, 157, 152, 154, 139, 146, 
        157, 132, 160, 169, 157, 137, 153, 199, 199, 214, 200, 188, 210, 
        189, 201, 236, 182, 208, 188, 200, 221, 216, 207, 187, 203, 212, 
        203, 205, 190, 191, 211, 185, 207, 216, 205, 180, 200, 246, 249, 
        263, 237, 230, 252, 231, 248, 285, 220, 261, 220, 244, 270, 242, 
        248, 234, 243, 259, 246, 253, 225, 229, 250, 237, 257, 261, 248, 
        219, 244, 283, 293, 312, 272, 280, 298, 275, 297, 350, 260, 313, 
        273, 289, 326, 281, 288, 280, 283, 307, 286, 298, 267, 272, 285, 
        286, 303, 295, 289, 258, 286, 320, 354, 328, 297, 323, 331, 305, 
        338, 376, 296, 352, 314, 325, 358, 312, 324, 316, 317, 336, 321, 
        334, 302, 302, 323, 331, 345, 333, 316, 291, 324), .Dim = c(30, 5)),
        x = c(8.0, 15.0, 22.0, 29.0, 36.0)),
    inits = list(alpha = c(250, 250, 250, 250, 250, 250, 250, 250, 250,
        250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250,
        250, 250, 250, 250, 250, 250, 250, 250),
        beta = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6),
        alpha.c = 150, beta.c = 10, tau.c = 1, tau.alpha = 1, tau.beta = 1),
    model = function() {
        for (i in 1:N) {
           for (j in 1:T) {
              mu[i,j] <- alpha[i] + beta[i]*(x[j] - x.bar)
              Y[i,j]   ~ dnorm(mu[i,j],tau.c)
           }
           alpha[i] ~ dnorm(alpha.c,tau.alpha)
           beta[i]  ~ dnorm(beta.c,tau.beta)
        }
        alpha.c   ~ dnorm(0,1.0E-4)
        beta.c    ~ dnorm(0,1.0E-4)
        tau.c     ~ dgamma(1.0E-3,1.0E-3)
        tau.alpha ~ dgamma(1.0E-3,1.0E-3)
        tau.beta  ~ dgamma(1.0E-3,1.0E-3)
        sigma.c     <- 1.0/sqrt(tau.c)
        sigma.alpha <- 1.0/sqrt(tau.alpha)
        sigma.beta  <- 1.0/sqrt(tau.beta)
        x.bar    <- mean(x[])
        alpha0   <- alpha.c - beta.c*x.bar
    })
rownames(rats0$data$Y) <- paste("rat", 1:30, sep=".")
colnames(rats0$data$Y) <- paste("week", 1:5, sep=".")
names(rats0$data$x) <- paste("week", 1:5, sep=".")
#m <- jags.fit(rats$data, c("alpha0", "beta.c"), rats$model, rats$inits)
#mm <- dc.fit(rats$data, c("alpha0", "beta.c"), rats$model, NULL,
#    n.clones = 1:5, multiply = "N", unchanged = c("T", "x"))

rats <- new("dcFit")
rats@data <- rats0$data
rats@inits <- rats0$inits
rats@model <- custommodel(rats0$model)

rats@params <- c("alpha0", "beta.c", "sigma.c", "sigma.alpha", "sigma.beta")
rats@multiply <- "N"
rats@unchanged <- c("T", "x")
rats@initsfun <- function(model, n.clones) {
    dclone(rats@inits, n.clones,
        unchanged=c("alpha0", "beta.c", "tau.c", "tau.alpha", "tau.beta"))
}




rats@initsfun <- NULL
rats@inits <- NULL
res <- dcmle(rats, n.clones=1:3, n.adapt=0, n.update=0, n.iter=100)
res@diag
res@n.clones
#res@k
res
## this works, but not k>1
dcModel(as(rats, "dcBugs"), n.adapt=0, n.update=0, n.iter=100)

dcmle(rats, n.clones=1, n.iter=100)
dcmle(rats, n.clones=2, n.iter=100)
dcmle(rats, n.clones=1:3, n.iter=100)

cl <- makeSOCKcluster(3)
dcmle(rats, n.clones=2, n.iter=1000, cl=cl)
dcmle(rats, n.clones=1:3, n.iter=1000, cl=cl)
dcmle(rats, n.clones=1:3, n.iter=1000, cl=cl, partype="parchains")
dcmle(rats, n.clones=1:3, n.iter=1000, cl=cl, partype="both")
stopCluster(cl)

params <- x@params
dat <- dclone(x@data, n.clones, x@dclone$multiply, x@dclone$unchanged)
m <- jags.fit(dat, params, x@model)
mm <- dc.fit(rats$data, c("alpha0", "beta.c"), rats$model, NULL,
    n.clones = 1:5, multiply = "N", unchanged = c("T", "x"))

## ovenbird example
## data
data(ovenbird)
Y <- ovenbird$count
X <- model.matrix(~uplow + thd, ovenbird)
## model
glmm.model.up <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta)
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(priors[j,1], priors[j,2])
    }
    sigma ~ dgamma(priors[(np+1),2], priors[(np+1),1])
}
## function for updating, x is an MCMC object
upfun <- function(x) {
    if (missing(x)) {
        p <- ncol(X)
        return(cbind(c(rep(0, p), 0.001), rep(0.001, p+1)))
    } else {
        par <- coef(x)
        return(cbind(par, rep(0.01, length(par))))
    }
}
updat <- list(n = length(Y), Y = Y, X = X, np = NCOL(X), priors = upfun())
## inits with latent variable and parameters
ini <- list(alpha=rep(0,length(Y)), beta=rep(0, NCOL(X)))
## function to update inits
ifun <- function(model, n.clones) {
    list(alpha=dclone(rep(0,jdata$n), n.clones),
        beta=coef(model)[-length(coef(model))])
}
dcmod <- dc.fit(updat, c("beta", "sigma"), glmm.model.up, ini,
    n.clones = 1:2, multiply = "n", unchanged = "np",
    update = "priors", updatefun = upfun, initsfun=ifun)

oven <- new("dcExample")
oven@data@data <- updat
oven@data@inits <- ini
oven@data@model <- custommodel(glmm.model.up)

oven@fit@params <- c("beta", "sigma")
oven@fit@multiply <- "n"
oven@fit@unchanged <- "np"
oven@fit@initsfun <- ifun
oven@fit@update <- "priors"
oven@fit@updatefun <- upfun

dcModel(oven, n.clones=1:3, n.iter=2000)




## Seeds example from BUGS Vol. I.
seeds <- list(
    data = list(N = 21,
        r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 
            3, 22, 15, 32, 3),
        n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 
            4, 12, 41, 30, 51, 7),
        x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        x2 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1)),
    inits = list(tau = 1, alpha0 = 0, alpha1 = 0, alpha2 = 0, alpha12 = 0),
    model = function() {
        alpha0  ~ dnorm(0.0,1.0E-6);     # intercept
        alpha1  ~ dnorm(0.0,1.0E-6);     # seed coeff
        alpha2  ~ dnorm(0.0,1.0E-6);     # extract coeff
        alpha12 ~ dnorm(0.0,1.0E-6);     # intercept
        tau     ~ dgamma(1.0E-3,1.0E-3); # 1/sigma^2
        sigma  <- 1.0/sqrt(tau);
        for (i in 1:N) {
           b[i]         ~ dnorm(0.0,tau);
           logit(p[i]) <- alpha0 + alpha1*x1[i] + alpha2*x2[i] +
                          alpha12*x1[i]*x2[i] + b[i];
           r[i]         ~ dbin(p[i],n[i]);
        }
    })
#m <- jags.fit(seeds$dat, c("alpha0", "alpha1","alpha2","alpha12","sigma"), seeds$model, seeds$inits)
#mm <- dc.fit(seeds$dat, c("alpha0", "alpha1","alpha2","alpha12","sigma"), seeds$model, seeds$inits,
#        n.clones = 1:5, multiply = "N")

x <- new("dcexamples")
x@name <- ""
x@data <- rats$data
x@inits <- rats$inits
x@model <- custommodel(rats$model)
x@dclone <- list(multiply = "N", unchanged = c("T", "x"))
x@params <- c("alpha0", "beta.c")

dcexample <- 
function(topic, n.clones=NULL, ...) 
{
#    data(topic)
#    x <- get(topic)
    cat("dcexample: \"", deparse(substitute(topic)), "\"\n\n")
    cat("..@data\n")
    print(str(topic@data))
    cat("..@model\n")
    print(custommodel(topic@model))
    if (!is.null(n.clones))
        dcmle(topic, n.clones, ...) else invisible(NULL)
}
dcexample(topic)

## time series example
## data and model taken from Ponciano et al. 2009
## Ecology 90, 356-362.
paurelia <- c(17,29,39,63,185,258,267,392,510,570,650,560,575,650,550,480,520,500)
paramecium <- new("dcFit")
paramecium@data <- list(ncl=1, n=length(paurelia), Y=dcdim(data.matrix(paurelia)))
paramecium@model <- custommodel(beverton.holt <- function() {
    for (k in 1:ncl) {
        for(i in 2:(n+1)){
            Y[(i-1), k] ~ dpois(exp(X[i, k])) # observations
            X[i, k] ~ dnorm(mu[i, k], 1 / sigma^2) # state
            mu[i, k] <- X[(i-1), k] + log(lambda) - log(1 + beta * exp(X[(i-1), k]))
        }
        X[1, k] ~ dnorm(mu0, 1 / sigma^2) # state at t0
    }
    beta ~ dlnorm(-1, 1) # Priors on model parameters
    sigma ~ dlnorm(0, 1)
    tmp ~ dlnorm(0, 1)
    lambda <- tmp + 1
    mu0 <- log(2)  + log(lambda) - log(1 + beta * 2)
})
paramecium@multiply <- "ncl"
paramecium@unchanged <- "n"
paramecium@params <- c("lambda","beta","sigma")
m <- dcmle(paramecium, n.clones=2, n.iter=1000)

dcexample <- 
function(topic, n.clones=NULL, ...) 
{
#    data(topic, package="dcexamples")
    x <- get(deparse(substitute(topic)))
    cat("dcexample: ", deparse(substitute(topic)), "\n\n")
    cat("..@data\n")
    print(str(topic@data))
    cat("..@model\n")
    print(custommodel(topic@model))
    if (!is.null(n.clones))
        dcmle(topic, n.clones=n.clones, ...) 
    else invisible(NULL)
}
system.time(m1 <- dcmle(paramecium, n.clones=1, n.iter=1000))
system.time(m2 <- dcmle(paramecium, n.clones=2, n.iter=1000))
system.time(m2 <- dcmle(paramecium, n.clones=1:3, n.iter=1000))
cl <- makeSOCKcluster(3)
system.time(m2 <- dcmle(paramecium, n.clones=2, n.iter=1000, cl=cl))
system.time(m2 <- dcmle(paramecium, n.clones=1:3, n.iter=1000, cl=cl))
system.time(m2 <- dcmle(paramecium, n.clones=1:3, n.iter=1000, cl=cl, partype="parchains"))
system.time(m2 <- dcmle(paramecium, n.clones=1:3, n.iter=1000, cl=cl, partype="both"))
stopCluster(cl)
