## BAM short course

glm.pois <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(mu, prec)
    }
}
Y <- 4
X <- matrix(1, 1, 1)
vals <- expand.grid(mu=c(-5, 0, 5), prec=c(0.001, 1))
d <- apply(vals, 1, function(z) list(n = length(Y), Y = Y, X = X, np = ncol(X), mu=z[1], prec=z[2]))
m <- lapply(d, function(z) jags.fit(z, "beta", glm.pois, n.iter = 1000))
res <- data.frame(vals, est=sapply(m, coef), std.err=sapply(m, dcsd))
res$diff <- res$est - coef(glm(Y ~ 1, family=poisson))
res
glm(Y ~ 1, family=poisson)

n <- 20
set.seed(1234)
Y <- rpois(n, 4)
X <- matrix(1, n, 1)
vals <- expand.grid(mu=c(-5, 0, 5), prec=c(0.001, 1))
d <- apply(vals, 1, function(z) list(n = length(Y), Y = Y, X = X, np = ncol(X), mu=z[1], prec=z[2]))
m <- lapply(d, function(z) jags.fit(z, "beta", glm.pois, n.iter = 1000))
res <- data.frame(vals, est=sapply(m, coef), std.err=sapply(m, dcsd))
res$diff <- res$est - coef(glm(Y ~ 1, family=poisson))
res
glm(Y ~ 1, family=poisson)


fun1 <- function(y) sum(dpois(Y, exp(y), log=TRUE))
parm <- seq(0, 2, len=101)
llik <- sapply(parm, fun1)
par(mfrow=c(1,2))
plot(parm, llik, type="l", col=4, ylab="log Likelihood", xlab="Estimate")
abline(v=coef(glm(Y ~ 1, family=poisson)), col=2)
plot(parm, exp(llik), type="l", col=4, ylab="Likelihood", xlab="Estimate")
abline(v=coef(glm(Y ~ 1, family=poisson)), col=2)

## next: add a continuous covariate, llik surface using outer

x <- rnorm(n)
X <- model.matrix(~x)
Y <- rpois(n, exp(X %*% c(1, -0.5)))
glm(Y ~ x, family=poisson)
vals <- expand.grid(b0=seq(0,2,len=101), b1=seq(-1,1,len=101))
fun2 <- function(z) sum(dpois(Y, exp(X %*% z), log=TRUE))
tp <- apply(vals, 1, fun2)
L <- matrix(exp(tp), 101, 101)
image(seq(0,2,len=101), seq(-1,1,len=101), max(L)-L)
abline(v=coef(glm(Y ~ x, family=poisson))[1], h=coef(glm(Y ~ x, family=poisson))[2], col=4)



- Installing necessary software
- WinBUGS basics
- R and WinBUGS, JAGS
- basic GLM and GLMM examples
- MCMC convergence disgnostics
- data cloning (DC) basics (getting MLE out of BUGS)
- DC convergence diagnostics
- speeding up computations by parallel computing (snow package, parallel MCMC)

1) Motivation

a) GLM
Y ~ Poisson(lambda)
log(lambda) = beta_0
calculate the likelihood function
determine its maximum: MLE
(? score function, curvature)
glm(y ~ 1)

b) covariates
log(lambda) = beta_0 + beta_1*x = X %*% beta
understand design matrix
determine likelihood surface for 2 pars
glm(y ~ x)

c) WinBUGS
write GLM model
use low n to show effect of priors
increase n to how MLE is reached asymptotically
refresh memories from Banff
do not want to click, use bugs() etc

d) GLMM (Poisson-lognormal model)
no. of unknown increases with n
adequate sample size is meant by information and not by df
troubles with high dim integral

e) Bayesian model
use R and JAGS for GLMM model
MCMC diagnostics
Bayesian idea works, but there is the prior
Prior is bad because: inference, prediction
cannot use Jeffreys priors
Bayesian learning: empirical Bayes, updating prior info

e) DC
DC idea of repeated experiments
DC asymptotics
... follow dclone paper (JAGS, diagnostics)

f) computation demands - go parallel



## area-duration

## negbin version
library(MASS)
shape <- 2
beta <- c(1.2, -0.5)
phi <- 0.3
n <- 200
x <- rnorm(n)
X <- model.matrix(~x)
A <- rep(1:5, each=20)
T <- rep(1:5, 20)
D <- exp(X %*% beta)
p <- 1 - exp(-phi * T)
mu <- D * A * p
lambda <- rgamma(n, shape=shape, scale=mu/shape)
Y <- rpois(n, lambda)
glmnb.fitter <- function(z) {
    z <- max(.Machine$double.eps, z)
    off <- log(A) + log(1 - exp(-z * T))
    d <- data.frame(y=Y, x=x, off=off)
    fit <- glm.nb(y ~ x + offset(off), data=d)
    -logLik(fit)
}
res <- suppressWarnings(optim(1, glmnb.fitter, method="Nelder-Mead", lower=.Machine$double.eps, hessian=TRUE))
phi.hat <- res$par
phi.se <- 1 / res$hessian

res <- sapply((1:100)/100, glmnb.fitter)
plot((1:100)/100, res)
abline(v=phi, col=2)
abline(v=phi.hat, col=4)



beta <- c(1.2, -0.5)
phi <- 0.3
n <- 200
x <- rnorm(n)
X <- model.matrix(~x)
A <- rep(1:5, each=20)
T <- rep(1:5, 20)
D <- exp(X %*% beta)
p <- 1 - exp(-phi * T)
lambda <- D * A * p
Y <- rpois(n, lambda)

glm.fitter <- function(z) {
    z <- max(.Machine$double.eps, z)
    off <- log(A) + log(1 - exp(-z * T))
    fit <- glm.fit(X, Y, family=poisson(), offset=off)
    class(fit) <- c("glm", "lm")
    -logLik(fit)
}
res <- suppressWarnings(optim(1, glm.fitter, method="Nelder-Mead", lower=.Machine$double.eps, hessian=TRUE))
phi.hat <- res$par
phi.se <- 1 / res$hessian

res <- sapply((1:100)/100, glm.fitter)
plot((1:100)/100, res)
abline(v=phi, col=2)
abline(v=phi.hat, col=4)

glm.at <- function(formula, data, area, duration, 
init.phi=1, method="Nelder-Mead", control=list(),
model = TRUE, x = FALSE, ...) {
    ## parsing formula
    if (missing(data))
        data <- parent.frame()
    mf <- match.call(expand.dots = FALSE)
    mm <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, mm)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    Y <- model.response(mf, "numeric")
    ff <- formula
    ff[[2]] <- NULL
    mt <- terms(ff, data = data)
    X <- model.matrix(mt, mf)
    Xlevels <- .getXlevels(mt, mf)
    ## check variables
    if (length(Y) < 1) 
        stop("empty model")
    if (!isTRUE(all.equal(as.vector(Y), as.integer(round(Y + 
        0.001))))) 
        stop("invalid dependent variable, non-integer values")
    Y <- as.integer(round(Y + 0.001))
    if (any(Y < 0)) 
        stop("invalid dependent variable, negative counts")
    if (length(Y) != NROW(X)) 
        stop("invalid dependent variable, not a vector")
    ## internal fun for optim
    glm.fitter <- function(z) {
        z <- max(.Machine$double.eps, z)
        off <- log(area) + log(1 - exp(-z * duration))
        fit <- glm.fit(X, Y, family=poisson(), offset=off)
        class(fit) <- c("glm", "lm")
        -logLik(fit)
    }
    ## optimizing for phi
    res <- suppressWarnings(optim(init.phi, glm.fitter, 
        method=method, lower=.Machine$double.eps, hessian=TRUE, control=control))
    phi.hat <- res$par
    ## refitting
    off <- log(area) + log(1 - exp(-phi.hat * duration))
    out <- glm.fit(X, Y, family=poisson(), offset=off)
    out$call <- match.call()
    out$phi <- phi.hat
    out$SE.phi <- 1 / res$hessian
    out$df.residual <- out$df.residual - 1
    out$df.null <- out$df.null - 1
    out2 <- list(formula=formula,
        offset=off,
        terms=mt,
        levels=Xlevels,
        contrasts=attr(X, "contrasts"),
        model= if (model) mf else NULL,
        x= if (x) X else NULL)
    out <- c(out, out2)

    class(out) <- c("glmat", "glm", "lm")
    out
}
summary.glmat <- 
function (object, ...) 
{
    summ <- c(summary.glm(object, dispersion = 1, correlation = FALSE), 
        object[c("phi", "SE.phi")])
    class(summ) <- c("summary.glmat", "summary.glm")
    summ
}
print.summary.glmat <- 
function (x, ...) 
{
    NextMethod()
    dp <- max(2 - floor(log10(x$SE.phi)), 0)
    cat("\n                Phi: ", format(round(x$phi, dp), 
        nsmall = dp), "\n          Std. Err.: ", format(round(x$SE.phi, 
        dp), nsmall = dp), "\n")
    invisible(x)
}
m <- glm.at(Y~x, area=A, duration=T)
summary(m)
summary(update(m, Y~1))


## check if fun can be evaluated remotely
library(snow)
cl <- makeSOCKcluster(2)
inits <- function() runif(1)
clusterExport(cl, "inits")
parLapply(cl, 1:length(cl), function(i) try(inits()))
a <- 1
inits <- function() runif(1) * a

clusterExport(cl, "inits")
Try <- parLapply(cl, 1:length(cl), function(i) try(inits()))
if (inherits(Try[[1]], "try-error"))
    stop(paste("  from remote workers\n", Try[[1]], sep="  "))



    if (is.null(getOption("dcoptions")))
        options("dcoptions"=list("rhat"=1.1,
            "autoburnin"=TRUE,
            "diag"=0.05,
            "verbose"=1,
            "LB"=FALSE,
            "RNG"="RNGstream"))

dcoptions <-
function(...)
{
    opar <- getOption("dcoptions")
    args <- list(...)
    if (length(args)) {
        if (length(args)==1 && is.list(args[[1]])) {
            npar <- args[[1]]
        } else {
            npar <- opar
            npar[match(names(args), names(npar))] <- args
        }
        options("pboptions"=npar)
    }
    invisible(opar)
}


## progress bar for packages
## make it according to sapply and lapply, 
#http://ryouready.wordpress.com/2010/01/11/progress-bars-in-r-part-ii-a-wrapper-for-apply-functions/

options("pbapply.pb"="txt")
options("pbapply.gui"=list(title="R progress bar",
    label="", width=300, initial=0))
options("pbapply.text"=list(char="+", width=50, style=3, initial=0))

pblapply <-
function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    progress.bar <- getOption("pbapply.pb")
    if (!is.null(progress.bar)) {
        progress.bar <- match.arg(progress.bar, c("txt", "win", "tk", "none"))
        if (progress.bar == "none") 
            progress.bar <- NULL
    }
    B <- length(X)
    do.pd <- interactive() && !is.null(progress.bar) && B >= 1
    if (!do.pd)
        return(.Internal(lapply(X, FUN)))
    control <- switch(progress.bar,
        text = getOption("pbapply.text"),
        win = getOption("pbapply.gui"),
        tk = getOption("pbapply.gui"))
    pb <- switch(progress.bar, 
        text = txtProgressBar(0, B, initial=control$initial,
            style = control$style, width = control$width, char = control$char),
        win = winProgressBar(min=0, max=B, initial=control$initial,
            title = control$title, label = control$label),
        tk = tkProgressBar(min=0, max=B, initial=control$initial,
            title = control$title, label = control$label))
    rval <- vector("list", length(X))
    for (i in 1:B) {
        rval[[i]] <- FUN(X[[i]], ...)
        switch(progress.bar, 
            text = setTxtProgressBar(pb, i), 
            win = setWinProgressBar(pb, i, label=control$label),
            win = setTkProgressBar(pb, i, label=control$label))
    }
    close(pb)
    rval
}

pbsapply <-
function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    answer <- pblapply(X, FUN, ...)
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (simplify && length(answer) && length(common.len <- unique(unlist(lapply(answer, 
        length)))) == 1L) {
        if (common.len == 1L) 
            unlist(answer, recursive = FALSE)
        else if (common.len > 1L) 
            array(unlist(answer, recursive = FALSE), dim = c(common.len, 
                length(X)), dimnames = if (!(is.null(n1 <- names(answer[[1L]])) & 
                is.null(n2 <- names(answer)))) 
                list(n1, n2))
        else answer
    }
    else answer
}

pbapply <-
function (X, MARGIN, FUN, ...)
{
    if (is.null(dim(X)) || length(dim(X)) != 2)
        stop("X must have 2 dimensions")
    if (MARGIN == 2)
        X <- t(X) 
    Z <- lapply(1:NROW(X), function(i) X[i,])
    names(Z) <- rownames(X)
    FUN <- match.fun(FUN)
    pbsapply(Z, FUN, ...)
}

n <- 200
x <- rnorm(n)
y <- rnorm(n, model.matrix(~x) %*% c(0,1), sd=0.5)
d <- data.frame(y, x)
mod <- lm(y~x, d)
ndat <- model.frame(mod)
B <- 100
bid <- sapply(1:B, function(i) sample(nrow(ndat), nrow(ndat), TRUE))
fun <- function(z) {
    ndat <- ndat[sample(nrow(ndat), nrow(ndat), TRUE),]
    coef(lm(mod$call$formula, data=ndat[z,]))
}

system.time(res1 <- lapply(1:B, function(i) fun(bid[,i])))
system.time(res2 <- sapply(1:B, function(i) fun(bid[,i])))
system.time(res3 <- apply(bid, 2, fun))

system.time(res4 <- pblapply(1:B, function(i) fun(bid[,i])))
system.time(res5 <- pbsapply(1:B, function(i) fun(bid[,i])))
system.time(res6 <- pbapply(bid, 2, fun))

system.time(l_ply(1:1000, identity, .progress = "none"))
system.time(l_ply(1:1000, identity, .progress = "tk"))
system.time(l_ply(1:1000, identity, .progress = "text"))
system.time(l_ply(1:1000, identity, .progress = progress_text(char = "-")))

system.time(res2 <- pblapply(1:1000, identity))
system.time(res4 <- pbsapply(1:1000, identity))
system.time(res6 <- pbapply(bid, 2, fun))



lapply_pb <- function(X, FUN, ...)
{
 env <- environment()
 pb_Total <- length(X)
 counter <- 0
 pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)   

 # wrapper around FUN
 wrapper <- function(...){
   curVal <- get("counter", envir = env)
   assign("counter", curVal +1 ,envir=env)
   setTxtProgressBar(get("pb", envir=env), curVal +1)
   FUN(...)
 }
 lapply(X, wrapper, ...)
 close(pb)
}


l <- sapply(1:10000, function(x) rnorm(1000))
system.time(tmp <- lapply(l, mean))
system.time(tmp <- pblapply(l, mean))
system.time(tmp <- lapply_pb(l, mean))

system.time(tmp <- sapply(l, mean))
system.time(tmp <- pbsapply(l, mean))

system.time(tmp <- apply(l, 1, mean))
system.time(tmp <- pbapply(l, 1, mean))


## seed in WinBUGS/OpenBUGS
set.seed(1234)
n <- 50
beta <- c(1.8, -0.9)
sigma <- 0.2
x <- runif(n, min = 0, max = 1)
X <- model.matrix(~ x)
alpha <- rnorm(n, mean = 0, sd = sigma)
lambda <- exp(alpha + drop(X %*% beta))
Y <- rpois(n, lambda)

glmm.model.bugs <- function() {
   for (i in 1:n) {
      Y[i] ~ dpois(lambda[i])
      lambda[i] <- exp(alpha[i] +
         inprod(X[i,], beta[1,]))
      alpha[i] ~ dnorm(0, tau) %_% I(-5, 5)
   }
   for (j in 1:np) {
      beta[1,j] ~ dnorm(0, 0.01) %_% I(-5, 5)
   }
   log.sigma ~ dnorm(0, 0.01) %_% I(-5, 5)
   sigma <- exp(log.sigma)
   tau <- 1 / pow(sigma, 2)
}
dat <- list(Y = Y, X = X, n = n,
   np = ncol(X))

cl <- makeSOCKcluster(3)
mod <- bugs.parfit(cl, dat, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1, program="openbugs")
mod[1:2,]
mod2 <- bugs.parfit(cl, dat, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1, program="winbugs")
mod2[1:2,]

mod.wb1 <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1, bugs.seed=1234)
mod.ob1 <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, program = "openbugs", 
   DIC = FALSE, n.thin = 1, seed=1234)
mod.wb2 <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1, bugs.seed=1234)
mod.ob2 <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, program = "openbugs", 
   DIC = FALSE, n.thin = 1, seed=1234)
mod.wb3 <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1, bugs.seed=1234, n.burnin=500)
mod.ob3 <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, program = "openbugs", 
   DIC = FALSE, n.thin = 1, seed=1234, n.burnin=500)
mod.wb4 <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1, bugs.seed=1235, n.burnin=500)
mod.ob4 <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, program = "openbugs", 
   DIC = FALSE, n.thin = 1, seed=1235, n.burnin=500)


mod.wb1[1:2,][[1]]
mod.wb2[1:2,][[1]]
mod.wb3[501:502,][[1]]
mod.wb4[501:502,][[1]]

mod.ob1[1:2,][[1]]
mod.ob2[1:2,][[1]]
mod.ob3[501:502,][[1]]

cl <- makeSOCKcluster(3)

bugs.parfit <-
function(cl, data, params, model, inits=NULL, 
n.chains = 3, seed = 1:n.chains,
program=c("winbugs", "openbugs"), ...) ## only mcmc.list format is supported
{
    if (!inherits(cl, "cluster"))
        stop("'cl' must be a 'cluster' object")
    trace <- getOption("dclone.verbose")
    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
    if (length(unique(seed)) < n.chains)
        stop("'seed' must have 'n.chains' unique values")
    ## not case sensitive evaluation of program arg
    program <- match.arg(tolower(program), c("winbugs", "openbugs"))
    ## retrieves n.clones
    n.clones <- dclone:::nclones.list(data)
    ## removes n.clones attr from each element of data
    data <- lapply(data, function(z) {
        attr(z, "n.clones") <- NULL
        z
    })
    ## using write.model to enable custommodel settings
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        model <- write.jags.model(model)
        on.exit(try(clean.jags.model(model)))
    }
    if (is.null(inits))
        inits <- lapply(1:n.chains, function(i) NULL)
    ## common data to cluster
    cldata <- list(data=data, params=params, model=model, inits=inits, 
        seed=seed, program=program)
    ## parallel function to evaluate by snowWrapper
    bugsparallel <- function(i, ...)   {
        bugs.fit(data=cldata$data, params=cldata$params, 
            model=cldata$model, 
            inits=cldata$inits[[i]], n.chains=1, 
            seed=cldata$seed[i], 
            program=cldata$program, format="mcmc.list", ...)
    }
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    ## parallel computations
    balancing <- if (getOption("dclone.LB"))
        "load" else "none"
    mcmc <- snowWrapper(cl, 1:n.chains, bugsparallel, cldata, lib="dclone", 
        balancing=balancing, size=1, dir=getwd(), ...)
    ## binding the chains
    res <- as.mcmc.list(lapply(mcmc, as.mcmc))

    ## adding n.clones attribute, and class attr if mcmc.list
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        if (format == "mcmc.list")
            class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
mod <- bugs.parfit(cl, dat, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1)



## brackets
brackets <-
function (x, ...) 
    UseMethod("brackets")

brackets.mcmc.list <-
function (x, parm, ...) {
    pnames <- varnames(x)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    brackets.default(coef(x[,parm]), dcsd(x[,parm]), ...)
}

brackets.default <-
function (x, sd, times=3, len=10, lower=-Inf, upper=Inf, ...)
{
    fun <- function(m, s, times, len, lower, upper) {
        z <- c(seq(max(m-times*s, lower), m, len=len+1),
            seq(m, min(m+times*s, upper), len=len+1)[-1])
        attr(z, "sd") <- as.numeric(s)
        z
    }
    np <- length(x)
    sd <- rep(sd, np)[1:np]
    times <- rep(times, np)[1:np]
    lower <- rep(lower, np)[1:np]
    upper <- rep(upper, np)[1:np]
    rval <- lapply(1:np, function(i) {
        fun(x[i], sd[i], times[i], len, lower[i], upper[i])
    })
    if (length(x) > 1) {
        pnames <- names(x)
        if (is.null(pnames))
            pnames <- paste("var", 1:np, sep="")
        rval <- as.data.frame(rval)
        colnames(rval) <- pnames
    } else {
        rval <- rval[[1]]
    }
    class(rval) <- c("brackets", class(rval))
    rval
}

library(dclone)
data(regmod)
brackets(regmod)
brackets(regmod, 1)
brackets(4, 1)
brackets(c(4, 5), c(1,2))
brackets(5, 2, len=5)
brackets(c(5,5), 2, lower=c(-Inf,0))
brackets(c(5,5), Inf, lower=c(0,2), upper=c(20,20))

x <- brackets(rep(5, 7), sd=c(2,2,2,3,3,3,Inf), 
    lower=c(-Inf,0,-Inf,-Inf,0,-Inf,0), 
    upper=c(Inf,Inf,8,Inf,Inf,8,8))
plot(as.numeric(x[11,]), 1:7, xlim=range(x), ylim=c(7,1), axes=FALSE, ann=FALSE, cex=1.5)
axis(1)
tmp <- lapply(1:7, function(i) {
    abline(h=i, col="grey")
    s <- attr(x[,i],"sd")
    lines(c(x[11,i]-s, x[11,i]+s), c(i,i), lwd=2)
    points(x[,i], rep(i, 21), pch="|")
})

## ranking

jfun <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], 1/exp(log.sigma)^2)
        mu[i] <- alpha + beta * (x[i] - x.bar)
    }
    x.bar <- mean(x[])
    alpha ~ dnorm(0.0, 1.0E-4)
    beta ~ dnorm(0.0, 1.0E-4)
    log.sigma ~ dnorm(0.0, 1.0E-4)
}
## data generation
set.seed(1234)
N <- 10
alpha <- 1
beta <- -1
sigma <- 2.5
x <- runif(N)
linpred <- model.matrix(~x) %*% c(alpha, beta)
Y <- rnorm(N, mean = linpred, sd = sigma)
## list of data for the model
jdata <- list(N = N, Y = Y, x = x)
## what to monitor
jpara <- c("alpha", "beta", "log.sigma")
## fit the model with JAGS
regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3)

pfun <- function() {
    for (i in 1:N) {
        Y[i] ~ dnorm(mu[i], 1/exp(log.sigma)^2)
        mu[i] <- alpha + beta * (x[i] - x.bar)
        d[i] <- Y[i]-mu[i]
    }
    x.bar <- mean(x[])
    alpha ~ dnorm(parm[1], prec[1])
    beta ~ dnorm(parm[2], prec[2])
    log.sigma ~ dnorm(parm[3], prec[3])
}
pdata <- list(N = N, x = x, parm=coef(regmod), prec=1/dcsd(regmod))

prmod <- jags.fit(pdata, "d", pfun, n.chains = 1, n.iter=1000)

mcmcrank <-
function (x, decreasing=FALSE, na.last=TRUE, 
ties.method=c("average", "first", "random", "max", "min"))
{
    x <- mcmcapply(x, array)
    n <- nrow(x)
    p <- ncol(x)
    r <- apply(x, 1, rank, na.last = na.last, ties.method = ties.method)
    vp <- 1:p
    if (!decreasing)
        r <- p + 1 - r
    fun <- function(z) {
        sapply(vp, function(i) sum(z == i))
    }
    tmp <- sapply(1:p, function(i) {
        sapply(vp, function(z) sum(r[i,] == z))
    })
    tmp <- tmp/n
    rval <- data.frame(tmp)
    colnames(rval) <- colnames(x)
#    attr(rval, "H") <- -colSums(apply(tmp, 2, function(z) z*log(z)))
    attr(rval, "conc") <- apply(tmp, 2, function(z) {
        (max(z) - 1/p) / (1 - 1/p)
    })
    attr(rval, "max") <- max.col(t(tmp))
    rval
}

x <- mcmcrank(prmod)
x
attr(x, "conc")
attr(x, "max")

library(sardata)
data(sardata01)
d <- sardata01$sar[sardata01$sar$study=="hice2002",]
jdata <- list(N = nrow(d), Y = log(d$S+0.5), x = log(d$A))
regmod <- jags.fit(jdata, jpara, jfun, n.chains = 3)
pdata <- list(N = nrow(d), x = log(d$A), parm=coef(regmod), prec=1/dcsd(regmod))
prmod <- jags.fit(pdata, "d", pfun, n.chains = 1, n.iter=10000)
x <- mcmcrank(prmod)
x
attr(x, "conc")
attr(x, "max")
xx <- x[,order(attr(x, "max"))]
par(mfrow=c(4,5))
for (i in 1:20) plot(1:nrow(x), xx[,i], type="h")



`oecosimu2` <-
    function(comm, nestfun, method, nsimul=99,
             burnin=0, thin=1, statistic = "statistic",
             alternative = c("two.sided", "less", "greater"),
             ...)
{
    alternative <- match.arg(alternative)
    nestfun <- match.fun(nestfun)
    if (!is.function(method)) {
        method <- match.arg(method, c("r00", "r0", "r1", "r2", "c0",
                                  "swap", "tswap", "backtrack", "quasiswap",
                                  "r2dtable"))
        if (method == "r2dtable") {
            nr <- rowSums(comm)
            nc <- colSums(comm)
            permfun <- function(z) r2dtable(1, nr, nc)[[1]]
        }
    } else {
        permfun <- match.fun(method)
        method <- "custom"
    }
    quant <- method %in% c("r2dtable", "custom")

    ind <- nestfun(comm, ...)
    if (is.list(ind))
        indstat <- ind[[statistic]]
    else
        indstat <- ind
    n <- length(indstat)
    simind <- matrix(0, nrow=n, ncol=nsimul)

    ## permutation for binary data
    if (!quant) {
        comm <- ifelse(comm > 0, 1, 0)
        if (method %in% c("swap", "tswap")){
            checkbrd <- 1
            if (method == "tswap") {
                checkbrd <- sum(designdist(comm, "(J-A)*(J-B)", "binary"))
                M <- ncol(comm)
                N <- nrow(comm)
                checkbrd <- M*(M-1)*N*(N-1)/4/checkbrd
                thin <- round(thin*checkbrd)
            }
            attr(simind, "thin") <- thin
            attr(simind, "burnin") <- burnin
            x <- comm
            if (burnin > 0)
                x <- commsimulator(x, method= method, thin = round(checkbrd) * burnin)
            for(i in 1:nsimul) {
                x <- commsimulator(x, method = method, thin = thin)
                tmp <- nestfun(x, ...)
                if (is.list(tmp))
                    simind[,i] <- tmp[[statistic]]
                else
                    simind[,i] <- tmp
            }
        }
        else {
            require(pbapply)
            pb <- startpb(0, nsimul)
            for (i in 1:nsimul) {
                x <- commsimulator(comm, method=method)
                tmp <- nestfun(x,...)
                if (is.list(tmp))
                    simind[,i] <- tmp[[statistic]]
                else
                    simind[,i] <- tmp
                setpb(pb, i)
            }
            close(pb)
        }
    ## permutation for count data
    } else {
        if (!all(dim(comm) == dim(permfun(comm))))
            stop("permutation function is not compatible with community matrix")
        ## sequential algorithms
        if (burnin > 0 || thin > 1) {
            if (burnin > 0) {
                m <- permfun(comm, burnin=burnin, thin=1)
            }  else m <- comm
            for (i in 1:nsimul) {
                tmp <- nestfun(permfun(m, burnin=0, thin=thin), ...)
                if (is.list(tmp))
                    simind[, i] <- tmp[[statistic]]
                else simind[, i] <- tmp
            }
            attr(simind, "thin") <- thin
            attr(simind, "burnin") <- burnin
        ## not sequential algorithms
        } else {
            for (i in 1:nsimul) {
                tmp <- nestfun(permfun(comm), ...)
                if (is.list(tmp)) {
                    simind[, i] <- tmp[[statistic]]
                } else simind[, i] <- tmp
            }
            attr(simind, "thin") <- NULL
            attr(simind, "burnin") <- NULL
        }
    }
    ## end of addition
    sd <- apply(simind, 1, sd)
    z <- (indstat - rowMeans(simind))/sd
    if (any(sd < sqrt(.Machine$double.eps)))
        z[sd < sqrt(.Machine$double.eps)] <- 0
    pless <- rowSums(indstat <= simind)
    pmore <- rowSums(indstat >= simind)
    p <- switch(alternative,
                two.sided = 2*pmin(pless, pmore),
                less = pless,
                greater = pmore)
    p <- pmin(1, (p + 1)/(nsimul + 1))

    ## ADDITION: if z is NA then it is not correct to calculate p values
    ## try e.g. oecosimu(dune, sum, "permat")
    if (any(is.na(z)))
        p[is.na(z)] <- NA

    if (is.null(names(indstat)))
        names(indstat) <- statistic
    if (!is.list(ind))
        ind <- list(statistic = ind)
    if (method == "custom")
        attr(method, "permfun") <- permfun
    ind$oecosimu <- list(z = z, pval = p, simulated=simind, method=method,
                         statistic = indstat, alternative = alternative)
    class(ind) <- c("oecosimu", class(ind))
    ind
}
library(vegan)
data(sipoo)
system.time(out <- oecosimu(sipoo, decorana, "quasiswap", thin=10, statistic="evals"))
system.time(out <- oecosimu2(sipoo, decorana, "quasiswap", thin=10, statistic="evals"))

## label can be added for win, and *=burnin, +=iter

library(dclone)
data(schools)
dat <- list(J = nrow(schools), y = schools$estimate, sigma.y = schools$sd)
bugs.model <- function(){
       for (j in 1:J){
         y[j] ~ dnorm (theta[j], tau.y[j])
         theta[j] ~ dnorm (mu.theta, tau.theta)
         tau.y[j] <- pow(sigma.y[j], -2)
       }
       mu.theta ~ dnorm (0.0, 1.0E-6)
       tau.theta <- pow(sigma.theta, -2)
       sigma.theta ~ dunif (0, 1000)
     }  
inits <- function(){
    list(theta=rnorm(nrow(schools), 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
param <- c("mu.theta", "sigma.theta")
sim2 <- dc.fit(dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="WinBUGS", multiply="J")
sim3 <- dc.fit(dat, param, bugs.model, n.clones=1:2, 
    flavour="bugs", program="OpenBUGS", multiply="J")


dc.fit2 <- 
function(data, params, model, inits, n.clones, multiply=NULL, unchanged=NULL, 
update=NULL, updatefun=NULL, initsfun=NULL, flavour = c("jags", "bugs"), ...)
{
    ## initail evals
    flavour <- match.arg(flavour)
    if (missing(n.clones))
        stop("'n.clones' argument must be provided")
    if (identical(n.clones, 1))
        stop("'n.clones = 1' gives the Bayesian answer, no need for DC")
    if (is.environment(data))
        stop("'data' should be list, not environment")
    ## determine k
    k <- n.clones[order(n.clones)]
    k <- unique(k)
    times <- length(k)
    rhat.crit <- getOption("dclone.rhat")
    trace <- getOption("dclone.verbose")
    ## evaluate updating
    if (!is.null(update) != !is.null(updatefun))
        stop("both 'update' and 'updatefun' must be provided")
    if (!is.null(update)) {
        unchanged <- c(unchanged, update)
        updatefun <- match.fun(updatefun)
    }
    ## evaluate inits
    if (missing(inits))
        inits <- NULL
    if (!is.null(initsfun))
        initsfun <- match.fun(initsfun)
    ## list for dcdiag results
    dcdr <- list()
    ## iteration starts here
    for (i in 1:times) {
        tmpch <- if (k[i] == 1) "clone" else "clones"
        if (trace) {
            cat("\nFitting model with", k[i], tmpch, "\n\n")
            flush.console()
        }
        jdat <- dclone(data, k[i], multiply=multiply, unchanged=unchanged)
        mod <- if (flavour == "jags") {
            jags.fit(jdat, params, model, inits, ...)
        } else {
            bugs.fit(jdat, params, model, inits, format="mcmc.list", ...)
        }
        ## dctable evaluation
        if (i == 1) {
            vn <- varnames(mod)
            dcts <- list()
            ## note: quantiles must remain unchanged, because these values are
            ## defined in extractdctable.default
            quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)
            dcts0 <- matrix(0, times, 4 + length(quantiles))
            dcts0[,1] <- k
            colnames(dcts0) <- c("n.clones", "mean", "sd", names(quantile(0, probs=quantiles)), "r.hat")
            for (j in 1:length(vn))
                dcts[[vn[j]]] <- dcts0
        } else {
            if (!is.null(update))
                jdat[[update]] <- updatefun(mod)
            if (!is.null(initsfun))
                inits <- initsfun(mod)
        }
        dctmp <- extractdctable.default(mod)
        dcdr[[i]] <- extractdcdiag.default(mod)
        for (j in 1:length(vn)) {
            dcts[[j]][i,-1] <- dctmp[j,]
        }
    }
    ## warning if R.hat < crit
    if (nchain(mod) > 1 && any(dctmp[,"r.hat"] >= rhat.crit))
        warning("chains convergence problem, see R.hat values")
    ## finalizing dctable attribute
    dcts <- lapply(dcts, function(z) as.data.frame(z))
    class(dcts) <- "dctable"
    attr(mod, "dctable") <- dcts
    ## finalizing dcdiag attribute
    dcd <- t(as.data.frame(dcdr))
    rownames(dcd) <- 1:length(dcdr)
    dcd <- data.frame(dcd)
    class(dcd) <- c("dcdiag", class(dcd))
    attr(mod, "dcdiag") <- dcd
    mod
}


summary(R2WinBUGS:::as.mcmc.list.bugs(sim2))
summary(dclone:::as.mcmc.list.bugs(sim2))

summary(as.mcmc.list.bugs1(bugs.fit(dat2, param, bugs.model, program="openbugs", format="bugs", n.thin=3)))

as.mcmc.list.bugs1 <-
function(x, ...)
{
    ## retrieve coda samples
    sarr <- x$sims.array
    ## rearranging the array into coda mcmc.list format
    res <- lapply(1:x$n.chains, function(i) sarr[,i,, drop=FALSE])
    DIM <- dim(res[[1]])[-2]
    DIMNAMES <- dimnames(res[[1]])[-2]
    for (i in 1:x$n.chains) {
        dim(res[[i]]) <- DIM
        dimnames(res[[i]]) <- DIMNAMES
    }
    ## retrieve ts attributes
    start <- x$n.burnin+1
    end <- x$n.iter
    thin <- x$n.thin

    niter <- NROW(res[[1]])
    nobs <- floor((end - start)/thin + 1)
    ## some tweaking for OpenBUGS
    if (niter < nobs) {
        start <- start + thin - 1
    }
    ## makes mcmc objects
    res <- lapply(res, function(z) mcmc(data = z,
        start = start, end = end, thin = thin))
    ## coercing into mcmc.list
    res <- as.mcmc.list(res)
    ## retrieves n.clones attr
    n.clones <- attr(x, "n.clones")
    ## final class determination based on n.clones
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}

a<-1:333 * 3

as.mcmc.list.bugs2 <- 
function (x, ...) 
{
    if (!inherits(x, "bugs")) 
        stop("Method as.mcmc.list.bugs() is only intended for bugs objects.")
    if (dim(x$sims.array)[2] != x$n.chains) 
        stop("Inconsistancy in bug object regarding the number of chains.")
    mclis <- vector("list", x$n.chains)
    strt <- x$n.burnin + 1
    end <- x$n.iter
    ord <- order(dimnames(x$sims.array)[[3]])
    for (i in 1:x$n.chains) {
        tmp1 <- x$sims.array[, i, ord]
        mclis[[i]] <- mcmc(tmp1, start = strt, end = end, thin = x$n.thin)
    }
    as.mcmc.list(mclis)
}
