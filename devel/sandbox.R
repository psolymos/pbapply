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



