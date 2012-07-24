## this returns the model to be used
## no. of parameters in the model
## do not allow to fix all params (only p-1) ???
gompertz <- 
function(obs.error="none", fixed) 
{

    ## Gompertz model w/o obs. error
    ## p = 3
    ## model parameters: (a , b , sigma) ~ {R, (-2 , 0), R+}
    ## parameters to monitor for convergence: (a , z , lnsigma)
    cm_lik_0 <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         x[1,i] ~ dnorm(m, prc)",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i], prcx)",
            "             mu[j,i] <- a + (1+b)*x[j-1,i]",
            "         }",
            "     }")
    cm_end_0 <- c(
            c=      "     c <- 1 + b",
            z=      "     z <- 0.5 * log((1+c) / (1-c))",
            m=      "     m <- a / (1-c)",
            pcr=    "     prc <- (1-(c*c)) / (sigma*sigma)",
            a=      "     a ~ dnorm(0, 0.01)",
            b=      "     b ~ dunif(-0.999, -0.0001)",
            sigma=  "     sigma <- exp(lnsigma)",
            lnsigma="     lnsigma ~ dnorm(0, 1)",
            prcx=   "     prcx <- 1/sigma^2",
            "}")

    ## Gompertz model w/ Normal obs. error
    ## p = 4
    ## model parameters: (a , b , sigma, tau) ~ {R, (-2 , 0), R+, R+}
    ## parameters to monitor convergence: (a , z , lnsigma, lntau)
    cm_lik_N <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         x[1,i] ~ dnorm(m, prc)",
            "         y[1,i] ~ dnorm(x[1,i], prcy)",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i], prcx)",
            "             mu[j,i] <- a + (1+b)*x[j-1,i]",
            "             y[j,i] ~ dnorm(x[j,i], prcy)",
            "         }",
            "     }")
    cm_end_N <- c(
            c=      "     c <- 1 + b",
            z=      "     z <- 0.5 * log((1+c) / (1-c))",
            sigma=  "     sigma <- exp(lnsigma)",
            tau=    "     tau <- exp(lntau)",
            m=      "     m <- a / (1-c)",
            prc=    "     prc <- (1-(c*c)) / (sigma*sigma)",
            lnsigma="     lnsigma ~ dnorm(0, 1)",
            lntau=  "     lntau ~ dnorm(0, 1)",
            a=      "     a ~ dnorm(0, 0.01)",
            b=      "     b ~ dunif(-0.999, -0.0001)",
            prcx=   "     prcx <- 1/sigma^2",
            prcy=   "     prcy <- 1/tau^2",
            "}")

    ## Gompertz model w/ Poisson obs. error
    ## p = 3
    ## model parameters: (a , b , sigma) ~ {R, (-2 , 0), R+}
    ## parameters to monitor for convergence: (a , z , lnsigma)
    cm_lik_P <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         x[1,i] ~ dnorm(m, prc)",
            "         N[1,i] <- min(exp(x[1,i]), 10000)",
            "         O[1,i] ~ dpois(N[1,i])",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i], prcx)",
            "             mu[j,i] <- a + (1+b)*x[j-1,i]",
            "             N[j,i] <- min(exp(x[j,i]), 10000)",
            "             O[j,i] ~ dpois(N[j,i])",
            "         }",
            "     }")
    cm_end_P <- cm_end_0

    ## match observation error type
    obs.error <- match.arg(tolower(obs.error), 
        c("none", "normal", "poisson"))
    ## put together the model file
    cm_lik <- switch(obs.error,
        "none"    = cm_lik_0,
        "poisson" = cm_lik_P,
        "normal"  = cm_lik_N)
    cm_end <- switch(obs.error,
        "none"    = cm_end_0,
        "poisson" = cm_end_P,
        "normal"  = cm_end_N)
    ## number of model parameters
    p <- switch(obs.error,
        "none"    = 3,
        "poisson" = 3,
        "normal"  = 4)
    ## range of support for parameters
    support <- switch(obs.error,
        "none"    = rbind(a=c(-Inf, Inf), b=c(-2, 0), 
            sigma=c(.Machine$double.eps, Inf)),
        "poisson" = rbind(a=c(-Inf, Inf), b=c(-2, 0), 
            sigma=c(.Machine$double.eps, Inf)),
        "normal"  = rbind(a=c(-Inf, Inf), b=c(-2, 0), 
            sigma=c(.Machine$double.eps, Inf), tau=c(.Machine$double.eps, Inf)))
    colnames(support) <- c("Min", "Max")
    ## check range of support and put in fixed values
    if (!missing(fixed)) {
        pp <- c("a","b","sigma")
        if (all(pp %in% names(fixed)))
            warning("Fixing all parameters can be a bad idea, think twice!")
        if (obs.error == "normal")
            pp <- c(pp, "tau")
        tmp <- names(fixed)[!(names(fixed) %in% pp)]
        if (length(tmp))
            stop("fixed parameter ", tmp, " not found in model")
        if ("a" %in% names(fixed)) {
            if (fixed[["a"]] < support["a","Min"] || fixed[["a"]] > support["a","Max"])
                stop("support for fixed parameter 'a' ill-defined")
            cm_end["a"] <- paste("     a <-", round(fixed[["a"]], 4))
        }
        if ("b" %in% names(fixed)) {
            if (fixed[["b"]] < support["b","Min"] || fixed[["b"]] > support["b","Max"])
                stop("support for fixed parameter 'b' ill-defined")
            cm_end["b"] <- paste("     b <-", round(fixed[["b"]], 4))
        }
        if ("sigma" %in% names(fixed)) {
            if (fixed[["sigma"]] < support["sigma","Min"] || fixed[["sigma"]] > support["sigma","Max"])
                stop("support for fixed parameter 'sigma' ill-defined")
            cm_end["sigma"] <- paste("     sigma <-", round(fixed[["sigma"]], 4))
            cm_end["lnsigma"] <- "     lnsigma <- log(sigma)"
        }
        if ("tau" %in% names(fixed)) {
            if (fixed[["tau"]] < support["tau","Min"] || fixed[["tau"]] > support["tau","Max"])
                stop("support for fixed parameter 'tau' ill-defined")
            cm_end["tau"] <- paste("     tau <-", round(fixed[["tau"]], 4))
            cm_end["lntau"] <- "     lntau <- log(tau)"
        }
    } else fixed <- NULL
    ## put together stuff
    model <- structure(c(cm_lik, unname(cm_end)),
            class = "custommodel")
    fancy <- c("Gompertz", 
        ifelse(obs.error=="none", NA, 
            gsub("\\b(\\w)", "\\U\\1", obs.error, perl=TRUE)))
    ## list params (fixed will not be a parameter)
    params <- switch(obs.error,
        "none"    = c("a","z","lnsigma"),
        "poisson" = c("a","z","lnsigma"),
        "normal"  = c("a","z","lnsigma","lntau"))
    varnames <- switch(obs.error,
        "none"    = c("a","b","sigma"),
        "poisson" = c("a","b","sigma"),
        "normal"  = c("a","b","sigma","tau"))
    params <- params[!(varnames %in% names(fixed))]

    ## this scales diagnostic parameters to the scale of the summaries
    backtransf <- function(mcmc, obs.error) {
        mcmc <- as.mcmc.list(mcmc)
        vn <- varnames(mcmc)
        for (i in seq_len(nchain(mcmc))) {
            if ("z" %in% vn)
                mcmc[[i]][,"z"] <- tanh(mcmc[[i]][,"z"]) - 1
            if ("lnsigma" %in% vn)
                mcmc[[i]][,"lnsigma"] <- exp(mcmc[[i]][,"lnsigma"])
            if ("lntau" %in% vn)
                mcmc[[i]][,"lntau"] <- exp(mcmc[[i]][,"lntau"])
        }
        if ("z" %in% vn)
            vn[vn=="z"] <- "b"
        if ("lnsigma" %in% vn)
            vn[vn=="lnsigma"] <- "sigma"
        if ("lntau" %in% vn)
            vn[vn=="lntau"] <- "tau"
        varnames(mcmc) <- vn
        mcmc
    }
    ## this scales summaries to the scale of diagnostic parameters
    transf <- function(mcmc, obs.error) {
        mcmc <- as.mcmc.list(mcmc)
        vn <- varnames(mcmc)
        for (i in seq_len(nchain(mcmc))) {
            if ("b" %in% vn)
                mcmc[[i]][,"b"] <- atanh(mcmc[[i]][,"b"] + 1)
            if ("sigma" %in% vn)
                mcmc[[i]][,"sigma"] <- log(mcmc[[i]][,"sigma"])
            if ("tau" %in% vn)
                mcmc[[i]][,"tau"] <- log(mcmc[[i]][,"tau"])
        }
        if ("b" %in% vn)
            vn[vn=="b"] <- "z"
        if ("sigma" %in% vn)
            vn[vn=="sigma"] <- "lnsigma"
        if ("tau" %in% vn)
            vn[vn=="tau"] <- "lntau"
        varnames(mcmc) <- vn
        mcmc
    }
    ## log density function arguments:
    ## data: observations (scale depends on model)
    ## mle: vector with estimates as in coef()
    ## logx: latent variable prediction for random effects model case
    ##       this includes w/o error models with missing data
    ## missing: logical, used for w/o error model (see logx)
    dfun <- switch(obs.error,
        ## logx: log obs (w/0 obs error) or latent variable (w/ obs error)
        ## mle: vector of point estimates
        ## data: data on original scale (this is used to check missing values)
        ## null_obserror: logical, if the null model has obs error
        ## alt_obserror: logical, if the alternative model has obs error
        "none"    = function(logx, mle, data, 
        null_obserror=FALSE, alt_obserror=FALSE) {
            T <- length(logx)
            m <- which(is.na(data))
            if (length(m) > 0) {
                if (alt_obserror) {
                    ## null is NOE, alt is OE, NA present (II.a)
                    ii <- ts_index(data)
                    jj <- setdiff(which(!is.na(data)), ii)
                    do <- exp(sum(dnorm(data[jj][-1], 
                        mean= mle["a"] + mle["b"] * data[jj-1][-length(jj)],
                        sd = mle["sigma"], log=TRUE)))
                    expect <- mean(dnorm(data[ii], 
                        mean= mle["a"] + mle["b"] * logx[ii-1],
                        sd = mle["sigma"], log=FALSE))
                    rval <- log(do*expect)
                } else {
                    ## null is NOE, alt is NOE, NA present (II.b)
                    y <- data # data is on log scale for "none"
                    y[is.na(data)] <- logx[is.na(data)]
                    rval <- sum(dnorm(y[-1],
                        mean = mle["a"] + mle["b"] * data[-T],
                        sd = mle["sigma"], log=TRUE))
                }
            } else {
                logd1 <- dnorm(data[-1],
                    mean = mle["a"] + mle["b"] * data[-T],
                    sd = mle["sigma"], log=TRUE)
                ## null is NOE, alt is OE, NA absent (III.a)
                logd2 <- if (alt_obserror) {
                    dnorm(logx[-1],
                        mean = mle["a"] + mle["b"] * logx[-T],
                        sd = mle["sigma"], log=TRUE)
                ## null is NOE, alt is NOE, NA absent (III.b)
                } else {
                    dnorm((data[-1])[m-1],
                        mean = mle["a"] + mle["b"] * (data[-T])[m],
                        sd = mle["sigma"], log=TRUE)
                }
            rval <- sum(logd1) + sum(logd2)
            }
            rval
        },
        ## data on the log scale
        "poisson" = function(logx, mle, data, 
        null_obserror=FALSE, alt_obserror=FALSE) {
            T <- length(data)
            if (null_obserror) {
                logd1 <- dnorm(logx[-1],
                    mean = mle["a"] + mle["b"] * logx[-T],
                    sd = mle["sigma"], log=TRUE)
                logd2 <- dpois(data[-1],
                    exp(logx[-1]), log=TRUE)
                rval <- sum(logd1) + sum(logd2, na.rm=TRUE)
            } else {
                rval <- sum(dpois(data, exp(logx), log=TRUE), na.rm=TRUE)
            }
            rval
        },
        ## data on the log scale
        "normal"  = function(logx, mle, data, 
        null_obserror=FALSE, alt_obserror=FALSE) {
            T <- length(data)
            if (null_obserror) {
                logd1 <- dnorm(logx[-1],
                    mean = mle["a"] + mle["b"] * logx[-T],
                    sd = mle["sigma"], log=TRUE)
                logd2 <- dnorm(data[-1],
                    mean = logx[-1],
                    sd = mle["tau"], log=TRUE)
                rval <- sum(logd1) + sum(logd2, na.rm=TRUE)
            } else {
                rval <- sum(dnorm(data, 
                    mean=logx, 
                    sd=mle["sigma"], log=TRUE), na.rm=TRUE)
            }
            rval
        })
    neff <- function(obs)
        sum(!is.na(obs))
    out <- new("pvamodel")
    out@growth.model <- "gompertz"
    out@obs.error <- obs.error
    out@model <- model
    out@p <- as.integer(p)
    out@support <- support
    out@params <- params
    out@varnames <- varnames
    out@fixed <- fixed
    out@fancy <- fancy
    out@backtransf <- backtransf
    out@transf <- transf
    out@logdensity <- dfun
    out@neffective <- neff
    out
}
## make this as an S4 class??? use contains???
#gompertz()
#gompertz("Pois")
#gompertz("normal")
#gompertz("normal", fixed=c(a=5, sigma=0.5))

