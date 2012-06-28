## this returns the model to be used
## no. of parameters in the model
## do not allow to fix all params (only p-1) ??? ##Correct##
ricker <- 
function(obs.error="none", fixed) 
{

    ## Ricker model w/o obs. error
    ## p = 3
    ## model parameters: (a , b , sigma) ~ {R, R+, R+}
    ## parameters to monitor for convergence: (a , b , lnsigma)
    cm_lik_0 <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         N[1,i] <- exp(x[1,i])",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i], prcx)",
            "             mu[j,i] <- a + b * N[j-1,i] + x[j-1,i]",
            "             N[j,i] <- min(exp(x[j,i]), 10000)",
            "         }",
            "     }")
    cm_end_0 <- c(
            a=      "     a ~ dnorm(1, 0.01)",
            b=      "     b ~ dnorm(0, 10)",
            sigma=  "     sigma <- exp(lnsigma)",
            lnsigma="     lnsigma ~ dnorm(0, 1)",
            prcx=   "     prcx <- 1/sigma^2",
            "}")

    ## Ricker model w/ Normal obs. error
    ## p = 4
    ## model parameters: (a , b , sigma, tau) ~ {R, R, R+, R+}
    ## parameters to monitor for convergence: (a , b , lnsigma, lntau)
    cm_lik_N <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         N[1,i] <- exp(y[1,i])",
            "         x[1,i] <- y[1,i]",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i], prcx)",
            "             mu[j,i] <- a + b * N[j-1,i] + x[j-1,i]",
            "             N[j,i] <- min(exp(x[j,i]), 10000)",
            "             y[j,i] ~ dnorm(x[j,i], prcy)",
            "         }",
            "     }")
    cm_end_N <- c(
            sigma=  "     sigma <- exp(lnsigma)",
            tau=    "     tau <- exp(lntau)",
            lnsigma="     lnsigma ~ dnorm(0, 1)",
            lntau=  "     lntau ~ dnorm(0, 1)",
            a=      "     a ~ dnorm(0, 0.01)",
            b=      "     b ~ dnorm(0, 10)",
            prcx=   "     prcx <- 1/sigma^2",
            prcy=   "     prcy <- 1/tau^2",
            "}")

    ## Ricker model w/ Poisson obs. error
    ## p = 3
    ## model parameters: (a , b , sigma) ~ {R, R, R+}
    ## parameters to monitor for convergence: (a , b , lnsigma)
    cm_lik_P <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         N[1,i] <- O[1,i]",
            "         x[1,i] <- log(O[1,i])",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i], prcx)",
            "             mu[j,i] <- a + b * N[j-1,i] + x[j-1,i]",
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
        "none"    = rbind(a=c(-Inf, Inf), b=c(-Inf, Inf), 
            sigma=c(.Machine$double.eps, Inf)),
        "poisson" = rbind(a=c(-Inf, Inf), b=c(-Inf, Inf), 
            sigma=c(.Machine$double.eps, Inf)),
        "normal"  = rbind(a=c(-Inf, Inf), b=c(-Inf, Inf), 
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
    fancy <- c("Ricker", 
        ifelse(obs.error=="none", NA, 
            gsub("\\b(\\w)", "\\U\\1", obs.error, perl=TRUE)))
    ## list params (fixed will not be a parameter)
    params <- switch(obs.error,
        "none"    = c("a","b","lnsigma"),
        "poisson" = c("a","b","lnsigma"),
        "normal"  = c("a","b","lnsigma","lntau"))
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
            if ("lnsigma" %in% vn)
                mcmc[[i]][,"lnsigma"] <- exp(mcmc[[i]][,"lnsigma"])
            if ("lntau" %in% vn)
                mcmc[[i]][,"lntau"] <- exp(mcmc[[i]][,"lntau"])
        }
        if ("lnsigma" %in% vn)
            vn[vn=="lnsigma"] <- "sigma"
        if ("lntau" %in% vn)
            vn[vn=="lntau"] <- "tau"
        varnames(mcmc) <- vn
        mcmc
    }
    ## this scales summaries to the scale of diagnostic parameters
#    transf <- function(mcmc, obs.error) {
#    }
    dfun <- switch(obs.error,
        ## data on the log scale (logx)
        ## w/o missing data, log is the log data
        ## w/ missing values both logx (missing) and data should be provided
        "none"    = function(logx, mle, data, alt_obserror=FALSE) {
            T <- length(logx)
            y <- data
            m <- which(is.na(data))
            y[m] <- logx[m]
            logd1 <- dnorm(y[-1],
                mean = y[-T] + mle["a"] + mle["b"] * exp(y[-T]),
                sd = mle["sigma"], log=TRUE)
            logd2 <- if (alt_obserror) {
                dnorm(logx[-1],
                    mean = logx[-T] + mle["a"] + mle["b"] * exp(logx[-T]),
                    sd = mle["sigma"], log=TRUE)
            } else {
                dnorm((logx[-1])[m-1],
                    mean = (logx[-T])[m] + mle["a"] + 
                        mle["b"] * exp((logx[-T])[m]),
                    sd = mle["sigma"], log=TRUE)
            }
            sum(logd1) + sum(logd2)
        },
        ## data on the log scale
        "poisson" = function(logx, mle, data, alt_obserror=FALSE) {
            T <- length(data)
            logd1 <- dnorm(logx[-1],
                mean = logx[-T] + mle["a"] + mle["b"] * exp(logx[-T]),
                sd = mle["sigma"], log=TRUE)
            logd2 <- dpois(data[-1], exp(logx[-1]), log=TRUE)
            sum(logd1) + sum(logd2, na.rm=TRUE)
        },
        ## data on the log scale
        "normal"  = function(logx, mle, data, alt_obserror=FALSE) {
            T <- length(data)
            logd1 <- dnorm(logx[-1],
                mean = logx[-T] + mle["a"] + mle["b"] * exp(logx[-T]),
                sd = mle["sigma"], log=TRUE)
            logd2 <- dnorm(data[-1],
                mean = logx[-1],
                sd = mle["tau"], log=TRUE)
            sum(logd1) + sum(logd2, na.rm=TRUE)
        })
    neff <- function(obs)
        sum(!is.na(obs)) - 1
    out <- new("pvamodel")
    out@growth.model <- "ricker"
    out@obs.error <- obs.error
    out@model <- model
    out@p <- as.integer(p)
    out@support <- support
    out@params <- params
    out@varnames <- varnames
    out@fixed <- fixed
    out@fancy <- fancy
    out@backtransf <- backtransf
#    out@transf <- transf
    out@logdensity <- dfun
    out@neffective <- neff
    out
}
## make this as an S4 class??? use contains???
#ricker()
#ricker("Pois")
#ricker("normal")
#ricker("normal", fixed=c(a=5, sigma=0.5))

