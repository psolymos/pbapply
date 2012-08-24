## this returns the model to be used
## no. of parameters in the model
## do not allow to fix all params (only p-1) ???
bevertonholt <- 
function(obs.error="none", fixed) 
{

    ## Generalized Beverton-Holt model w/o obs. error
    ## p = 4
    ## model parameters: (r , K , theta, sigma) ~ {R, R+, R, R+}
    ## parameters to monitor for convergence: (r , K , theta, lnsigma)
    cm_lik_0 <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i],prcx)",
            "             mu[j,i] <- x[j-1,i] + r - log(1 + (exp(x[j-1,i])/K)^theta)",
            "         }",
            "     }")
    cm_end_0 <- c(
            r=      "     r ~ dnorm(0,4)",
            K=      "     K ~ dexp(0.005)",
            theta=  "     theta ~ dnorm(3,1)",
            sigma=  "     sigma <- exp(lnsigma)",
            lnsigma="     lnsigma ~ dnorm(0, 1)",
            prcx=   "     prcx <- 1/sigma^2",
            "}")

    ## Generalized Beverton-Holt model w/ Normal obs. error
    ## p = 5
    ## model parameters: (r , K , theta, sigma, tau) ~ {R, R+, R, R+, R+}
    ## parameters to monitor convergence: (r , K , theta, lnsigma, lntau)
    cm_lik_N <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         N[1,i] <- exp(y[1,i])",
            "         x[1,i] <- y[1,i]",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i],prcx)",
            "             mu[j,i] <- x[j-1,i] + r - log(1 + (N[j-1,i]/K)^theta)",
            "             N[j,i] <- max(exp(x[j,i]) , 1)",
            "             y[j,i]~dnorm(x[j,i],prcy)",
            "         }",
            "     }")
    cm_end_N <- c(
            r=      "     r ~ dnorm(0,4)",
            K=      "     K ~ dexp(0.01)",
            theta=  "     theta ~ dnorm(3,2.25)",
            sigma=  "     sigma <- exp(lnsigma)",
            lnsigma="     lnsigma ~ dnorm(0, 1)",
            prcx=   "     prcx <- 1/sigma^2",
            tau=    "     tau <- exp(lntau)",
            lntau="     lntau ~ dnorm(0, 1)",
            prcy=   "     prcy <- 1/tau^2",
            "}")

    ## Generalized Beverton-Holt model w/ Poisson obs. error
    ## p = 4
    ## model parameters: (r , K , theta, sigma) ~ {R, R+, R, R+}
    ## parameters to monitor for convergence: (r , K , theta, lnsigma)
    cm_lik_P <- c(
            "model {",
            "     for (i in 1:kk) {",
            "         N[1,i] <- O[1,i]",
            "         x[1,i] <- log(O[1,i])",
            "         for (j in 2:T) {",
            "             x[j,i] ~ dnorm(mu[j,i],prcx)",
            "             mu[j,i] <- x[j-1,i] + r - log(1 + (N[j-1,i]/K)^theta)",
            "             N[j,i] <- max(exp(x[j,i]) , 1)",
            "             O[j,i]~dpois(N[j,i])",
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
        "none"    = 4,
        "poisson" = 4,
        "normal"  = 5)
    ## range of support for parameters
    support <- switch(obs.error,
        "none"    = rbind(r=c(-Inf, Inf), K=c(.Machine$double.eps, Inf), 
            theta=c( -Inf, Inf), sigma=c(.Machine$double.eps, Inf)),
        "poisson" = rbind(r=c(-Inf, Inf), K=c(.Machine$double.eps, Inf), 
            theta=c( -Inf, Inf), sigma=c(.Machine$double.eps, Inf)),
        "normal"  = rbind(r=c(-Inf, Inf), K=c(.Machine$double.eps, Inf), 
            theta=c( -Inf, Inf), sigma=c(.Machine$double.eps, Inf),tau=c(.Machine$double.eps, Inf)))
    colnames(support) <- c("Min", "Max")
    ## check range of support and put in fixed values
    if (!missing(fixed)) {
        pp <- c("r","K","theta","sigma")
        if (all(pp %in% names(fixed)))
            warning("Fixing all parameters can be a bad idea, think twice!")
        if (obs.error == "normal")
            pp <- c(pp, "tau")
        tmp <- names(fixed)[!(names(fixed) %in% pp)]
        if (length(tmp))
            stop("fixed parameter ", tmp, " not found in model")
        if ("r" %in% names(fixed)) {
            if (fixed[["r"]] < support["r","Min"] || fixed[["r"]] > support["r","Max"])
                stop("support for fixed parameter 'r' ill-defined")
            cm_end["r"] <- paste("     r <-", round(fixed[["r"]], 4))
        }
        if ("K" %in% names(fixed)) {
            if (fixed[["K"]] < support["K","Min"] || fixed[["K"]] > support["K","Max"])
                stop("support for fixed parameter 'b' ill-defined")
            cm_end["K"] <- paste("     K <-", round(fixed[["K"]], 4))
        }
        if ("theta" %in% names(fixed)) {
            if (fixed[["theta"]] < support["theta","Min"] || fixed[["theta"]] > support["theta","Max"])
                stop("support for fixed parameter 'b' ill-defined")
            cm_end["theta"] <- paste("     theta <-", round(fixed[["theta"]], 4))
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
    fancy <- c("Generalized Beverton-Holt", 
        ifelse(obs.error=="none", NA, 
            gsub("\\b(\\w)", "\\U\\1", obs.error, perl=TRUE)))
    ## list params (fixed will not be a parameter)
    params <- switch(obs.error,
        "none"    = c("r","K","theta","lnsigma"),
        "poisson" = c("r","K","theta","lnsigma"),
        "normal"  = c("r","K","theta","lnsigma","lntau"))
    varnames <- switch(obs.error,
        "none"    = c("r","K","theta","sigma"),
        "poisson" = c("r","K","theta","sigma"),
        "normal"  = c("r","K","theta","sigma","tau"))
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
                    stop("not yet implemented")
                    ## null is NOE, alt is OE, NA present (II.a)
##                    ii <- ts_index(data)
##                    jj <- setdiff(which(!is.na(data)), ii)
##                    do <- sum(dnorm(data[jj][-1], 
##                        mean= mle["r"] + mle["b"] * data[jj-1][-length(jj)],
##                        sd = mle["sigma"], log=TRUE))
##                    expect <- log(mean(dnorm(data[ii], 
##                        mean= mle["a"] + mle["b"] * logx[ii-1],
##                        sd = mle["sigma"], log=FALSE)))
##                    rval <- do + expect
                } else {
                    ## null is NOE, alt is NOE, NA present (II.b)
                    y <- data # data is on log scale for "none"
                    y[m] <- logx[m]
                    rval <- sum(dnorm(y[-1],
                        mean = y[-T]+ mle["r"]+log( 1+(exp(y[-T])/mle["K"])^mle["theta"] ),
                        sd = mle["sigma"], log=TRUE))
                }
            } else {
                logd1 <- dnorm(data[-1],
                    mean = data[-T]+ mle["r"]+log( 1+(exp(data[-T])/mle["K"])^mle["theta"] ),
                    sd = mle["sigma"], log=TRUE)
                ## null is NOE, alt is OE, NA absent (III.a)
                logd2 <- if (alt_obserror) {
                    dnorm(logx[-1],
                    mean = logx[-T]+ mle["r"]+log( 1+(exp(logx[-T])/mle["K"])^mle["theta"] ),
                        sd = mle["sigma"], log=TRUE)
                ## null is NOE, alt is NOE, NA absent (III.b)
                } else {
                    ###dnorm((data[-1])[m-1],
                        ###mean = data[-T]+ mle["r"]+log( 1+(exp(data[-T])/mle["K"])^mle["theta"] ),
                        ###sd = mle["sigma"], log=TRUE)
                    0
                }
                rval <- sum(logd1) + sum(logd2)
            }
            rval
        },
        ## data on the log scale
        "poisson" = function(logx, mle, data, 
        null_obserror=FALSE, alt_obserror=FALSE) {
            T <- length(data)
            if (!(!null_obserror && any(is.na(data)))) {
                logd1 <- dnorm(logx[-1],
                    mean = logx[-T]+ mle["r"]+log( 1+(exp(logx[-T])/mle["K"])^mle["theta"] ),
                    sd = mle["sigma"], log=TRUE)
                logd2 <- dpois(data[-1],
                    exp(logx[-1]), log=TRUE)
                rval <- sum(logd1) + sum(logd2, na.rm=TRUE)
            } else {
                stop("not yet implemented")
#                rval <- sum(dpois(data, exp(logx), log=TRUE), na.rm=TRUE)
            }
            rval
        },
        ## data on the log scale
        "normal"  = function(logx, mle, data, 
        null_obserror=FALSE, alt_obserror=FALSE) {
            T <- length(data)
            if (!(!null_obserror && any(is.na(data)))) {
                logd1 <- dnorm(logx[-1],
                    mean = logx[-T]+ mle["r"]+log( 1+(exp(logx[-T])/mle["K"])^mle["theta"] ),
                    sd = mle["sigma"], log=TRUE)
                logd2 <- dnorm(data[-1],
                    mean = logx[-1],
                    sd = mle["tau"], log=TRUE)
                rval <- sum(logd1) + sum(logd2, na.rm=TRUE)
            } else {
                stop("not yet implemented")
#                rval <- sum(dnorm(data, 
#                    mean=logx, 
#                    sd=mle["sigma"], log=TRUE), na.rm=TRUE)
            }
            rval
        })
    neff <- function(obs)
        sum(!is.na(obs))
    out <- new("pvamodel")
    out@growth.model <- "bevertonholt"
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
#bevertonHolt()
#bevertonHolt("Pois")
#bevertonHolt("normal")
#bevertonHolt("normal", fixed=c(a=5, sigma=0.5))

#m1z <- pva(paurelia, bevertonHolt("none"), 2, n.iter=1000)
#m2z <- pva(paurelia, bevertonHolt("poisson"), 2, n.iter=1000)
#m3z <- pva(paurelia, bevertonHolt("normal"), 2, n.iter=1000)
#model.select(m1z, m2z)
