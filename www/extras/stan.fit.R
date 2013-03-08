## stan.fit for dclone
library(dclone)

as.mcmc.list.stanfit <- function(x, ...) {
    a <- extract(x, permuted=FALSE, inc_warmup=FALSE)
    a <- a[,,dimnames(a)[[3]] != "lp__"]
    res <- as.mcmc.list(lapply(seq_len(dim(a)[2]), function(i) {
        mcmc(a[,i,], 
            start = x@sim$iter - x@sim$warmup + x@sim$thin, 
            thin = x@sim$thin)
    }))
    n.clones <- nclones(x)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}

stan.fit <- 
function(data, params, model, inits = NULL, 
    seed = sample.int(.Machine$integer.max, 1), 
    n.chains = 3, 
    format = c("mcmc.list", "stanfit"), 
    stan.model = TRUE, fit = NA, chain_id, ...)
{
    if (!suppressWarnings(require(rstan))) 
        stop("there is no package called 'rstan'")
    format <- match.arg(format)
    if (missing(params))
        params <- NA
    if (missing(chain_id))
        chain_id <- as.integer(seq_len(n.chains))
    if (length(chain_id) != n.chains)
        stop("length of 'chain_id' must equal 'n.chains'")
    if (is.null(inits))
        inits <- "random"
    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    n.clones <- dclone:::nclones.list(data)

    # ignored: model_name = "anon_model", model_code = ""
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model)) 
            model <- match.fun(model)
        model <- write.jags.model(model)
        on.exit(try(clean.jags.model(model)))
    }
    if (identical(fit, NA)) {
        fit0 <- stan(file=model, 
            data = data, pars = params, 
            chains = n.chains, chain_id = chain_id, seed = seed,
            init = inits, 
            fit = fit,
            ...)
    } else {
        if (is.character(fit))
            fit <- get(fit, pos = -1, inherits = TRUE)
        if (inherits(fit, "mcmc.list")) {
            sm <- stan.model(fit)
            if (is.null(sm))
                stop("no 'stan.model' attribute found in 'fit'")
        }
        if (inherits(fit, "stanmodel"))
            sm <- fit
        if (inherits(fit, "stanfit"))
            sm <- get_stanmodel(fit)
            
        fit0 <- sampling(sm, 
            data = data, pars = params, 
            chains = n.chains, chain_id = chain_id, seed = seed,
            init = inits, 
            ...)
    }
    ## dc info stuff
    if (format == "mcmc.list") {
        res <- as.mcmc.list(fit0)
        if (stan.model)
            attr(res, "stan.model") <- get_stanmodel(fit0)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
            class(res) <- c("mcmc.list.dc", class(res))
        }
    } else {
        res <- fit0
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
        }
    }
    res
}

stan.model <- function (object, ...)
    UseMethod("stan.model")

stan.model.mcmc.list <- function (object, ...)
    attr(object, "stan.model")

stan.model.mcmc.list.dc <- function (object, ...) {
    attr(attr(object, "stan.model"), "n.clones") <- nclones(object)
    attr(object, "stan.model")
}

stan.parfit <- 
function(cl, data, params, model, inits = NULL, 
    seed = sample.int(.Machine$integer.max, 1), 
    n.chains = 3, 
    format = c("mcmc.list", "stanfit"), 
    stan.model = TRUE, fit = NA, chain_id, ...)
{
    cl <- evalParallelArgument(cl, quit = TRUE)
    if (is.null(cl)) {
        return(stan.fit(data, params, model, inits = NULL, 
            seed = sample.int(.Machine$integer.max, 1), 
            n.chains = 3, 
            format = c("mcmc.list", "stanfit"), 
            stan.model = TRUE, fit = NA, chain_id, ...))
    }

    if (!suppressWarnings(require(rstan))) 
        stop("there is no package called 'rstan'")
    format <- match.arg(format)
    if (missing(params))
        params <- NA
    if (missing(chain_id))
        chain_id <- as.integer(seq_len(n.chains))
    if (length(chain_id) != n.chains)
        stop("length of 'chain_id' must equal 'n.chains'")
    ## this is rstan default
    if (is.null(inits))
        inits <- "random"
    ## this is to generate inits list fron function
    if (is.function(inits)) {
        inits <- if (is.null(formals(inits))) {
            lapply(chain_id, function(i) inits())
        } else {
            lapply(chani_id, inits)
        }
    }
    ## this is to repeat if 0 "0" "random" or list()
    ## i.e. not list of lists
    if (length(inits) != n.chains && 
        !identical(unique(unlist(lapply(inits, class))), "list")) {
            inits <- lapply(seq_len(n.chains), function(i) inits)
    }

    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    n.clones <- dclone:::nclones.list(data)
    trace <- getOption("dcoptions")$verbose
    if (n.chains == 1) 
        stop("no need for parallel computing with 1 chain")
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model)) 
            model <- match.fun(model)
        if (is.numeric(cl) || inherits(cl, "SOCKcluster")) {
            model <- write.jags.model(model)
            on.exit(try(clean.jags.model(model)))
        }
    }

    cldata <- list(data = data, params = params, model = model, 
        inits = inits, fit = fit, chain_id = chain_id, seed = seed)
    stanparallel <- function(i, ...) {
        cldata <- as.list(get(".DcloneEnv", envir = .GlobalEnv))
        ## here one have to do stuff
        ini <- cldata$inits[[i]]
        stan.fit(data = cldata$data, 
            params = cldata$params, 
            model = cldata$model, 
            inits = cldata$inits[[i]], 
            seed = sample.int(.Machine$integer.max, 1), 
            n.chains = 1, 
            format = "stanfit", 
            stan.model = FALSE, 
            fit = cldata$fit, 
            chain_id = i, ...)
    }
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    balancing <- if (getOption("dcoptions")$LB) 
        "load"
    else "none"
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    mcmc <- snowWrapper(cl, 1:n.chains, stanparallel, cldata, 
        name = NULL, use.env = TRUE, lib = c("dclone", "rstan"), 
        balancing = balancing, size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = dir, unload = FALSE, ...)
    fit0 <- sflist2stanfit(mcmc)
    ## dc info stuff
    if (format == "mcmc.list") {
        res <- as.mcmc.list(fit0)
        if (stan.model)
            attr(res, "stan.model") <- get_stanmodel(fit0)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
            class(res) <- c("mcmc.list.dc", class(res))
        }
    } else {
        res <- fit0
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
        }
    }
    res
}

