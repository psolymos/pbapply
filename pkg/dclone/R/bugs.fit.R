bugs.fit <-
function(data, params, model, inits=NULL, n.chains = 3,
format=c("mcmc.list", "bugs"), 
program=c("winbugs", "openbugs", "brugs"), seed, ...)
{
    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    ## not case sensitive evaluation of program arg
    program <- match.arg(tolower(program), 
        c("winbugs", "openbugs", "brugs"))
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
    ## WinBUGS evaluation is simple
    ## only default behavour is changed for args
    if (program == "winbugs") {
        if (missing(seed))
            seed <- NULL
        res <- R2WinBUGS:::bugs(data=data, 
            inits=inits, 
            parameters.to.save=params, 
            model.file=model, 
            n.chains=n.chains, 
            codaPkg=FALSE, 
            bugs.seed=seed, ...)
    }
    ## OpenBUGS needs model file, and can't provide mcmc.list as output
    ## thin != 1 can cause problems in conversion -- not anymore, fixed
    if (program == "brugs") {
        warnings("program = 'openbugs' is the preferred interface")
        if (missing(seed))
            seed <- NULL
        res <- R2WinBUGS:::openbugs(data=data, 
            inits=inits, 
            parameters.to.save=params, 
            model.file=model, 
            n.chains=n.chains, seed=seed, ...)
    }
    if (program == "openbugs") {
        if (missing(seed))
            seed <- 1
        res <- R2OpenBUGS:::bugs(data=data, 
            inits=inits, 
            parameters.to.save=params, 
            model.file=model,
            n.chains=n.chains, 
            codaPkg=FALSE, 
            bugs.seed=seed, ...)
    }
    ## converting bugs objects into mcmc.list
    format <- match.arg(format)
    if (format == "mcmc.list")
        res <- dclone:::as.mcmc.list.bugs(res)
    ## adding n.clones attribute, and class attr if mcmc.list
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        if (format == "mcmc.list")
            class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
