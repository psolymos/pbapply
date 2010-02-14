bugs.fit <-
function(data, params, model, inits=NULL, format=c("mcmc.list", "bugs"), 
program=c("winbugs", "openbugs"), DIC=FALSE, bugs.seed=NULL, dir=getwd(), ...)
{
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
    ## WinBUGS evaluation is simple
    ## only default behavour is changed for args
    if (program == "winbugs") {
        res <- bugs(data, inits, params, model, DIC=DIC, codaPkg=FALSE, working.directory=dir, bugs.seed=bugs.seed, ...)
    } else {
    ## OpenBUGS needs model file, and can't provide mcmc.list as output
    ## thin != 1 can cause problems in conversion
        res <- openbugs.seed(data, inits, params, model, DIC=DIC, working.directory=dir, bugs.seed=bugs.seed, ...)
    }
    ## converting bugs objects into mcmc.list
    format <- match.arg(format)
    if (format == "mcmc.list")
        res <- as.mcmc.list(res, DIC=DIC)
    ## adding n.clones attribute, and class attr if mcmc.list
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        if (format == "mcmc.list")
            class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}

