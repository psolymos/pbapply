bugs.fit <-
function(data, params, model, inits=NULL, format=c("mcmc.list", "bugs"), 
program=c("winbugs", "openbugs"), DIC=FALSE, dir=getwd(), ...)
{
    program <- match.arg(tolower(program), c("winbugs", "openbugs"))
    n.clones <- dclone:::nclones.list(data)
    data <- lapply(data, function(z) {
        attr(z, "n.clones") <- NULL
        z
    })
    ## WinBUGS evaluation is simple
    if (program == "winbugs") {
        res <- bugs(data, inits, params, model, DIC=DIC, codaPkg=FALSE, working.directory=dir, ...)
    } else {
    ## OpenBUGS needs model file, and can't provide mcmc.list as output
    ## thin != 1 can cause problems in conversion
        if (is.function(model)) {
            model <- match.fun(model)
            model <- write.jags.model(model)
            on.exit(clean.jags.model(model))
        }
        res <- openbugs(data, inits, params, model, DIC=DIC, working.directory=dir, ...)
    }
    if (match.arg(format) == "mcmc.list")
        res <- as.mcmc.list(res)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
#        class(res) <- c("bugs.dc", class(res))
    }
    res
}

