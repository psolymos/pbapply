bugs.fit <-
function(data, params, model, inits=NULL, format=c("mcmc.list", "bugs"), 
program=c("winbugs", "openbugs"), DIC=FALSE, dir=getwd(), ...)
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
    ## WinBUGS evaluation is simple
    ## only default behavour is changed for args
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
    ## converting bugs objects into mcmc.list
    if (match.arg(format) == "mcmc.list")
        res <- as.mcmc.list(res)
    ## adding n.clones attribute
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
    }
    res
}

