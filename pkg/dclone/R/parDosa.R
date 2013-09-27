parDosa <-
function(cl, seq, fun, cldata, 
    lib = NULL, dir = NULL, evalq = NULL,
    size = 1, balancing = c("none", "load", "size", "both"), 
    rng.type = c("none", "RNGstream", "SPRNG"), 
    cleanup = TRUE, unload = FALSE, ...)
{
    ## get defaults right for cl argument
    cl <- evalParallelArgument(cl, quit=TRUE)
## common stuff for snow and multicore
    rng.type <- match.arg(rng.type) 
    TRUENAM <- "cldata"
    TMPNAM <- "cldata"
## snow cluster
    if (inherits(cl, "cluster")) {

        ## push cldata
        ## this uses type="model"
        TMPNAM <- tempfile("cldata", "")
        TMPNAM <- substr(TMPNAM, 2, nchar(TMPNAM))
        ## use TMPNAM to avoid overwiting object on workers
        pushDcloneEnv(TMPNAM, cldata, type = "model")
        on.exit(clearDcloneEnv(list=listDcloneEnv(type = "model"), 
            type = "model"))

        require(snow)
        balancing <- match.arg(balancing)
        ## loads lib on each worker
        if (!is.null(lib)) {
            pkglist <- unique(unlist(clusterEvalQ(cl, .packages())))
            for (i in lib)
                if (!(i %in% pkglist))
                    eval(parse(text=paste("clusterEvalQ(cl, library(", i, "))", sep="")))
        }
        ## set seed on each worker
        if (rng.type != "none") {
            clusterSetupRNG(cl, type = rng.type)
        }
        ## sets common working directory, but dir can be a vector as well
        if (!is.null(dir)) {
            if (cleanup)
                dirold <- clusterEvalQ(cl, getwd())
            dir <- rep(dir, length(cl))[1:length(cl)]
            dirnew <- lapply(dir, function(z) paste("setwd(\"", z, "\")", sep=""))
            parLapply(cl, dirnew, function(z) eval(parse(text=z)))
        }
        ## evaluates literal expressions if needed
        if (!is.null(evalq)) {
            for (i in evalq)
                eval(parse(text=paste("clusterEvalQ(cl,", i, ")")))
        }
        ## export TMPNAM to workers
        clusterExport(cl, TMPNAM, envir = dclone:::.DcloneEnvModel)
        ## push TMPNAM into .env as TRUENAM
        eval(parse(text=paste("clusterEvalQ(cl, pushDcloneEnv(\"",
            TRUENAM, "\", ", TMPNAM, ", type = \"model\"))", sep="")))
        ## remove TMPNAM
        eval(parse(text=paste("clusterEvalQ(cl, rm(list=\"",
            TMPNAM, "\"))", sep="")))
        ## parallel work done here according to balancing
        res <- switch(balancing,
            "none" = parLapply(cl, seq, fun, ...),
            "load" = clusterApplyLB(cl, seq, fun, ...),
            "size" = parLapplySB(cl, seq, size=size, fun, ...),
            "both" = parLapplySLB(cl, seq, size=size, fun, ...))
        if (cleanup) {
            ## remove all objects from .env
            clusterEvalQ(cl, 
                clearDcloneEnv(list=listDcloneEnv(type = "model"), 
                type = "model"))
            ## set old dir
            if (!is.null(dir)) {
                dirold <- lapply(dirold, function(z) paste("setwd(\"", z, "\")", sep=""))
                parLapply(cl, dirold, function(z) eval(parse(text=z)))
            }
        }
        ## unload libs
        if (unload && !is.null(lib)) {
            for (i in lib)
                if (!(i %in% pkglist))
                    eval(parse(text=paste("clusterEvalQ(cl, detach(package:", 
                        i, ", unload=TRUE))", sep="")))
        }
    } else {
## multicore
        ## TMPNAM is TRUENAM
        pushDcloneEnv(TMPNAM, cldata, type = "model")

        #require(parallel)
        if (balancing == "load") {
            balancing <- "none"
            warning("forking is used: balancing type was set to 'none'")
        }
        if (balancing == "both") {
            balancing <- "size"
            warning("forking is used: balancing type was set to 'size'")
        }
        res <- mclapplySB(seq, fun, ...,
            mc.preschedule = (balancing == "none"), # no need to preschedule when SB
            mc.set.seed = !(rng.type == "none"),
            mc.silent = as.logical(getOption("dcoptions")$verbose), 
            mc.cores = cl,
            mc.cleanup = cleanup, 
            mc.allow.recursive = FALSE, # no need for recursive forking
            size = size)
    }
    res
}
