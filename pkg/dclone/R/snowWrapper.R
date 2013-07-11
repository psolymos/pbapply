snowWrapper <-
function(cl, seq, fun, cldata, name = "cldata", use.env = FALSE,
lib = NULL, dir = NULL, evalq = NULL,
size = 1, balancing = c("none", "load", "size", "both"), 
rng.type = c("none", "RNGstream", "SPRNG"), 
cleanup = TRUE, unload = FALSE, envir = .GlobalEnv, ...)
{
    warning("'snowWrapper' IS DEPRECATED. USE 'parDosa' INSTEAD!")

    ## get defaults right for cl argument
    cl <- evalParallelArgument(cl, quit=TRUE)
## common stuff for snow and multicore
    rng.type <- match.arg(rng.type) 
    ## if object name exists in envir, make a copy as tmp, and put back in the end
    if (!use.env && exists(name, envir=envir)) {
        assign("tmp", get(name, envir=envir))
        on.exit(rm(list=name, envir=envir), add=TRUE)
        on.exit(assign(name, tmp, envir=envir), add=TRUE)
    }
    if (use.env) {
        if (is.null(name))
            name <- ".DcloneEnv"
        assign(name, as.environment(cldata), envir=envir)
    }
    ## place object name into envir (clusterExport can reach it)
    if (!use.env)
        assign(name, cldata, envir=envir)
    ## from R >= 2.14.1 clusterExport allows other envir than GlobalEnv
    ## this is a feature that needs to be incorporated at some point
## snow cluster
    if (inherits(cl, "cluster")) {
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
        ## grab data list/env from envir
        clusterExport(cl, name)
        #clusterExport(cl, name, envir) # make use of this feature later
        ## parallel work done here according to balancing
        res <- switch(balancing,
            "none" = parLapply(cl, seq, fun, ...),
            "load" = clusterApplyLB(cl, seq, fun, ...),
            "size" = parLapplySB(cl, seq, size=size, fun, ...),
            "both" = parLapplySLB(cl, seq, size=size, fun, ...))
        if (cleanup) {
            ## remove cldata by name
            eval(parse(text=paste("clusterEvalQ(cl, rm(", name, "))")))
#            if (!use.env)
#                eval(parse(text=paste("clusterEvalQ(cl, rm(", name, "))")))
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
        require(parallel)
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
