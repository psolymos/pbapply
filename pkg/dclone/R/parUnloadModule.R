parUnloadModule <-
function(cl, name, quiet = FALSE) 
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, unload.module,
        name=name, quiet=quiet)
}
