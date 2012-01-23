parListFactories <-
function(cl, type) 
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, list.factories, type=type)
}
