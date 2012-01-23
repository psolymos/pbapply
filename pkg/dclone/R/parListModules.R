parListModules <-
function(cl) 
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    clusterEvalQ(cl, require(rjags))
    clusterEvalQ(cl, list.modules())
}
