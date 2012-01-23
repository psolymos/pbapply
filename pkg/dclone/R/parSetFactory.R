parSetFactory <-
function(cl, name, type, state) 
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, set.factory,
        name=name, type=type, state=state)
}
