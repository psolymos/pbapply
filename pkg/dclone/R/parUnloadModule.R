parUnloadModule <-
function(cl, name, quiet = FALSE) 
{
    clusterEvalQ(cl, require(rjags))
    clusterCall(cl, unload.module,
        name=name, quiet=quiet)
}
