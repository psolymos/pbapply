parListModules <-
function(cl) 
{
    clusterEvalQ(cl, require(rjags))
    clusterEvalQ(cl, list.modules())
}
