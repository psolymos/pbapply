variable.names.singleocc <- 
function (object, model = c("full", "occ", "det"), ...) 
{
    names(coef(object, model))
}
