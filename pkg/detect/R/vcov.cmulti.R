vcov.cmulti <- 
function(object, ...) 
{
    rv <- object$vcov
    NAM <- names(coef(object))
    dimnames(rv) <- list(NAM, NAM)
    rv
}
