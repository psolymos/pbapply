`update.formula.singleocc` <-
function (old, new) 
{
    oldocc <- . ~ .
    olddet <- ~.
    oldocc[[2]] <- old[[2]]
    oldocc[[3]] <- old[[3]][[2]]
    olddet[[3]] <- old[[3]][[3]]
    olddet[[2]] <- NULL
    newocc <- . ~ .
    newdet <- ~.
    newocc[[2]] <- new[[2]]
    newocc[[3]] <- new[[3]][[2]]
    newdet[[3]] <- new[[3]][[3]]
    newdet[[2]] <- NULL

    tmpocc <- update.formula(as.formula(oldocc), as.formula(newocc))
    tmpdet <- update.formula(as.formula(olddet), as.formula(newdet))

    tmp <- . ~ . | .
    tmp[[2]] <- tmpocc[[2]]
    tmp[[3]][[2]] <- tmpocc[[3]]
    tmp[[3]][[3]] <- tmpdet[[2]]

    out <- formula(terms.formula(tmp, simplify = FALSE))
    return(out)
}
