update.formula.svisit <-
function (old, new) 
{
    oldsta <- . ~ .
    olddet <- ~.
    oldsta[[2]] <- old[[2]]
    oldsta[[3]] <- old[[3]][[2]]
    olddet[[3]] <- old[[3]][[3]]
    olddet[[2]] <- NULL
    newsta <- . ~ .
    newdet <- ~.
    newsta[[2]] <- new[[2]]
    newsta[[3]] <- new[[3]][[2]]
    newdet[[3]] <- new[[3]][[3]]
    newdet[[2]] <- NULL

    tmpsta <- update.formula(as.formula(oldsta), as.formula(newsta))
    tmpdet <- update.formula(as.formula(olddet), as.formula(newdet))

    tmp <- . ~ . | .
    tmp[[2]] <- tmpsta[[2]]
    tmp[[3]][[2]] <- tmpsta[[3]]
    tmp[[3]][[3]] <- tmpdet[[2]]

    out <- formula(terms.formula(tmp, simplify = FALSE))
    return(out)
}

