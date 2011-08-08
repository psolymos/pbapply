simulate.svocc <-
function(object, nsim = 1, seed = NULL, ...)
{
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    if (is.null(seed)) {
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    } else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
    phi <- object$occurrence.probabilities
    delta <- object$detection.probabilities
    nm <- names(object$y)
    n <- object$nobs
    val <- sapply(1:nsim, function(i) rbinom(n, 1, rbinom(n, 1, phi) * delta))
    rownames(val) <- nm
    colnames(val) <- paste("sim", seq_len(nsim), sep = "_")
    attr(val, "seed") <- RNGstate
    as.data.frame(val)
}

