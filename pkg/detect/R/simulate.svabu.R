simulate.svabu <-
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

#N <- stats:::rnbinom(n, size=1/var, prob=1/(1+var*lambda*A))

    phi <- if (object$zeroinfl)
        object$zif.probabilities else rep(0, object$nobs)
    lambda <- fitted(object)
    delta <- object$detection.probabilities
    nm <- names(object$y)
    n <- object$nobs

    if (inherits(object, "svabu_p")) {
        val <- sapply(1:nsim, function(i) rbinom(n, 
            rpois(n, lambda * rbinom(n, 1, 1-phi)), 
            delta))
    } else {
        var <- exp(object$var$est)
        val <- sapply(1:nsim, function(i) rbinom(n, 
            stats:::rnbinom(n, size=1/var, prob=1/(1+var*lambda*rbinom(n, 1, 1-phi))),
            delta))
    }
#    val <- if (object$zeroinfl) {
#        sapply(1:nsim, function(i) rbinom(n, rpois(n, lambda * rbinom(n, 1, 1-phi)), delta))
#    } else {
#        sapply(1:nsim, function(i) rbinom(n, rpois(n, lambda), delta))
#    }

    rownames(val) <- nm
    colnames(val) <- paste("sim", seq_len(nsim), sep = "_")
    attr(val, "seed") <- RNGstate
    as.data.frame(val)
}

