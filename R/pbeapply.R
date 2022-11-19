pbeapply <- function(env, FUN, ..., 
all.names = FALSE, USE.NAMES = TRUE, cl = NULL) {
    FUN <- match.fun(FUN)
    # adapted from future.apply::future_eapply
    names <- ls(envir = env, all.names = all.names, sorted = FALSE)
    X <- mget(names, envir = env, inherits = FALSE)
    if (!USE.NAMES)
        names(X) <- NULL
    pblapply(X = X, FUN = FUN, ..., cl = cl)
}
