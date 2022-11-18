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

# examples
# require(stats)
# env <- new.env(hash = FALSE) # so the order is fixed
# env$a <- 1:10
# env$beta <- exp(-3:3)
# env$logic <- c(TRUE, FALSE, FALSE, TRUE)
# # what have we there?
# utils::ls.str(env)
# # compute the mean for each list element
#        eapply(env, mean)
#        pbeapply(env, mean)
# unlist(eapply(env, mean, USE.NAMES = FALSE))
# unlist(pbeapply(env, mean, USE.NAMES = FALSE))
# # median and quartiles for each element (making use of "..." passing):
# eapply(env, quantile, probs = 1:3/4)
# pbeapply(env, quantile, probs = 1:3/4)
# eapply(env, quantile)
# pbeapply(env, quantile)
