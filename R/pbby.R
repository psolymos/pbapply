# adapted from base::by
pbby <- function(data, INDICES, FUN, ..., simplify = TRUE, cl = NULL) {
    UseMethod("pbby")
}
pbby.data.frame <- function(data, INDICES, FUN, ..., simplify = TRUE,
cl = NULL) {
    if (!is.list(INDICES)) {
        IND <- list(INDICES)
        names(IND) <- deparse(substitute(INDICES))[1L]
    } else {
        IND <- INDICES
    }
    FUNx <- function(x) FUN(data[x, , drop = FALSE], ...)
    nd <- nrow(data)
    structure(
        eval(
            substitute(
                pbtapply(seq_len(nd), IND, FUNx,
                    simplify = simplify, cl = cl)
            ), 
        data), 
    call = match.call(), class = "by")
}
pbby.default <- function (data, INDICES, FUN, ..., simplify = TRUE, cl = NULL) {
    dd <- as.data.frame(data)
    if (length(dim(data))) {
        pbby(dd, INDICES, FUN, ..., simplify = simplify)
    } else {
        if (!is.list(INDICES)) {
            IND <- list(INDICES)
            names(IND) <- deparse(substitute(INDICES))[1L]
        } else {
            IND <- INDICES
        }
        FUNx <- function(x) FUN(dd[x, ], ...)
        nd <- nrow(dd)
        structure(
            eval(
                substitute(
                    pbtapply(seq_len(nd), IND, FUNx, 
                        simplify = simplify, cl = cl)
                ),
            dd),
        call = match.call(), class = "by")
    }
}

# require(stats)
# by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
# pbby(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
# by(warpbreaks[, 1],   warpbreaks[, -1],       summary)
# pbby(warpbreaks[, 1],   warpbreaks[, -1],       summary)
# by(warpbreaks, warpbreaks[,"tension"],
#    function(x) lm(breaks ~ wool, data = x))
# pbby(warpbreaks, warpbreaks[,"tension"],
#    function(x) lm(breaks ~ wool, data = x))
# ## now suppose we want to extract the coefficients by group
# tmp <- with(warpbreaks,
#             by(warpbreaks, tension,
#                function(x) lm(breaks ~ wool, data = x)))
# sapply(tmp, coef)
# tmp <- with(warpbreaks,
#             pbby(warpbreaks, tension,
#                function(x) lm(breaks ~ wool, data = x)))
# sapply(tmp, coef)
