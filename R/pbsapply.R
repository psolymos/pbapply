pbsapply <-
function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, cl = NULL)
{
    FUN <- match.fun(FUN)
    answer <- pblapply(X = X, FUN = FUN, ..., cl = cl) # pb_specific_code
    if (USE.NAMES && is.character(X) && is.null(names(answer)))
        names(answer) <- X
    if (!identical(simplify, FALSE) && length(answer))
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}
