pbvapply <- function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE, cl = NULL) {
    FUN <- match.fun(FUN)
    if (!is.vector(X) || is.object(X))
        X <- as.list(X)
    # adapted from future.apply::future_vapply
    n <- length(X)
    if(!is.function(FUN))
        stop("FUN must be a function")
    if(!(is.vector(FUN.VALUE) || is.array(FUN.VALUE)))
        stop("FUN.VALUE must be a vector or an array")
    type <- typeof(FUN.VALUE)
    times <- length(FUN.VALUE)
    dim <- dim(FUN.VALUE)
    if(!(is.logical(USE.NAMES) && length(USE.NAMES) == 1L && !is.na(USE.NAMES)))
        stop("USE.NAMES must be TRUE/FALSE")
    valid_types <- switch(type, 
        logical = "logical", 
        integer = c("logical", "integer"), 
        double = c("logical", "integer", "double"), 
        complex = c("logical", "integer", "double", "complex"), 
        type)
    x_FUN <- FUN
    res <- pblapply(X, FUN = function(x, ...) {
        value <- x_FUN(x, ...)
        if (length(value) != times) {
            stop(sprintf("values must be length %d, but FUN(X[[ii]]) result is length %d",
                times, length(value)))
        }
        if (!all(dim(value) == dim))
            stop("Dimensions are wrong")
        if (!(typeof(value) %in% valid_types))
            stop("Types are not valid")
        value
    }, ..., cl = cl)
    if (!is.null(dim)) {
        dim_res <- c(dim, n)
    } else if (times != 1L) {
        dim_res <- c(times, n)
    } else {
        dim_res <- NULL
    }
    if (USE.NAMES && length(res) > 0L) {
        if (is.null(dim)) {
            names_FUN.VALUE <- names(FUN.VALUE)
            if (is.null(names_FUN.VALUE))
                names_FUN.VALUE <- names(res[[1]])
        } else {
            names_FUN.VALUE <- dimnames(FUN.VALUE)
            if (is.null(names_FUN.VALUE))
                names_FUN.VALUE <- dimnames(res[[1]])
        }
    }
    res <- unlist(res, use.names = FALSE)
    if (is.null(res))
        res <- vector(mode = type, length = 0L)
    if (!is.null(dim_res))
        dim(res) <- dim_res
    if (USE.NAMES) {
        if (is.array(res)) {
            n_dim <- length(dim(res))
            dimnames <- vector("list", length = n_dim)
            if (is.null(dim)) {
                names <- names(X)
                if (!is.null(names))
                  dimnames[[2]] <- names
                names <- names_FUN.VALUE
                if (!is.null(names))
                  dimnames[[1]] <- names
            } else {
                names <- names(X)
                if (!is.null(names))
                  dimnames[[n_dim]] <- names
                names <- names_FUN.VALUE
                if (!is.null(names))
                  dimnames[-n_dim] <- names
            }
            if (!all(unlist(lapply(dimnames, FUN = is.null),
                use.names = FALSE))) {
                dimnames(res) <- dimnames
            }
        }  else {
            names(res) <- names(X)
        }
    }
    res
}


# require(stats); require(graphics)
# i39 <- sapply(3:9, seq) # list of vectors
# sapply(i39, fivenum)
# vapply(i39, fivenum,
#        c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))
# pbvapply(i39, fivenum,
#        c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

# ## sapply(*, "array") -- artificial example
# (v <- structure(10*(5:8), names = LETTERS[1:4]))
# f2 <- function(x, y) outer(rep(x, length.out = 3), y)
# (a2 <- sapply(v, f2, y = 2*(1:5), simplify = "array"))
# a.2 <- vapply(v, f2, outer(1:3, 1:5), y = 2*(1:5))
# stopifnot(dim(a2) == c(3,5,4), all.equal(a2, a.2),
#           identical(dimnames(a2), list(NULL,NULL,LETTERS[1:4])))
# a.2 <- pbvapply(v, f2, outer(1:3, 1:5), y = 2*(1:5))
# stopifnot(dim(a2) == c(3,5,4), all.equal(a2, a.2),
#           identical(dimnames(a2), list(NULL,NULL,LETTERS[1:4])))
