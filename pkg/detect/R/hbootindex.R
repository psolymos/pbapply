hbootindex <- 
function(groups, strata, B = 199) 
{
    groups <- as.integer(as.factor(as.integer(groups)))
    n <- length(groups)
    if (missing(strata)) {
        strata <- rep(1, n)
    } else {
        strata <- as.integer(as.factor(as.integer(strata)))
    }
    if (length(strata) != length(groups))
        stop("lengths must equal")
    n <- length(groups)
    ni <- seq_len(n)
    ng <- length(unique(groups))
    ns <- length(unique(strata))
    nsj <- seq_len(ns)
    xyclboot <- function(i) {
        if (i==1)
            return(sample.int(n, n, replace=FALSE)) ## randomize order
        out <- list()
        for (j in nsj) {
            g0 <- unique(groups[strata==j])
            g <- sample(g0, length(g0), replace=TRUE)
            out[[j]] <- unlist(lapply(g, function(z)
                sample(ni[groups==z], sum(groups==z), replace=TRUE)))
        }
        out <- unlist(out)
        sample(out, length(out), replace=FALSE)
    }
    ## length can be different for unequal groups size
    if (require(pbapply)) {
        pblapply(1:(B+1), xyclboot)
    } else {
        lapply(1:(B+1), xyclboot)
    }
}
