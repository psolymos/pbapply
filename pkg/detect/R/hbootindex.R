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
    ni <- seq_len(n)
    ns <- length(unique(strata))
    nsj <- seq_len(ns)
    xyclboot <- function(i) {
        if (i==1)
            return(sample.int(n, n, replace=FALSE)) ## randomize order
        out <- vector("list", ns)
        for (j in nsj) {
            g <- groups[strata==j]
            g0 <- unique(g)
            w0 <- table(g)
            w <- w0[match(g, names(w0))]
            w <- w / sum(w)
            out[[j]] <- sample(ni[strata==j], length(g), 
                replace=TRUE, prob=w)
        }
        out <- unlist(out)
        sample(out, length(out), replace=FALSE)
    }
    if (require(pbapply)) {
        pbsapply(1:(B+1), xyclboot)
    } else {
        sapply(1:(B+1), xyclboot)
    }
}
