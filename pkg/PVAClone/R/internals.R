## this returns some TS related indices
ts_index <- 
function(x, type=c("density", "expectation"))
{
    type <- match.arg(type)
    if (type == "density") {
        i <- which(is.na(c(NA,x,NA)))
        j <- unlist(apply(cbind(i[-length(i)], i[-1]), 1, 
            FUN = function (z) {
                if (diff(z) < 3)
                    numeric(0) else (z[1]+1):(z[2]-1)
            })) - 1
    } else {
        i <- which(is.na(x))
        j <- setdiff(i+1, i)
    }
    as.integer(j[j <= length(x)])
}

