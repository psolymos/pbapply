## this returns some TS related indices
ts_index <- 
function(x)
{
    i <- which(is.na(x))
    j <- setdiff(i+1, i)
    as.integer(j[j <= length(x)])
}
