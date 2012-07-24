## this returns some TS related indices
ts_index <- 
function(x)
{
    i <- which(is.na(x))
    as.integer(setdiff(i[-length(i)]+1, i))
}
