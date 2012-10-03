## simplified version of dmultinom
logdmultinom <- 
function (x, size, prob) 
{
    lgamma(size + 1) + sum(x * log(prob) - lgamma(x + 1))
}
