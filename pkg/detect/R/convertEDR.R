convertEDR <- 
function(edr, r, truncated=FALSE) 
{
    ## return unlimited EDR
    if (truncated) {
        if (r < edr)
            stop("r must not be smaller than edr")
        f <- function(edr, edr_r, r) {
            edr*sqrt(1-exp(-r^2/edr^2)) - edr_r
        }
        out <- uniroot(f, c(edr, 10*edr), edr_r=edr, r=r)$root
    ## return truncated EDR
    } else {
        out <- edr*sqrt(1-exp(-r^2/edr^2))
    }
    out
}
