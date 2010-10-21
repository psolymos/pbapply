dcoptions <-
function(...)
{
    opar <- getOption("dcoptions")
    args <- list(...)
    if (length(args)) {
        if (length(args)==1 && is.list(args[[1]])) {
            npar <- args[[1]]
        } else {
            npar <- opar
            npar[match(names(args), names(npar))] <- args
        }
        options("dcoptions"=npar)
    }
    invisible(opar)
}
