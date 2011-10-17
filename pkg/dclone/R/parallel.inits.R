parallel.inits <- 
function(inits, n.chains) 
{
    factory <- if ("lecuyer" %in% list.modules())
        "lecuyer::RngStream" else "base::BaseRNG"
    RNGs <- parallel.seeds(factory, n.chains)
    if (!is.null(inits)) {
        checkParameters <- function(inits) {
            if(!is.list(inits))
                return (FALSE)
            inames <- names(inits)
            if (is.null(inames) || any(nchar(inames) == 0))
                return (FALSE)
            if (any(duplicated(inames)))
                return (FALSE)
            if (any(inames==".RNG.name")) {
                rngname <- inits[[".RNG.name"]]
                if (!is.character(rngname) || length(rngname) != 1)
                    return (FALSE)
                inits[[".RNG.name"]] <- NULL
            }
            ## Strip null initial values, but give a warning
            null.inits <- sapply(inits, is.null)
            if (any(null.inits)) {
                warning("NULL initial values supplied for variable",
                        paste(inames[null.inits], sep=","))
                inits <- inits[!null.inits]
            }
            if (!all(sapply(inits, is.numeric)))
                return (FALSE)
            return (TRUE)
        }
        init.values <- vector("list", n.chains)
        if (is.function(inits)) {
            if (any(names(formals(inits)) == "chain")) {
                for (i in 1:n.chains) {
                    init.values[[i]] <- inits(chain=i)
                }
            }
            else {
                for (i in 1:n.chains) {
                    init.values[[i]] <- inits()
                }
            }
        }
        else if (is.list(inits)) {
            if (checkParameters(inits)) {
                ## Replicate initial values for all chains
                for (i in 1:n.chains) {
                    init.values[[i]] <- inits
                }
            }
            else if (!all(sapply(inits, checkParameters))) {
                stop("Invalid initial values")
            }
            else {
                if (length(inits) != n.chains) {
                    stop("Length mismatch in inits")
                }
                init.values <- inits
            }
        }
        for (i in 1:n.chains) {
            init.values[[i]]$.RNG.state <- RNGs[[i]]$.RNG.state
            init.values[[i]]$.RNG.name <- RNGs[[i]]$.RNG.name
        }
    } else init.values <- RNGs
    init.values
}
