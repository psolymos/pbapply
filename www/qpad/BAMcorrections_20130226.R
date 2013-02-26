## -- BAMcorrections -------------------------------
## (Febr 2013)
cat("BAM QPAD access functions loaded, version 20130226\n")

## utility functions for accessing BAMcorrections from BAMCOEFS

## get version
getBAMversion <- function() {
    get("version", envir=.BAMCOEFS)
}

## get species list
getBAMspecieslist <- function() {
    get("spp", envir=.BAMCOEFS)
}
#getBAMspecieslist()

## get model list
getBAMmodellist <- function() {
    list(sra=get("sra_list", envir=.BAMCOEFS),
        edr=get("edr_list", envir=.BAMCOEFS))
}
#getBAMmodellist()

getBAMspeciestable <- function() {
    get("spp_table", envir=.BAMCOEFS)
}
#getBAMspeciestable()

## check if coefs are available for a species
checkBAMspecies <- function(species) {
    species %in% getBAMspecieslist()
}
#checkBAMspecies("OVEN")

## get coefs for a species
getBAMspecies <- function(species) {
    if (!checkBAMspecies(species))
        stop("Estimates for species ", species, " not found")
    list(edr=as.list(.BAMCOEFS)$edr_estimates[[species]],
        sra=as.list(.BAMCOEFS)$sra_estimates[[species]])
}
#getBAMspecies("OVEN")
#getBAMspecies("ABCD") # gives an error

## check if model combination can be found
checkBAMspeciesmodel <- function(species, model.sra=0, model.edr=0) {
    model.sra <- match.arg(as.character(model.sra), names(get("sra_df", envir=.BAMCOEFS)))
    model.edr <- match.arg(as.character(model.edr), names(get("edr_df", envir=.BAMCOEFS)))
    mp <- as.logical(get("sra_models", envir=.BAMCOEFS)[species, model.sra])
    mq <- as.logical(get("edr_models", envir=.BAMCOEFS)[species, model.edr])
    if (!mp)
        stop("model.sra", model.sra, "not found for species", species)
    if (!mq)
        stop("model.edr", model.edr, "not found for species", species)
    list(model.sra=model.sra, model.edr=model.edr)
}

## get coef for p/q model
coefBAMspecies <- function(species, model.sra=0, model.edr=0) {
    tmp <- checkBAMspeciesmodel(species, model.sra, model.edr)
    model.sra <- tmp$model.sra
    model.edr <- tmp$model.edr
    x <- getBAMspecies(species)
    sra <- x$sra[[model.sra]]$coef
    edr <- x$edr[[model.edr]]$coef
    names(sra) <- x$sra[[model.sra]]$names
    names(edr) <- x$edr[[model.edr]]$names
    out <- list(sra=sra, edr=edr)
    attr(out, "species") <- species
    attr(out, "model.sra") <- as.integer(model.sra)
    attr(out, "model.edr") <- as.integer(model.edr)
    out
}
#coefBAMspecies("OVEN")
#coefBAMspecies("OVEN", 8, 1)

vcovBAMspecies <- function(species, model.sra=0, model.edr=0) {
    tmp <- checkBAMspeciesmodel(species, model.sra, model.edr)
    model.sra <- tmp$model.sra
    model.edr <- tmp$model.edr
    x <- getBAMspecies(species)
    sra <- x$sra[[model.sra]]$vcov
    edr <- x$edr[[model.edr]]$vcov
    dimnames(sra) <- list(x$sra[[model.sra]]$names, x$sra[[model.sra]]$names)
    dimnames(edr) <- list(x$edr[[model.edr]]$names, x$edr[[model.edr]]$names)
    out <- list(sra=sra, edr=edr)
    attr(out, "species") <- species
    attr(out, "model.sra") <- as.integer(model.sra)
    attr(out, "model.edr") <- as.integer(model.edr)
    out
}
#vcovBAMspecies("OVEN")
#vcovBAMspecies("OVEN", "8", "1")

## get summary for p/q model
summaryBAMspecies <- 
function(species, model.sra, model.edr) {
    if (missing(model.sra))
        model.sra <- bestmodelBAMspecies(species)$sra
    if (missing(model.edr))
        model.edr <- bestmodelBAMspecies(species)$edr
    COEF <- coefBAMspecies(species, model.sra, model.edr)
    VCOV <- vcovBAMspecies(species, model.sra, model.edr)
    sra <- COEF$sra
    edr <- COEF$edr
    coefmat <- cbind(c(sra, edr), 
        c(sqrt(diag(VCOV$sra)), sqrt(diag(VCOV$edr))))
    rownames(coefmat) <- c(paste("sra_", names(sra), sep=""),
        paste("edr_", names(edr), sep=""))
    colnames(coefmat) <- c("Estimate","Std. Error")
    attr(coefmat, "species") <- species
    attr(coefmat, "model.sra") <- as.integer(model.sra)
    attr(coefmat, "model.edr") <- as.integer(model.edr)
    class(coefmat) <- c("summaryBAMspecies", "matrix")
    coefmat
}

## print method for BAMcoef
print.summaryBAMspecies <- function(x, ...) {
    cat("BAMcorrection object for species", 
        attr(x, "species"), "\n")
    cat("model.sra =", attr(x, "model.sra"),
        "\nmodel.edr =", attr(x, "model.edr"), "\n")
    printCoefmat(x, 
        digits = max(3, getOption("digits") - 3),
        signif.stars = FALSE, na.print = "NA", ...)
    invisible(x)
}
#summaryBAMspecies("OVEN")
#unclass(summaryBAMspecies("OVEN"))
#summaryBAMspecies("OVEN", "8", "1")

## AIC/BIC based model selection
selectmodelBAMspecies <-
function(species, model.sra, model.edr) {
    l1 <- get("sra_loglik", envir=.BAMCOEFS)[species,]
    l2 <- get("edr_loglik", envir=.BAMCOEFS)[species,]
    sra <- data.frame(model=names(l1),
        logLik=l1,
        df=get("sra_df", envir=.BAMCOEFS),
        nobs=get("sra_n", envir=.BAMCOEFS)[species],
        AIC=get("sra_aic", envir=.BAMCOEFS)[species,],
        BIC=get("sra_bic", envir=.BAMCOEFS)[species,])
    edr <- data.frame(model=names(l2),
        logLik=l2,
        df=get("edr_df", envir=.BAMCOEFS),
        nobs=get("edr_n", envir=.BAMCOEFS)[species],
        AIC=get("edr_aic", envir=.BAMCOEFS)[species,],
        BIC=get("edr_bic", envir=.BAMCOEFS)[species,])
    if (missing(model.sra))
        model.sra <- rownames(sra)
    if (missing(model.edr))
        model.edr <- rownames(edr)
    out <- list(sra=sra[as.character(model.sra),],
        edr=edr[as.character(model.edr),])
    out$sra$dAIC <- out$sra$AIC-min(out$sra$AIC)
    out$edr$dAIC <- out$edr$AIC-min(out$edr$AIC)
    out$sra$dBIC <- out$sra$BIC-min(out$sra$BIC)
    out$edr$dBIC <- out$edr$BIC-min(out$edr$BIC)
    out
}
#selectmodelBAMspecies("OVEN")

## model ID for best supported model
bestmodelBAMspecies <-
function(species, model.sra, model.edr, type=c("AIC", "BIC")) {
    type <- match.arg(type)
    x <- selectmodelBAMspecies(species, model.sra, model.edr)
    out <- if (type=="AIC") {
        list(sra=as.character(x$sra$model[which.min(x$sra$AIC)]),
            edr=as.character(x$edr$model[which.min(x$edr$AIC)]))
    } else {
        list(sra=as.character(x$sra$model[which.min(x$sra$BIC)]),
            edr=as.character(x$edr$model[which.min(x$edr$BIC)]))
    }
    out
}
#bestmodelBAMspecies("OVEN")

edr_fun <- function(r, sigma) {
    sigma^2*(1-exp(-r^2/sigma^2))/r^2
}
sra_fun <- function(t, phi) {
    1-exp(-t*phi)
}

## this makes GLOBAL correction data frame
## species = 4 letter acronym for species
## r = point count radius in 100 m (1 unit is 100 m)
## t = point count duration in min
## labels = vector for rownames of output
## boot = logical, the point estimates are used if FALSE,
##        a parametric bootstrap version is used when TRUE
##        (note: this gives only one iteration per call)
##        TRUE will create one random draw from the Normal distr
globalBAMcorrections <- 
function(species, r, t, labels, boot=FALSE, ...)
{
    SPP <- coefBAMspecies(species, 0, 0)
    VCV <- vcovBAMspecies(species, 0, 0)
    qfun <- edr_fun
    pfun <- sra_fun
#    qfun <- function(rr, sigma) {
#        sigma^2*(1-exp(-rr^2/sigma^2))/rr^2
#    }
#    pfun <- function(tt, phi) {
#        1-exp(-tt*phi)
#    }
    phi0 <- list(...)$phi
    sigma0 <- list(...)$sigma
    if (is.null(phi0)) {
        model.sra <- 0
        if (boot) {
            phi <- exp(rnorm(1, SPP$sra, sqrt(VCV$sra[1,1])))
        } else {
            phi <- exp(SPP$sra)
        }
    } else {
        model.sra <- NA
        phi <- phi0
    }
    names(phi) <- NULL
    if (is.null(sigma0)) {
        model.edr <- 0
        if (boot) {
            sigma <- exp(rnorm(1, SPP$edr, sqrt(VCV$edr[1,1])))
        } else {
            sigma <- exp(SPP$edr)
        }
    } else {
        model.edr <- NA
        sigma <- sigma0
    }
    names(sigma) <- NULL
    t.nona <- !is.na(t)
    r.nona <- !is.na(r)
    tt <- t[t.nona]
    rr <- r[r.nona]
    unlim <- ifelse(rr==Inf, TRUE, FALSE)
    A <- q <- r
    p <- t
    A[r.nona] <- ifelse(unlim, pi*sigma^2, pi*rr^2)
    p[t.nona] <- pfun(tt, phi)
    q[r.nona] <- ifelse(unlim, 1, qfun(rr, sigma))
    out <- data.frame(A, p, q)
    if (!missing(labels))
        rownames(out) <- labels
    class(out) <- c("globalBAMcorrections", "BAMcorrections", "data.frame")
    attr(out, "species") <- species
    attr(out, "model.sra") <- as.integer(model.sra)
    attr(out, "model.edr") <- as.integer(model.edr)
    out
}
#t <- c(3,3,3,5,5,5,10,10,10,3,NA)
#r <- c(0.5,1,Inf,0.5,1,Inf,0.5,1,Inf,NA,1)
#x <- globalBAMcorrections("OVEN", r, t, labels=LETTERS[1:11])
#x
#globalBAMcorrections("OVEN", r, t, labels=LETTERS[1:11], phi=1)

## creates offset
corrections2offset <- function(x, link="log", na.rm=FALSE) {
    link <- match.arg(link, c("log"))
    linkfun <- switch(link,
        "log"=log)
    if (!inherits(x, "BAMcorrections"))
        stop("not BAMcorrections object")
    out <- rowSums(linkfun(x), na.rm=na.rm)
    attr(out, "species") <- attr(x, "species")
    attr(out, "model.sra") <- attr(x, "model.sra")
    attr(out, "model.edr") <- attr(x, "model.edr")
    attr(out, "link") <- link
    out
}
#rowSums(log(globalBAMcorrections("OVEN", r, t, labels=LETTERS[1:11])), na.rm=TRUE)
#corrections2offset(globalBAMcorrections("OVEN", r, t, labels=LETTERS[1:11]), na.rm=TRUE)

## correction factor for density
corrections <- function(x, na.rm=FALSE) {
    out <- apply(x, 1, prod, na.rm=na.rm)
    attr(out, "species") <- attr(x, "species")
    attr(out, "model.sra") <- attr(x, "model.sra")
    attr(out, "model.edr") <- attr(x, "model.edr")
    out
}
#corrections(x)
#log(corrections(x))

## this makes LOCAL correction data frame
## species = 4 letter acronym for species
## r = point count radius in m/100
## t = point count duration in min
## labels = vector for rownames of output
## jday = time since local spring (days/365, numeric)
## tssr = time since local sunrise (hours/24, numeric)
## tree = tree cover (0-1 scale, based on VCF)
## lcc = land cover class (5 levels)
## road = on/off-road (0=off-road, 1=on-road, integer)
## model.sra = model for singing rate
## model.edr = model for EDR
## ver selects different sets of covariates
## boot = logical, the point estimates are used if FALSE,
##        a parametric bootstrap version is used when TRUE
##        (note: this gives only one iteration per call)
##        TRUE will create one random draw from the MVN

## note: road effect is still experimental

localBAMcorrections <- 
function(species, r, t, 
jday, tssr, tree, lcc, road,
model.sra=0, model.edr=0, labels, ver=1, boot=FALSE, ...)
{
    if (boot)
        require(MASS)
    SPP <- coefBAMspecies(species, model.sra, model.edr)
    VCV <- vcovBAMspecies(species, model.sra, model.edr)
    model.sra <- as.character(model.sra)
    model.edr <- as.character(model.edr)
    qfun <- edr_fun
    pfun <- sra_fun
#    qfun <- function(rr, sigma) {
#        sigma^2*(1-exp(-rr^2/sigma^2))/rr^2
#    }
#    pfun <- function(tt, phi) {
#        1-exp(-tt*phi)
#    }
    phi0 <- list(...)$phi
    sigma0 <- list(...)$sigma
    ## sra
    if (is.null(phi0)) {
        ltssr <- names(getBAMmodellist()$sra)[grep("TSSR", getBAMmodellist()$sra)]
        ljday <- names(getBAMmodellist()$sra)[grep("jday", getBAMmodellist()$sra)]
        if (missing(tssr) && model.sra %in% ltssr)
            stop("tssr must be specified for model.sra", model.sra)
        if (missing(jday) && model.sra %in% ljday)
            stop("jday must be specified for model.sra", model.sra)
        Xt <- switch(model.sra,
            "0"=matrix(1,length(t),1),
            "1"=model.matrix(~jday),
            "2"=model.matrix(~tssr),
            "3"=model.matrix(~jday + I(jday^2)),
            "4"=model.matrix(~tssr + I(tssr^2)),
            "5"=model.matrix(~jday + tssr),
            "6"=model.matrix(~jday + I(jday^2) + tssr),
            "7"=model.matrix(~jday + tssr + I(tssr^2)),
            "15"=model.matrix(~jday + tssr + I(tssr^2)),
            "8"=model.matrix(~jday + I(jday^2) + tssr + I(tssr^2)))
        if (boot) {
            tmp <- mvrnorm(1, SPP$sra, VCV$sra)
            phi <- exp(drop(Xt %*% tmp))
        } else {
            phi <- exp(drop(Xt %*% SPP$sra))
        }
        if (missing(jday) && missing(tssr))
            t.nona <- !is.na(t)
        if (missing(jday) && !missing(tssr))
            t.nona <- !is.na(t) & !is.na(tssr)
        if (!missing(jday) && missing(tssr))
            t.nona <- !is.na(t) & !is.na(jday)
        if (!missing(jday) && !missing(tssr))
            t.nona <- !is.na(t) & !is.na(jday) & !is.na(tssr)
    } else {
        model.sra <- NA
        phi <- phi0
        t.nona <- !is.na(t)
        if (length(phi)==1)
            phi <- rep(phi, length(t))
    }
    phi <- phi[t.nona]
    names(phi) <- NULL
    ## edr
    if (is.null(sigma0)) {
        ltree <- names(getBAMmodellist()$edr)[grep("TREE", getBAMmodellist()$edr)]
        lroad <- names(getBAMmodellist()$edr)[grep("ROAD", getBAMmodellist()$edr)]
        if (missing(tree) && model.edr %in% ltree)
            stop("tree must be specified for model.edr", model.edr)
        if (missing(road) && model.edr %in% lroad)
            stop("road must be specified for model.edr", model.edr)
        if (ver==2) {
            Xr <- switch(model.edr,
                "0"=matrix(1,length(r),1),
                "1"=model.matrix(~tree),
                "2"=model.matrix(~lcc),
                "3"=model.matrix(~road),
                "4"=model.matrix(~tree + road),
                "5"=model.matrix(~lcc + road))
        }
        if (ver==1) {
            Xr <- switch(model.edr,
                "0"=matrix(1,length(r),1),
                "1"=model.matrix(~tree),
                "2"=model.matrix(~lcc))
        }
        if (boot) {
            tmp <- mvrnorm(1, SPP$edr, VCV$edr)
            sigma <- exp(drop(Xr %*% tmp))
        } else {
            sigma <- exp(drop(Xr %*% SPP$edr))
        }
        if (missing(tree) && missing(road))
            r.nona <- !is.na(r)
        if (missing(tree) && !missing(road))
            r.nona <- !is.na(r) & !is.na(road)
        if (!missing(tree) && missing(road))
            r.nona <- !is.na(r) & !is.na(tree)
        if (!missing(tree) && !missing(road))
            r.nona <- !is.na(r) & !is.na(tree) & !is.na(road)
    } else {
        model.edr <- NA
        sigma <- sigma0
        r.nona <- !is.na(r)
        if (length(sigma)==1)
            sigma <- rep(sigma, length(r))
    }
    sigma <- sigma[r.nona]
    names(sigma) <- NULL
    ## output
    tt <- t[t.nona]
    rr <- r[r.nona]
    unlim <- ifelse(rr==Inf, TRUE, FALSE)
    A <- q <- r
    p <- t
    A[r.nona] <- ifelse(unlim, pi*sigma^2, pi*rr^2)
    p[t.nona] <- pfun(tt, phi)
    q[r.nona] <- ifelse(unlim, 1, qfun(rr, sigma))
    out <- data.frame(A, p, q)
    if (!missing(labels))
        rownames(out) <- labels
    class(out) <- c("localBAMcorrections", "BAMcorrections", "data.frame")
    attr(out, "species") <- species
    attr(out, "model.sra") <- as.integer(model.sra)
    attr(out, "model.edr") <- as.integer(model.edr)
    out
}
