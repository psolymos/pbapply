## this function is needed, because R2WinBUGS::openbugs has no argument for random seed
## and random seed is required for parallel computations
openbugs.seed <- 
function (data, inits, parameters.to.save, model.file = "model.txt", 
    n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2), 
    n.thin = max(1, floor(n.chains * (n.iter - n.burnin)/n.sims)), 
    n.sims = 1000, DIC = TRUE, bugs.directory = "c:/Program Files/OpenBUGS/", 
    working.directory = NULL, digits = 5, bugs.seed = NULL) 
{
    if (!is.R()) 
        stop("OpenBUGS is not yet available in S-PLUS")
    if (!require("BRugs")) 
        stop("BRugs is required")
    modelFile <- model.file
    numChains <- n.chains
    nBurnin <- n.burnin
    nIter <- n.iter - n.burnin
    nThin <- n.thin
    if (DIC) 
        parameters.to.save <- c(parameters.to.save, "deviance")
    parametersToSave <- parameters.to.save
    if (is.null(working.directory)) {
        working.directory <- tempdir()
    }
    savedWD <- getwd()
    setwd(working.directory)
    on.exit(setwd(savedWD))
    if (!file.exists(modelFile)) {
        stop(modelFile, " does not exist")
    }
    if (file.info(modelFile)$isdir) {
        stop(modelFile, " is a directory, but a file is required")
    }
    if (!length(grep("\r\n", readChar(modelFile, 10^3)))) {
        message("Carriage returns added to model file ", modelFile)
        model <- readLines(modelFile)
        try(writeLines(model, modelFile))
    }
    BRugs::modelCheck(modelFile)
    if (!(is.vector(data) && is.character(data) && all(file.exists(data)))) {
        data <- BRugs::bugsData(data, digits = digits)
    }
    BRugs::modelData(data)
    BRugs::modelCompile(numChains)
    if (missing(inits) || is.null(inits)) {
        BRugs::modelGenInits()
    }
    else {
        if (is.list(inits) || is.function(inits) || (is.character(inits) && 
            !any(file.exists(inits)))) {
            inits <- BRugs::bugsInits(inits = inits, numChains = numChains, 
                digits = digits)
        }
        BRugs::modelInits(inits)
        BRugs::modelGenInits()
    }

    ## code for RNG seed added -- start
    if (!is.null(bugs.seed)) {
        BRugs::modelSetSeed(bugs.seed)
        if (getOption("BRugsVerbose")) {
            cat("RNG seed:", bugs.seed, "\n")
            flush.console()
        }
    }
    ## code for RNG seed added -- end

    BRugs::samplesSetThin(nThin)
    adaptivelines <- scan(system.file("OpenBUGS", "Bugs", "Rsrc", 
        "Registry.txt", package = "BRugs"), what = "character", 
        quiet = TRUE)
    factories <- sub(".adaptivePhase", "", adaptivelines[grep("adaptivePhase", 
        adaptivelines)])
    sapply(factories, BRugs::modelSetAP, max(0, nBurnin - 1))
    BRugs::modelUpdate(nBurnin)
    if (DIC) {
        BRugs::dicSet()
        on.exit(BRugs::dicClear(), add = TRUE)
    }
    BRugs::samplesSet(parametersToSave)
    BRugs::modelUpdate(nIter)
    params <- R2WinBUGS:::sort.name(BRugs::samplesMonitors("*"), parametersToSave)
    samples <- sapply(params, BRugs::samplesSample)
    n.saved.per.chain <- nrow(samples)/numChains
    samples.array <- array(samples, c(n.saved.per.chain, numChains, 
        ncol(samples)))
    dimnames(samples.array)[[3]] <- dimnames(samples)[[2]]
    if (DIC) {
        DICOutput <- BRugs::dicStats()
    }
    else {
        DICOutput <- NULL
    }
    bugs.output <- as.bugs.array(sims.array = samples.array, 
        model.file = modelFile, program = "OpenBUGS", DIC = DIC, 
        DICOutput = DICOutput, n.iter = n.iter, n.burnin = n.burnin, 
        n.thin = n.thin)
    bugs.output
}
