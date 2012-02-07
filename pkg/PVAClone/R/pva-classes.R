## this declares S4 class pvamodel
setClass("pvamodel", 
    representation(
        growth.model="character", 
        obs.error="character",
        model="dcModel",
        predmodel="dcModel",
        p="integer",
        support="matrix",
        params="character",
        varnames="character",
        fixed="nClones",
        fancy="character",
#        transf="function",      # original --> diagn
        backtransf="function",  # diagn --> original
        logdensity="function",
        neffective="function"))

## this declares inheritance and extension for 'pva' S4 class
setClass("pva", 
    representation(
        observations="numeric", 
        nobs="integer",
        model="pvamodel",
        dcdata="dcFit", 
        vcov="matrix"),
    contains = c("dcMle"))
