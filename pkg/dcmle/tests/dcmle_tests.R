## tests

library(dcmle)

## data type classes

as(new("gsFit"), "dcFit")
as(new("dcFit"), "gsFit")

## dcmle model fit classes

str(as(regmod, "MCMClist"))
str(as(regmod, "codaMCMC"))
str(as(regmod, "dcCodaMCMC"))
str(as(regmod, "dcmle"))

str(as(as(regmod, "MCMClist"), "MCMClist"))
str(as(as(regmod, "MCMClist"), "codaMCMC"))
str(as(as(regmod, "MCMClist"), "dcCodaMCMC"))
str(as(as(regmod, "MCMClist"), "dcmle"))

str(as(as(regmod, "codaMCMC"), "MCMClist"))
str(as(as(regmod, "codaMCMC"), "codaMCMC"))
str(as(as(regmod, "codaMCMC"), "dcCodaMCMC"))
str(as(as(regmod, "codaMCMC"), "dcmle"))

str(as(as(regmod, "dcCodaMCMC"), "MCMClist"))
str(as(as(regmod, "dcCodaMCMC"), "codaMCMC"))
str(as(as(regmod, "dcCodaMCMC"), "dcCodaMCMC"))
str(as(as(regmod, "dcCodaMCMC"), "dcmle"))

str(as.mcmc.list(regmod, "MCMClist"))
str(as.mcmc.list(regmod, "codaMCMC"))
str(as.mcmc.list(regmod, "dcCodaMCMC"))
str(as.mcmc.list(regmod, "dcmle"))

## testing plot methods

regmod_MCMClist <- as(regmod, "MCMClist")
regmod_codaMCMC <- as(regmod, "codaMCMC")
regmod_dcCodaMCMC <- as(regmod, "dcCodaMCMC")
regmod_dcmle <- as(regmod, "dcmle")

evalfun <- function(FUN) {
    evalfun_int <- function(x, FUN) {
        eval(parse(text=
          paste(FUN, "(", x, ")", sep="")
          ))
    }
    cat("\n\n***", FUN, "***")
    cat("\n\n---", FUN, "--- MCMClist ---\n")
    ooo <- try(evalfun_int("regmod_MCMClist", FUN))
    if (inherits(ooo, ""))
        ooo else cat("OK\n\n")
    cat("\n\n---", FUN, "--- codaMCMC ---\n")
    ooo <- try(evalfun_int("regmod_codaMCMC", FUN))
    if (inherits(ooo, ""))
        ooo else cat("OK\n\n")
    cat("\n\n---", FUN, "--- dcCodaMCMC ---\n")
    ooo <- try(evalfun_int("regmod_dcCodaMCMC", FUN))
    if (inherits(ooo, ""))
        ooo else cat("OK\n\n")
    cat("\n\n---", FUN, "--- dcmle ---\n")
    ooo <- try(evalfun_int("regmod_dcmle", FUN))
    if (inherits(ooo, ""))
        ooo else cat("OK\n\n")
}
#evalfun("str")

toEval <- c("plot",
  "traceplot",
  "densplot",
  "pairs",
  "densityplot",
  "qqmath",
  "xyplot",
  "acfplot",
  "crosscorr.plot",
  "dcdiag",
  "dctable",
  "nclones",
  "dcsd",
  "as.matrix",
  "as.array",
  "nvar",
  "varnames",
  "chanames",
  "nchain",
  "niter",
  "crosscorr",
  "mcpar",
  "thin",
  "coef",
  "vcov",
#  "confint",
  "quantile",
  "start",
  "end",
  "frequency",
  "time",
  "window",
  "stack",
  "str",
  "head",
  "tail",
  "autocorr.diag",
  "lambdamax.diag",
  "chisq.diag",
  "gelman.diag",
  "geweke.diag",
  "raftery.diag",
  "heidel.diag")

for (i in toEval)
  evalfun(i)

evalfun("confint")
attr(regmod, "n.clones") <- 2
regmod_MCMClist <- as(regmod, "MCMClist")
regmod_codaMCMC <- as(regmod, "codaMCMC")
regmod_dcCodaMCMC <- as(regmod, "dcCodaMCMC")
regmod_dcmle <- as(regmod, "dcmle")
evalfun("confint")

## testing stats/coda methods

#tmp <- mcmc(cbind(z=c(1,1,1,1), a=c(0,0,0,0), q2=c(-1,-1,-1,-1)),
#    start=101, end=107, thin=2)
#x <- as.mcmc.list(list(tmp, tmp+0.5, tmp-0.5))
#rm(tmp)

## update
## show, summary
## [, [[


