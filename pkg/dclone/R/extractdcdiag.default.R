extractdcdiag.default <- 
function(x, ...)
{
    chisq <- chisq.diag(x)$statistics
    c(lambda.max=lambdamax.diag(x),
        chisq.error=chisq$error, 
        chisq.cor=chisq$cor)
}
