variable.names.svisit <-
function (object, model = c("full", "sta", "det"), ...) 
{
    names(coef(object, match.arg(model)))
}

