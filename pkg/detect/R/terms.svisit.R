terms.svisit <-
function (x, model = c("sta", "det"), ...)
{
    x$terms[[match.arg(model)]]
}

