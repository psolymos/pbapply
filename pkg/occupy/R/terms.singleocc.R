`terms.singleocc` <-
function (x, model = c("occ", "det"), ...)
{
    x$terms[[match.arg(model)]]
}

