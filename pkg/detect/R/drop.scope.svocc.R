drop.scope.svisit <-
function (terms1, terms2, model = c("sta", "det")) 
{
    model <- match.arg(model)
    terms1 <- terms(terms1, model)
    f2 <- if (missing(terms2)) 
        numeric(0)
    else attr(terms(terms2, model), "factors")
    factor.scope(attr(terms1, "factors"), list(drop = f2))$drop
}

