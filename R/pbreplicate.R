pbreplicate <-
function (n, expr, simplify = "array", ..., cl = NULL)
pbsapply(integer(n), eval.parent(substitute(function(...) expr)),
    simplify = simplify, ..., cl = cl)
