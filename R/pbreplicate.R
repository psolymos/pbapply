pbreplicate <-
function (n, expr, simplify = TRUE) 
pbsapply(integer(n), eval.parent(substitute(function(...) expr)), 
    simplify = simplify)

