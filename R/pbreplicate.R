pbreplicate <-
function (n, expr, simplify = "array") 
pbsapply(integer(n), eval.parent(substitute(function(...) expr)), 
    simplify = simplify)
