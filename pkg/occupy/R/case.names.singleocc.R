case.names.singleocc <-
function (object, ...)
{
#    w <- weights(object)
    dn <- rownames(model.matrix(object))
#    if (full || is.null(w))
        dn
#    else dn[w != 0]
}
