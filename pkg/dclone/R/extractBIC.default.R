extractBIC.default <-
function(fit, ...) {
    extractAIC(fit, k=log(nobservations(fit)), ...)
}
