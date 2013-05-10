dctr <-
function(x)
{
    class(x) <- c("dctr", class(x))
    x
}
