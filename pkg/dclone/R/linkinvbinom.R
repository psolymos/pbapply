linkinvbinom <-
function(link)
{
    link <- match.arg(link, c("logit", "probit", "loglog", "cloglog"))
    switch(link,
        "logit" = {make.link("logit")$linkinv},
        "probit" = {make.link("probit")$linkinv},
        "loglog" = {function(eta) exp(-exp(-eta))},
        "cloglog" = {make.link("cloglog")$linkinv})
}
