library(dcmle)
pigs <- makeDcFit(
    data = list(
        "p.mendelian" =
            structure(c(1, 0.5, 0, 0.5, 0.25, 0, 0, 0, 0, 0, 0.5, 1, 0.5, 
            0.5, 0.5, 1, 0.5, 0, 0, 0, 0, 0, 0.25, 0.5, 0, 0.5, 1), .Dim = as.integer(c(3, 
            3, 3))),
        "p.recessive" =
            structure(c(1, 1, 0, 0, 0, 1), .Dim = as.integer(c(3, 2))),
        "A1" = 1,
        "B1" = 1,
        "C1" = 1,
        "D1" = 1,
        "E1" = 1,
        "F1" = 1,
        "G1" = 1,
        "H1" = 1,
        "J1" = 2),
    model = function() {
       q ~ dunif(0,1);                            # prevalence of a1
       p <- 1 - q;                                # prevalence of a2
       Ann1   ~ dbin(q,2);   Ann <- Ann1 + 1;     # geno. dist. for founder
       Brian1 ~ dbin(q,2);   Brian <- Brian1 + 1;
       Clare  ~ dcat(p.mendelian[Ann,Brian,]);    # geno. dist. for child
       Diane  ~ dcat(p.mendelian[Ann,Brian,]);
       Eric1  ~ dbin(q,2);   Eric <- Eric1 + 1;
       Fred   ~ dcat(p.mendelian[Diane,Eric,]);
       Gene   ~ dcat(p.mendelian[Diane,Eric,]);
       Henry1 ~ dbin(q,2);   Henry <- Henry1 + 1;
       Ian    ~ dcat(p.mendelian[Clare,Fred,]);
       Jane   ~ dcat(p.mendelian[Gene,Henry,]);
       A1 ~ dcat(p.recessive[Ann,]);              # phenotype distribution
       B1 ~ dcat(p.recessive[Brian,]);
       C1 ~ dcat(p.recessive[Clare,]);
       D1 ~ dcat(p.recessive[Diane,]);
       E1 ~ dcat(p.recessive[Eric,]);
       F1 ~ dcat(p.recessive[Fred,]);
       G1 ~ dcat(p.recessive[Gene,]);
       H1 ~ dcat(p.recessive[Henry,]);
       I1 ~ dcat(p.recessive[Ian,]);
       J1 ~ dcat(p.recessive[Jane,]);
       a <- Ann == 2;                       # event that Ann is carrier
       b <- Brian == 2;
       c <- Clare == 2;
       d <- Diane == 2;
       e <- Eric == 2;
       f <- Fred == 2;
       g <- Gene == 2;
       h <- Henry == 2;
       for (J in 1:3) {
              i[J] <- Ian == J      # i[1] = a1 a1
                                    # i[2] = a1 a2
                                    # i[3] = a2 a2 (i.e. Ian affected)
       }                     
    },
    inits = list(
        "Ann1" = 0,
        "Brian1" = 0,
        "Clare" = 1,
        "Diane" = 1,
        "Eric1" = 1,
        "Fred" = 1,
        "Gene" = 2,
        "Henry1" = 1,
        "Ian" = 1,
        "Jane" = 3),
    params = c("p", "i[3]"))
#dcmle(pigs)
