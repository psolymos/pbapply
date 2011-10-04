## asia: an expert system (BUGS Examples Vol. 2)
library(dcmle)
asia <- makeDcFit(
    data = list(
        "asia" = 2,
        "dyspnoea" = 2,
        "p.tuberculosis" =
            structure(c(0.99, 0.95, 0.01, 0.05), .Dim = as.integer(c(2, 2
            ))),
        "p.bronchitis" =
            structure(c(0.7, 0.4, 0.3, 0.6), .Dim = as.integer(c(2, 2))),
        "p.smoking" =
            c(0.5, 0.5),
        "p.lung.cancer" =
            structure(c(0.99, 0.9, 0.01, 0.1), .Dim = as.integer(c(2, 2))),
        "p.xray" =
            structure(c(0.95, 0.02, 0.05, 0.98), .Dim = as.integer(c(2, 2
            ))),
        "p.dyspnoea" =
            structure(c(0.9, 0.3, 0.2, 0.1, 0.1, 0.7, 0.8, 0.9), .Dim = as.integer(c(2, 
            2, 2)))),
    model = function() {
       smoking ~ dcat(p.smoking[]);
       tuberculosis ~ dcat(p.tuberculosis[asia,]);
       lung.cancer ~ dcat(p.lung.cancer[smoking,]);
       bronchitis ~ dcat(p.bronchitis[smoking,]);
       either <- max(tuberculosis,lung.cancer);
       xray ~ dcat(p.xray[either,]);
       dyspnoea ~ dcat(p.dyspnoea[either,bronchitis,])
    },
    params = c("smoking","tuberculosis","lung.cancer","bronchitis",
                       "either","xray"))
#dcmle(asia)
