require(lmtest)
library(lmtest)
attach(Structural_model)
View(Structural_model)


resettest(Resilience~`Analytics Capability`+`Robustness`+`Social Capital`, power = 2:3, type = "regressor")


resettest(`Analytics Capability`~`Robustness`+`Social Capital`, power = 2:3, type = "regressor")






