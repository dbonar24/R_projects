#Biblioteki
install.packages("openxlsx")
install.packages("rugarch")
install.packages("tidyverse")
install.packages("quantmod")
library("openxlsx")
library("rugarch")
library("tidyverse")
library("quantmod")
#Wgrywamy dane
getwd()
#setwd("C:/Users/Dawid/Desktop")

# data
dane_garch11 = read.xlsx("plik_do_R_afsc.xlsx", sheet = 1, startRow = 1, colNames = TRUE, rowNames = TRUE, detectDates = TRUE)

stopy = as.matrix(dane_garch11$zwrot)
rownames(stopy) = rownames(dane_garch11)

################################################################################

# GARCH(1,1) - norm

specification = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "norm", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

# GARCH(1,1) - t-st

specification = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "std", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

################################################################################

# GJR-GARCH(1,1) - norm

specification = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "norm", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

# GJR-GARCH(1,1) - t-st

specification = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "std", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

################################################################################

# EGARCH(1,1) - norm

specification = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "norm", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

# EGARCH(1,1) - t-st

specification = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "std", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

################################################################################

# GARCH(1,1)-inMean - norm

specification = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "norm", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

# GARCH(1,1)-inMean - t-st

specification = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "std", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

################################################################################

# GJR - GARCH(1,1)-inMean - norm

specification = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "norm", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

# GJR - GARCH(1,1)-inMean - t-st

specification = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "std", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

################################################################################

# EGARCH(1,1)-inMean - norm

specification = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "norm", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

# EGARCH(1,1)-inMean - t-st

specification = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                                 submodel = NULL, 
                                                 external.regressors = NULL,
                                                 variance.targeting = FALSE),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                             archm = TRUE, archpow = 1, arfima = FALSE, external.regressors = cbind(dane_garch11$ERTHQ, dane_garch11$EBRXT, dane_garch11$COVID),
                                             archex = FALSE), 
                           distribution.model = "std", start.pars = list(),fixed.pars = list())

garch11.norm = ugarchfit(data = stopy, spec = specification, solver = 'hybrid')
show(garch11.norm)
plot(garch11.norm, which = 1)
plot(garch11.norm, which = 8)
plot(garch11.norm, which = 10)
plot(garch11.norm, which = 11)

macierz_kow = vcov(garch11.norm, robust = TRUE)

################################################################################
