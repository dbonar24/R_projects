#Czyszczę pamięć oraz folder roboczy
rm(list=ls())
getwd()
setwd("C:/Users/Dawid/Desktop/analiza_jakosciowa")
getwd()
#Instalacja wymaganych pakietów
install.packages("readxl")
library(readxl)
install.packages("glm2")
library(glm2)
install.packages("tidyverse")
library(tidyverse)
#Wgrywanie danych
path_ = "dane_probit.xlsx"
dane = read_xlsx(path = path_, sheet = 1, range = "A1:I5737")
#Przygotowanie danych do modulu
colnames(dane)[colnames(dane) == "dośw (lata)"] = "dosw" #zmieniam nazwe dla uproszczenia
dane = dane %>%
  mutate(dosw_cube = dosw ^ 2)
#Obserwacje Y (potrzebne w pierwszym pliku)
y_ = as.matrix(dane[,1])
#Budowa formuły i estymacja modelu
formula_ = as.formula(Y_Fees01 ~ Rating01 + Ajurweda + Dentystyka + Dermatologia + Otorynolaryngologia + Medycyna_ogolna + dosw + dosw_cube)
formula_
probit = glm(formula = formula_, family = binomial(link="probit"), data = dane)
#Błedy średnie szacunku
cov_matrix = vcov(probit)
sd = as.matrix(diag(cov_matrix)^(0.5))
colnames(sd) = "Bledy srednie szacunku"
coeffs = as.matrix(probit$coefficients)
colnames(coeffs) = "Oceny Parametrow"
#Pierwszy plik z wynikami
plik_out = "Wyniki_probit.csv"
sink(plik_out, append = FALSE)
print(sd)
print(coeffs)
print(y_) #w taki sposób uzyskujeym tylko 1000 pierwszych obserwacji dlatego ręcznie podmieniłem te wartosci w pliku csv
sink()
#Prawdopodobienstwo pobierania wysokiej oraz niskiej oplaty za konsultacje
const = matrix(1, nrow = nrow(dane), 1)
X_1 = cbind(const, dane[,-1])
X_1 = as.matrix(X_1[,-9])
wsp = as.vector(coeffs)
zet = X_1%*%coeffs
Pr_Y1 = pnorm(zet, 0,1)
Pr_Y0 = 1 - Pr_Y1
#Trafność ocen (prognoz) - Tabela trafności
Y_prog = 1*as.matrix(Pr_Y1 >= 0.5)
y = dane[,1]
tmp = cbind(y, Y_prog)
colnames(tmp) = c("y01","y_prog")
Tab_traf = xtabs(formula = ~ Y_prog + y01, data = tmp)
Tab_traf
#Mierniki
R2_total = (Tab_traf[1,1]+Tab_traf[2,2])/nrow(y)
R2_czast_niska = Tab_traf[1,1]/sum(Tab_traf[,1])
R2_czast_wysoka = Tab_traf[2,2]/sum(Tab_traf[,2])
R2_total
R2_czast_niska
R2_czast_wysoka
#Drugi plik z wynikami
plik_out2 = "Mierniki.csv"
sink(file = plik_out2, append = FALSE)
print("R2_calkowite")
print(R2_total)
print("R2 dla lekarzy o niskiej oplacie")
print(R2_czast_niska)
print("R2 dla lekarzy o wysokiej oplace")
print(R2_czast_wysoka)
sink()
#Efekty krańcowe
#f-cja gęstosci 
f_x = dnorm(zet, mean = 0, sd = 1)
me_rat = f_x * coeffs[2]
me_aju = f_x * coeffs[3]
me_derm = f_x * coeffs[4]
me_dent = f_x * coeffs[5]
me_laryn = f_x * coeffs[6]
me_med_ogl = f_x * coeffs[7]
#Średnia ze wszystkich efektów kranocwych
mean_0 = mean(me_rat)
mean_1 = mean(me_aju)
mean_2 = mean(me_derm)
mean_3 = mean(me_dent)
mean_4 = mean(me_laryn)
mean_5 = mean(me__med_ogl)
all_means = cbind(mean_0, mean_1, mean_2, mean_3, mean_4, mean_5)
colnames(all_means) = c("me_rat", "sr_me_aju", "sr_me_derm", "sr_me_dent", "se_me_laryn", "se_me_med_ogl")
#Trzeci plik z efektami krańcowymi
dane_do_pliku = cbind(dane[,1], Pr_Y1, me_rat, me_aju, me_derm, me_dent, me_laryn, me__med_ogl)
colnames(dane_do_pliku) = c("Y", "Pr_Y1", "me_rat", "me_aju", "me_derm", "me_dent", "me_laryn", "me_med_ogl")
plik_out3 = "Efekty_krancowe_poprawione2.csv"
sink(file = plik_out3, append = FALSE)
print(all_means)
print(dane_do_pliku)
sink()
#Użyłem pakietu margins by sprawdzić czy efekty są poprawnie policzone
library(margins)
marg = margins(probit, variables = c("Rating01", "Ajurweda", "Dentystyka", "Dermatologia", "Otorynolaryngologia", "Medycyna_ogolna"))
marg
