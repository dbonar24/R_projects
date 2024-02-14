#Czyścimy zmienne
rm(list = ls())
#Biblioteki 
library(glm2)
library(tidyverse)
#Ustawiam obszar roboczy
getwd()
setwd("C:/Users/Dawid/Desktop/analiza_jakosciowa")
#Wgrywam dane
path1_ = "dane_testowe.csv"
path2_ = "dane_treningowe.csv"

dane_testowe = read.csv(file = path1_)
dane_treningowe = read.csv(file = path2_)
dane_treningowe = as.data.frame(dane_treningowe)
#Model logit na danych treningowych
formula_logit_ = as.formula(credit_risk ~ duration + credit_history + other_debtors + property + telephone)
logit = glm(formula = formula_logit_, family = binomial(link="logit"), data = dane_treningowe)
summary(logit)
#Kalkulacja wartosci teoretycznych dla logitu
coeffs_logit = as.vector(logit$coefficients)
X_logit = dane_treningowe %>%
  select(duration, credit_history, other_debtors, property, telephone)
X_logit = cbind(1, X_logit)
X_logit = as.matrix(X_logit)
zet_logit = X_logit %*% coeffs_logit
#Funkcja dla dystrybuanty
cdf_= function(zet_){
  cdf_=exp(zet_)/( 1+exp(zet_) )
  colnames(cdf_)="Pr_Y=1"
  return(cdf_)  
}
Pr_Y1_logit=cdf_(zet_=zet_logit)
#Trafnosc prognoz dla logitu
p_cut=mean(dane_treningowe$credit_risk)
T = nrow(dane_treningowe)
Y_prognozy_logit = 1*as.matrix(Pr_Y1_logit >= (p_cut*matrix(1,T,1)))
colnames(Y_prognozy_logit) = "prog"
y = dane_treningowe[,2]
tmp = cbind(y, Y_prognozy_logit)
Tab_traf_logit = xtabs(formula = ~ prog + y, data = tmp)
Tab_traf_logit
R2_total_logit = (Tab_traf_logit[1,1]+Tab_traf_logit[2,2])/nrow(dane_treningowe)
R2_total_logit
#Tabela trafności dla danych testowych dla logitu
X_logit_test  = dane_testowe %>%
  select(duration, credit_history, other_debtors, property, telephone)
X_logit_test = cbind(1, X_logit_test)
X_logit_test = as.matrix(X_logit_test)
zet_logit_test = X_logit_test %*% coeffs_logit
Pr_Y1_logit_test=cdf_(zet_=zet_logit_test)
#Trafnosc prognoz dla logitu
p_cut_test=mean(dane_testowe$credit_risk)
T_test = nrow(dane_testowe)
Y_prognozy_logit_test = 1*as.matrix(Pr_Y1_logit_test >= (p_cut_test*matrix(1,T_test,1)))
colnames(Y_prognozy_logit_test) = "prog"
y_test = dane_testowe[,2]
tmp_test = cbind(y_test, Y_prognozy_logit_test)
Tab_traf_logit_test = xtabs(formula = ~ prog + y_test, data = tmp_test)
Tab_traf_logit_test
R2_total_logit_test = (Tab_traf_logit_test[1,1]+Tab_traf_logit_test[2,2])/nrow(dane_testowe)
R2_total_logit_test
#Model probit na danych treningowych
formula_probit_ = as.formula(credit_risk ~ duration + credit_history + installment_rate + housing + telephone)
probit = glm(formula = formula_probit_, family = binomial(link="probit"), data = dane_treningowe)
summary(probit)
#Kalkulacja wartosci teoretycznych dla probitu
coeffs_probit = as.vector(probit$coefficients)
X_probit = dane_treningowe %>%
  select(duration, credit_history, installment_rate, housing, telephone)
X_probit = cbind(1, X_probit)
X_probit = as.matrix(X_probit)
zet_probit = X_probit %*% coeffs_probit
#Pr_Y1
Pr_Y1_probit = pnorm(zet_probit, 0,1)
#Trafność prognoz dla probitu
Y_prognozy_probit = 1*as.matrix(Pr_Y1_probit >= (p_cut*matrix(1,T,1)))
colnames(Y_prognozy_probit) = "prog"
tmp2 = cbind(y, Y_prognozy_probit)
Tab_traf_probit = xtabs(formula = ~ prog + y, data = tmp2)
Tab_traf_probit
R2_total_probit = (Tab_traf_probit[1,1]+Tab_traf_probit[2,2])/nrow(dane_treningowe)
R2_total_probit
#Tabela trafności dla danych testowych dla probitu
X_probit_test  = dane_testowe %>%
  select(duration, credit_history, installment_rate, housing, telephone)
X_probit_test = cbind(1,X_probit_test)
X_probit_test = as.matrix(X_probit_test)
zet_probit_test = X_probit_test %*% coeffs_probit
Pr_Y1_probit_test=cdf_(zet_=zet_probit_test)
#Trafnosc prognoz dla logitu
p_cut_test=mean(dane_testowe$credit_risk)
T_test = nrow(dane_testowe)
Y_prognozy_probit_test = 1*as.matrix(Pr_Y1_probit_test >= (p_cut_test*matrix(1,T_test,1)))
colnames(Y_prognozy_probit_test) = "prog"
y_test = dane_testowe[,2]
tmp_test2 = cbind(y_test, Y_prognozy_probit_test)
Tab_traf_probit_test = xtabs(formula = ~ prog + y_test, data = tmp_test2)
Tab_traf_probit_test
R2_total_probit_test = (Tab_traf_probit_test[1,1]+Tab_traf_probit_test[2,2])/nrow(dane_testowe)
R2_total_probit_test
#Funkcja gestosci dla logitu
pdf_= function(zet_){
  pdf_=exp(zet_)/( 1+exp(zet_) )^2 # f.gestosci dla logitu
  colnames(pdf_)="f(Y=y)"
  return(pdf_)  
}
f_x_logit = pdf_(zet_ = zet_logit)
#Efekty krancowe dla logitu
me_duration_l = f_x_logit * coeffs_logit[2]
me_credit_history_l = f_x_logit * coeffs_logit[3]
me_other_debtors_l = f_x_logit * coeffs_logit[4]
me_property_l = f_x_logit * coeffs_logit[5]
me_telephone_l = f_x_logit * coeffs_logit[6]
#Srednie efekty krancowe logit
mean_me_duration_l = mean(me_duration_l)
mean_me_credit_history_l = mean(me_credit_history_l)
mean_me_other_debtors_l = mean(me_other_debtors_l)
mean_me_property_l = mean(me_property_l)
mean_me_telephone_l = mean(me_telephone_l)
#Funkcja gestosci dla probitu
f_x_probit = dnorm(zet_probit, mean = 0, sd = 1)
me_duration_d = f_x_probit * coeffs_probit[2]
me_credit_history_d = f_x_probit * coeffs_probit[3]
me_installment_rate_d = f_x_probit * coeffs_probit[4]
me_housing_d = f_x_probit * coeffs_probit[5]
me_telephone_d = f_x_probit * coeffs_probit[6]
#Średnie efekty krańcowe probit
mean_me_duration_d = mean(me_duration_d)
mean_me_credit_history_d = mean(me_credit_history_d)
mean_me_installment_rate_d = mean(me_installment_rate_d)
mean_me_housing_d = mean(me_housing_d)
mean_me_telephone_d = mean(me_telephone_d)
#Krzywe ROC w celu porównania modelu logitowego z probitowym
install.packages("pROC")
library(pROC)
install.packages("plotROC")
library(plotROC)
roc(Tab_traf_logit_test)
#ROC dla logitu test
pred_vs_real_logit = cbind(dane_testowe$credit_risk, Pr_Y1_logit_test)
colnames(pred_vs_real_logit)= c("Real", "Pred")
pred_vs_real_logit = as.data.frame(pred_vs_real_logit)
ROC = roc(pred_vs_real_logit$Real, pred_vs_real_logit$Pred)
plot(ROC, col = "blue", print.auc=TRUE)
#ROC dla probitu test
pred_vs_real_probit = cbind(dane_testowe$credit_risk, Pr_Y1_probit_test)
colnames(pred_vs_real_probit) = c("Real", "Pred")
pred_vs_real_probit = as.data.frame(pred_vs_real_probit)
ROC2 = roc(pred_vs_real_probit$Real, pred_vs_real_probit$Pred)
plot(ROC2, col = "red", print.auc=TRUE)


