####################
#####Biblioteki#####
####################

library(tidyverse)
library(readxl)
########################
#####Obszar roboczy#####
########################

setwd("C:/Users/Dawid/Desktop/Licencjat/data")

##############
#####Dane#####
##############

eps_market_data = read.csv("eps_data.csv")

###########################################################
#####Definiujemy zbiór państw OECD które leżą w Europe#####
###########################################################

europe_country = c("Austria", "Belgium", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Iceland", "Italy", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")
europe_country = sort(europe_country)
europe_country

############################
#####Filtrowanie danych#####
############################

#Market based

market_based = eps_market_data %>%
                filter(Variable == "Market based policies",
                       Country %in% europe_country) %>%
                select(Country, Year, Value) %>%
                arrange(Country, Year) %>%
                rename("Market" = "Value")
market_based 

#Non-market based

non_market_based = eps_market_data %>%
  filter(Variable == "Non-market based policies",
         Country %in% europe_country) %>%
  select(Country, Year, Value) %>%
  arrange(Country, Year) %>%
  rename("Non_market" = "Value")

non_market_based

#Technology

technology = eps_market_data %>%
  filter(Variable == "Technology support policies",
         Country %in% europe_country) %>%
  select(Country, Year, Value) %>%
  arrange(Country, Year) %>%
  rename("Tech" = "Value")

technology

#KOFGI - wskaźnik globalizacji

kofgi = read_xlsx(path = "KOFGI.xlsx", sheet = 1)

kofgi_europe_countries = c("Austria", "Belgium", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Iceland", "Italy", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")

kofgi_selected = kofgi %>%
                  filter(country %in% kofgi_europe_countries,
                         year >= 1990 & year <= 2020) %>%
                  select(country, year, KOFGI) %>%
                  arrange(country, year) %>%
                  rename("Country" = "country",
                         "Year" = "year")

kofgi_selected

kofgi_selected$Country = str_replace(kofgi_selected$Country, "Czech Republic", "Czechia")

unique(kofgi_selected$Country)

kofgi_df = as.data.frame(kofgi_selected)
kofgi_df

#GDP per capita

gdp_per_capita = read_csv("GDP.csv")

gdp_selected = gdp_per_capita %>%
                select(-c("Country Code", "Indicator Name", "Indicator Code", "...68"))

name_table = cbind(gdp_selected["Country Name"], seq(from = 1, to = 266, by = 1))
colnames(name_table) = c("Country Name", "Number")
gdp_selected["Country Name"] = seq(from = 1, to = 266, by = 1)

#FDI % of GDP

FDI = read_csv("FDI.csv")

fdi_selected = FDI %>%
  select(-c("Country Code", "Indicator Name", "Indicator Code", "...68"))

fdi_selected["Country Name"] = seq(from = 1, to = 266, by = 1)

#Renewable energy consumption (% of energy consumption)

REC = read_csv("REC.csv")

rec_selected = REC %>%
  select(-c("Country Code", "Indicator Name", "Indicator Code", "...68"))

rec_selected["Country Name"] = seq(from = 1, to = 266, by = 1)

#Kod testowy z łazarski online courses
data = gdp_selected
data2 = fdi_selected
data3 = rec_selected
id<-gdp_selected$`Country Name`
id
c<-length(id)
c # to indicate the number of countries
a<-1960 #beginning of observation period
b<-2022 #end of observation period
year<-c(a:b)
year
d<-length(year)
d
matrix<-matrix(0, nrow=(c*d), ncol=10) ##number of columns should be at least 3 (cross-section indicator, year indicator, and at least one variable)
year.v<-rep(year, c)
year.v
for (n in 1:(c*d)){
  matrix[n,2]<-year.v[n]
}

matrix
country.v<-rep(id, each=d)
country.v
country.v[31]
for (n in 1:(c*d)){
  matrix[n,1]<-country.v[n]
}
matrix
### zmienna GDP
m1<-data[,-c(1)]
m1
m2<-t(m1)
m2.v<-as.vector(m2)
m2.v
for (n in 1:(c*d)){
  matrix[n,3]<-m2.v[n]
}
matrix
### zmienna FDI
m3 = data2[,-c(1)]
m3
m4 = t(m3)
m4.v = as.vector(m4)
m4.v
for (n in 1:(c*d)){
  matrix[n,4] = m4.v[n]
}
matrix
### zmienna REC
m5 = data3[,-c(1)]
m5
m6 = t(m5)
m6.v = as.vector(m6)
m6.v
for (n in 1:(c*d)) {
  matrix[n,5] = m6.v[n]
}
matrix
#### strukturyzowanie macierzy z danymi
world_bank_data = matrix[,-c(6:10)]
world_bank_data = as.data.frame(world_bank_data)
world_bank_data$V1 = as.numeric(world_bank_data$V1)
world_bank_data$V2 = as.numeric(world_bank_data$V2)
world_bank_data$V3 = as.numeric(world_bank_data$V3)
world_bank_data$V4 = as.numeric(world_bank_data$V4)
world_bank_data$V5 = as.numeric(world_bank_data$V5)
world_bank_data
colnames(world_bank_data) = c("country", "year", "GDP", "FDI", "REC")

#inner join nazw z ich liczbowymi odpowiednikami

name_table
world_bank_data = world_bank_data %>%
                inner_join(name_table, by = join_by(country == Number))

#Dane zostały zrestrukturyzowane, przejdzmy do filtrowania

world_bank_data_filtered = world_bank_data %>%
                select(`Country Name`, year, GDP, FDI, REC) %>%
                filter(`Country Name` %in% europe_country,
                       year >= 1990 & year <= 2020) %>%
                arrange(`Country Name`, year) %>%
                rename("Country" = `Country Name`)

world_bank_data_filtered
tail(world_bank_data_filtered)

#Macierz zmiennych objaśniających

X_matrix_PBA = cbind(world_bank_data_filtered, technology, market_based, non_market_based)
X_matrix_PBA
X_matrix_corrected_PBA = X_matrix_PBA[,-c(6,7,9,10,12,13)]
X_matrix_corrected_PBA

X_matrix_corrected_CBA = X_matrix_corrected_PBA %>%
                          filter(Country != "Norway",
                                 Country != "Iceland")
#Emisje CO2 podejściami CBA I PBA

PBA = read_csv("PBA.csv")
CBA = read_csv("CBA.csv")

PBA_eu_countries = c("Austria", "Belgium", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Iceland", "Italy", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")

PBA_selected = PBA %>%
                filter(Year >= 1990 & Year <= 2020,
                       Entity %in% PBA_eu_countries) %>%
                select(Entity, Year, `Annual CO2 emissions (per capita)`) %>%
                rename("PBA" = `Annual CO2 emissions (per capita)`) %>%
                arrange(Entity, Year)

CBA_eu_countries = c("Austria", "Belgium", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Iceland", "Italy", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom")

CBA_selected = CBA %>%
                filter(Year >= 1990 & Year <= 2020,
                       Entity %in% CBA_eu_countries,
                       Entity != "Norway") %>%
                select(Entity, Year, `Per capita consumption-based CO2 emissions`) %>%
                rename("CBA" = `Per capita consumption-based CO2 emissions`) %>%
                arrange(Entity, Year)

test = CBA_selected %>%
          count(Entity)
test

#Tworze dwie macierze do modeli dla zmiennych CBA I PBA

PBA_model = cbind(X_matrix_corrected_PBA, PBA_selected)
PBA_model = PBA_model[,-c(9,10)]
PBA_model

CBA_model = cbind(X_matrix_corrected_CBA, CBA_selected)
CBA_model = CBA_model[,-c(9,10)]
CBA_model 
