
####
#Projekt: Czynniki wplywajace na wystepowanie choroby serca
#
#Autor: Zuzanna Kontna
#
#Studia Podyplomowe: Data Scientist. Analityk danych
#Uniwersytet WSB Merito Gdańsk
#maj, 2023
####

# import bibliotek

library(readr)
library(dplyr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(Metrics)
library(caret)
library(randomForest)
library(xgboost)
library("FactoMineR")
library("factoextra")

# import plikow cardio i cardio_1
cardio <- read_csv('cardio_exit.csv')
cardio_1 <- read_csv('cardio_1_exit.csv')

head(cardio)
head(cardio_1)

cardio <- cardio %>% select(-age)


### drzewo decyzyjne

# zamieniam kolumny cholesterol, gluc, smoke, alco, active, cardio na kategorie
cardio[c('cholesterol', 'gluc', 'smoke', 'alco', 'active', 'cardio')] <-lapply(cardio[c('cholesterol', 'gluc', 'smoke', 'alco', 'active', 'cardio')], factor)

# z macierzy korelacji przygotowanej wczesniej w python wybieram kolumny, ktore maja najwieksza korelacje z wystepowaniem choroby

cardio_cor = cardio[c('ap_hi', 'ap_lo', 'age_years', 'cholesterol', 'BMI', 'cardio')]
head(cardio_cor)


# trenowanie modelu

model <- train(cardio ~ ap_hi + ap_lo + age_years + cholesterol + BMI,
              data = cardio_cor,
              method = 'rf',
              ntree = 1000, #dla 1000 generuje sie bardzo dlugo, lepiej zmniejszyc
              na.action = na.roughfix, # na.omit
              importance = TRUE,
              trControl = trainControl(method = "repeatedcv",
                                       number = 3,
                                       repeats = 3)
)

 ## ocena modelu
  
model
model$finalModel
model$resample

plot(varImp(model))


# backtesting

model_pred <- predict(model, cardio_cor, na.action = na.roughfix)
confusionMatrix(cardio_cor$cardio, model_pred)
confusionMatrix(model)


# accuracy na poziomie okolo 73%, model nie jest zly, ale oczywiscie mozna szukac sposobow, by go poprawic


# zapis modelu do pliku
saveRDS(model, file = 'C:/Users/zuza2/OneDrive/Pulpit/podyplomowka/projekt/model_r.rda')

# wczytywanie modelu z pliku
model_1 = readRDS('C:/Users/zuza2/OneDrive/Pulpit/podyplomowka/projekt/model_r.rda')


