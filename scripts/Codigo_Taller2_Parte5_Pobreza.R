#_____________________________________________________________________________#
#                                                                             #
#                      Problem Set 2: Predicting Poverty                      #
#                                                                             #
#_____________________________________________________________________________#

#   Autores: - Jorge Rodríguez                                                  
#            - Iván Velázquez  
#            - Santiago Gonzalez
#            - Maria Jose Colmenares
#
#  Fecha: 22/02/2023 


# - Librerias y paquetes 

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, caret, boot, openxlsx, rio, rpart)
p_load(rpart.plot, Metrics, AER, rattle, ipred, randomForest, ipred)
set.seed(1011)


# Fijar una semilla 
set.seed(10119)


# Dividir los datos de train 

index <- createDataPartition(train$Pobre, p = 0.7, list = FALSE)
train_train <- train[index, ]
train_test <- train[-index, ]




# Modelo 1 : Arbol simple 

m1p1 <- rpart(Pobre~ edad + edad_2 + mujer + estudiante + busca_trabajo +
              amo_casa + hijos_hogar + primaria + secundaria + media +
              superior + exp_trab_actual + horas_trab_usual, 
              data    = train_train,
              method = "class")

predictions <- predict(m1p1, newdata = train_test, type = "class")
correct_predictions <- sum(predictions == train_test$Pobre)
accuracy_1 <- correct_predictions / nrow(train_test)
print(accuracy_1)
# Accuracy de 0.82




# Modelo 2 : Arbol  bagging

m2p2 <- bagging(Pobre~ edad + edad_2 + mujer + estudiante + busca_trabajo +
                      amo_casa + hijos_hogar + primaria + secundaria + media +
                      superior + exp_trab_actual + horas_trab_usual,
                      data  = train_train, nbagg = 500)

m2p2 <- predict(m2p2,newdata = train_test, type="class")

predictions_2 <- predict(m1p1, newdata = train_test, type = "class")
correct_predictions <- sum(predictions_2 == train_test$Pobre)
accuracy_2 <- correct_predictions / nrow(train_test)
print(accuracy_2)
# Acurracy de 0.82




# Modelo 3 : Random Forest 

ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE)

m3p3 <- train(Pobre~ edad + edad_2 + mujer + estudiante + busca_trabajo +
                    amo_casa + hijos_hogar + primaria + secundaria + media +
                    superior + exp_trab_actual + horas_trab_usual, 
                  data = train_train, method = "rf", trControl = ctrl,
                  tuneGrid = expand.grid(mtry = 1:ncol(train_train)))

predictions <- predict(m3p3, newdata = train_test)
correct_predictions <- sum(predictions == train_test$Pobre)
accuracy_3 <- correct_predictions / nrow(train_test)
print(accuracy_3)



# Modelo 4 : Random Forest


# Creamos una grilla para tunear el random forest
tunegrid_rf <- expand.grid(mtry = c(3, 8, 12), 
                           min.node.size = c(500, 1000, 10000,
                                             20000, 30000),
                           splitrule = "gini")

m4p4 <- train(Pobre~ edad + edad_2 + mujer + estudiante + busca_trabajo +
                 amo_casa + hijos_hogar + primaria + secundaria + media +
                 superior + exp_trab_actual + horas_trab_usual,
                 method = "ranger", 
                 trControl = cv5,
                 metric = 'RMSE', 
                 tuneGrid = tunegrid_rf)


plot(m4p4)
