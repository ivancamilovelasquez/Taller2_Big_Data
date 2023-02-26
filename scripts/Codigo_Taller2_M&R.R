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

install.packages("glmnet")
install.packages("rattle")

library(pacman)
p_load(tidyverse, caret)

rm(test_hogares,test_personas,train_personas,train_hogares)

train2 <- train
test2 <- test
glimpse(train2)

# Predecir Ingresos - Regresion lineal

cv5 <- trainControl(number = 5, method = "cv")
mod1 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                         media+superior+exp_trab_actual, 
                       preProcess=NULL,
                       data = train2, 
                       method = "lm",
                       trControl = cv5,
                       metric = 'RMSE')
mod1


# Arbol de decision 
cv5 <- trainControl(number = 5, method = "cv")
modelo1 <- train(Ingtotug ~ edad+edad_2+mujer,
                 data = train2, 
                 method = "rpart", 
                 trControl = cv5)

library(rattle)
fancyRpartPlot(modelo1$finalModel)




