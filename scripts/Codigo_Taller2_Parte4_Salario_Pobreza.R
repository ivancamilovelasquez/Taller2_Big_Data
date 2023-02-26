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


# - Limpiar espacio de trabajo

rm(list = ls())

# - Librerias y paquetes 

p_load(caret, h2o, tidyverse)

train2 <- train 
test2 <- test 

## Modelos para Predecir los Salarios

# Modelo 1: Regresión Lineal 

cv5 <- trainControl(number = 5, method = "cv")
mod1 <- train(Ingtotug~edad + edad_2 + mujer + estudiante + primaria + secundaria + media + superior + exp_trab_actual, 
              data = train2, 
              method = "lm",
              trControl = cv1
)
mod1


# Modelo 2: Regresion lineal (diferentes controles)
mod2 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                 media+superior+exp_trab_actual+horas_trab_usual+busca_trabajo, 
               preProcess=NULL,
               data = train2, 
               method = "lm",
               trControl = cv5,
               metric = 'RMSE')
mod2

#Modelo 3: GBM 
grid_gbm<-expand.grid(n.trees=c(1000),interaction.depth=c(3),shrinkage=c(0.01),n.minobsinnode = c(30))
mod3 <- train(Ingtotug~edad + edad_2 + mujer + estudiante + primaria + secundaria + media + superior + exp_trab_actual,
                      data = train2, 
                      method = "gbm", 
                      trControl = cv5,
                      metric = "RSME",
                      tuneGrid = grid_gbm
)

mod3

# Modelo 3: Arbol de decision 
cv5 <- trainControl(number = 5, method = "cv")
mod3 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                   media+superior+exp_trab_actual,
                 data = train2, 
                 method = "rpart", 
                 trControl = cv5)
mod3
library(rattle)
fancyRpartPlot(mod3$finalModel)

# Modelo 4: Random forest y una grilla para tunear 
tunegrid_rf <- expand.grid(mtry = c(3, 5), 
                           min.node.size = c(10,50,100,150,300),
                           splitrule = "variance")

mod4 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                   media+superior+exp_trab_actual,
                 data = train2, 
                 method = "ranger", 
                 trControl = cv5,
                 metric = 'RMSE', 
                 tuneGrid = tunegrid_rf)
mod4
plot(mod4)
<<<<<<< HEAD

# Modelo 5: Arbol de decision 
cv5 <- trainControl(number = 5, method = "cv")
mod5 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                   media+superior+exp_trab_actual,
                 data = train2, 
                 method = "rpart", 
                 trControl = cv5)
mod5
library(rattle)
fancyRpartPlot(modelo1$finalModel)
=======
>>>>>>> 2fa85b49035eadc0128791c3bd080e920eaf09f8

#Tabla comparativa de los resultados
Resultados <- matrix(c(mean(mod1$resample$RMSE),mean(mod2$resample$RMSE),mean(mod3$resample$RMSE),mean(mod4$resample$RMSE),
                  mean(mod1$resample$Rsquared),mean(mod2$resample$Rsquared),mean(mod3$resample$Rsquared),mean(mod4$resample$Rsquared))
                ,ncol=4 
                ,byrow=TRUE) 
colnames(Resultados) <- c("Regresión Linear","Random Forest GBM","Arbol de Decisión","Random Forest")
rownames(Resultados) <- c("RMSE","R Squared")

Resultados

##Basado en esto, escogeremos el modelo de Random Forest para predecir la pobreza
