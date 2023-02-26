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

p_load(caret, h2o)

train2 <- train 
test2 <- test 

## Modelos para Predecir los Salarios

# Modelo 1: Regresión Linear

cv1 <- trainControl(number = 5, method = "cv")
mod1 <- train(Ingtotug~edad + edad_2 + mujer + estudiante + primaria + secundaria + media + superior + exp_trab_actual, 
              data = train2, 
              method = "lm",
              trControl = cv1
)
mod1

#Modelo 2: GBM 
grid_gbm<-expand.grid(n.trees=c(1000),interaction.depth=c(3),shrinkage=c(0.01),n.minobsinnode = c(30))
gbm_res <- train(Ingtotug~edad + edad_2 + mujer + estudiante + primaria + secundaria + media + superior + exp_trab_actual,
                      data = train2, 
                      method = "gbm", 
                      trControl = cv1,
                      metric = "RSME",
                      tuneGrid = grid_gbm
)

gbm_res$resample$RMSE
