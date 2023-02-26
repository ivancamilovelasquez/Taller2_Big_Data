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
install.packages("MLmetrics")
install.packages("Metrics")

library(pacman)
p_load(tidyverse, caret)

rm(test_hogares,test_personas,train_personas,train_hogares)

train2 <- train
test2 <- test
glimpse(train2)

# Predecir Ingresos - Regresion lineal

cv5 <- trainControl(number = 5, method = "cv")
mod1 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                         media+superior+exp_trab_actual+horas_trab_usual, 
                       preProcess=NULL,
                       data = train2, 
                       method = "lm",
                       trControl = cv5,
                       metric = 'RMSE')
mod1

# Arbol de decision 
cv5 <- trainControl(number = 5, method = "cv")
modelo1 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                   media+superior+exp_trab_actual,
                 data = train2, 
                 method = "rpart", 
                 trControl = cv5)

library(rattle)
fancyRpartPlot(modelo1$finalModel)


y_hat_insample1 = predict(modelo1, newdata = train2)
y_hat_outsample1 = predict(modelo1, newdata = test2)

library(MLmetrics)
MAE(y_pred = y_hat_insample1, y_true = train2$Ingtotug)

SMAPE(y_pred = y_hat_insample1, y_true = train2$Ingtotug)


# Random forest y una grilla para tunear 
tunegrid_rf <- expand.grid(mtry = c(3, 5), 
                           min.node.size = c(10,50,100,150,300),
                           splitrule = "variance")

modelo2 <- train(Ingtotug~edad+edad_2+mujer+estudiante+primaria+secundaria+
                 media+superior+exp_trab_actual,
                 data = train2, 
                 method = "ranger", 
                 trControl = cv5,
                 metric = 'RMSE', 
                 tuneGrid = tunegrid_rf)

plot(modelo2)

# Ahora predecimos pobreza

test2$Ingtotug_pred <- predict(modelo2, newdata = test2)

train2$Ingtotug_pred <- predict(modelo2, newdata = train2)

# Clasificacion con prediccion de Random Forest
# Logit

mylogit <- glm(Pobre~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                 primaria+secundaria+media+superior+exp_trab_actual,
               data = train2,
               family = "binomial")
summary(mylogit, type = "text")

test2$pred <- predict(mylogit,newdata = test2, type = "response")
summary(test2$pred)

hist(test2$pred)
test2$ajuste <- ifelse(test2$pred > 0.3, 1, 0)
prop.table(table(ajuste))

#Accuracy (train)
train2$ajuste <- predict(mylogit,newdata = train2, type = "response")
train2$pobre_pred <- ifelse(train2$ajuste > 0.3, 1,0)
train2$dif <- train2$Pobre - train2$pobre_pred
prop.table(table(train2$dif))


predictTest_mod <- data.frame(id = test2$id, pobre = test2$ajuste)

write.csv(predictTest_mod, "C:/Users/Santiago/Downloads/Escritorio/DOCUMENTOS SANTIGO/Maestria Uniandes/Big Data & Machine Learning/Problem Sets/predictTest_mod.csv", row.names = FALSE)


# logit caret

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = F,
                    verbose=FALSE,
                    savePredictions = T)

set.seed(2905)
mylogit_caret <- train(as.factor(Pobre)~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                         primaria+secundaria+media+superior+exp_trab_actual, 
                       data = train2, 
                       method = "glm",
                       trControl = ctrl_def,
                       family = "binomial", 
                       preProcess =  c("center", "scale"))

mylogit_caret

test2$pred <- predict(mylogit_caret,newdata = test2, type = "raw")
summary(test2$pred)

prop.table(table(test2$pred))

train2$pred <- predict(mylogit_caret,newdata = train2, type = "raw")
prop.table(table(train2$pred))


# logit lasso caret
lambda_grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300

set.seed(2905)
mylogit_lasso_acc <- train(as.factor(Pobre)~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                             primaria+secundaria+media+superior+exp_trab_actual, 
                           data = train2, 
                           method = "glmnet",
                           trControl = ctrl,
                           family = "binomial", 
                           metric = "Accuracy",
                           tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid), 
                           preProcess = c("center", "scale")
)
mylogit_lasso_acc


# logit ridge caret
lambda_grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300

set.seed(2905)
mylogit_lasso_acc <- train(as.factor(Pobre)~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                             primaria+secundaria+media+superior+exp_trab_actual, 
                           data = train2, 
                           method = "glmnet",
                           trControl = ctrl,
                           family = "binomial", 
                           metric = "Accuracy",
                           tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                           preProcess = c("center", "scale")
)
mylogit_lasso_acc


test2$pred_logit_lasso_acc <- predict(mylogit_lasso_acc,newdata = test2)
summary(test2$pred_logit_lasso_acc)
prop.table(table(test2$pred_logit_lasso_acc))

predictTest_mod_logit_lasso_acc <- data.frame(id = test2$id, pobre = test2$pred_logit_lasso_acc)

write.csv(predictTest_mod, "C:/Users/Santiago/Downloads/Escritorio/DOCUMENTOS SANTIGO/Maestria Uniandes/Big Data & Machine Learning/Problem Sets/predictTest_mod_logit_lasso_acc.csv", row.names = FALSE)

class(test2$pred_logit_lasso_acc)
class(test2$pred)
# UP-Sampling
glimpse(train2)
set.seed(2905)
train2$Pobre <- as.factor(train2$Pobre)
class(train2$Pobre)
upSampledTrain <- upSample(x = train2,
                           y = train2$Pobre,
                           yname = "Pobre")
dim(train2)

dim(upSampledTrain)

table(upSampledTrain$Pobre)

set.seed(2905)
mylogit_lasso_upsample <- train(Pobre~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                                  primaria+secundaria+media+superior+exp_trab_actual, 
                                data = upSampledTrain, 
                                method = "glmnet",
                                trControl = ctrl,
                                family = "binomial", 
                                metric = "Accuracy",
                                tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid), 
                                preProcess = c("center", "scale")
)
mylogit_lasso_upsample


# SMOTE
p_load("smotefamily")
glimpse(train2)

predictors<-c("edad","edad_2","mujer","estudiante","busca_trabajo",
              "amo_casa","hijos_hogar","primaria","secundaria","media",
              "superior", "exp_trab_actual","horas_trab_usual",
              "Ingtotug_pred")
head(train2[predictors])

smote_output = SMOTE(X = train2[predictors],
                     target = train2$Pobre)
smote_data = smote_output$data
table(train2$Pobre)
table(smote_data$class)

set.seed(2905)
mylogit_lasso_smote<- train(class~edad+edad_2+mujer+estudiante+busca_trabajo+
                            amo_casa+hijos_hogar+primaria+secundaria+media+
                            superior+exp_trab_actual+horas_trab_usual
                            +Ingtotug_pred,
                            data = smote_data, 
                            method = "glmnet",
                            trControl = ctrl,
                            family = "binomial", 
                            metric = "Accuracy",
                            tuneGrid = expand.grid(alpha = 1,lambda=lambda_grid), 
                            preProcess = c("center", "scale")
)
mylogit_lasso_smote

# LDA

lda_fit = train(Pobre~Ingtotug_pred+edad+edad_2+mujer+estudiante+
                  primaria+secundaria+media+superior+exp_trab_actual, 
                data=train2, 
                method="lda",
                trControl = ctrl,
                metric = 'Accuracy')

lda_fit

test2$pred_lda <- predict(lda_fit,newdata = test2, type = "raw")
summary(test2$pred_lda)
prop.table(table(test2$pred_lda))

