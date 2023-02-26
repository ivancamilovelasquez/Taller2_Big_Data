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

library(pacman)
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, openxlsx, rio)


# - 3. Estadísticas Descriptivas 

# - TRAIN

# - Todos 

Base_descriptivas <- train[c("mujer","edad", "amo_casa", "hijos_hogar",
                           "estudiante", "primaria", 
                            "secundaria", "media", "superior", 
                            "Ingtotug", "numero_personas", 
                            "exp_trab_actual", "horas_trab_usual", 
                            "num_menores", "Pobre")]

estadisticas_todos <- data.frame(sapply(Base_descriptivas, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_todos, file = "Estadisticos_todos.xlsx")

# - pobres

Base_pobres <- Base_descriptivas[Base_descriptivas$Pobre == 1, ]

estadisticas_pobres <- data.frame(sapply(Base_pobres, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_pobres, file = "Estadisticos_pobres.xlsx")

# - no pobres

Base_nopobres <- Base_descriptivas[Base_descriptivas$Pobre == 0, ]

estadisticas_nopobres <- data.frame(sapply(Base_nopobres, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_nopobres, file = "Estadisticos_nopobres.xlsx")


