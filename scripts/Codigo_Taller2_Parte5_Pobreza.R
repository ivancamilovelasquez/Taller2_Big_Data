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
p_load(rpart.plot)



# Modelo 1 Arbol simple
m1p1 <- rpart(Pobre~., 
               data    = train,
               method = "class",
               parms = list(split = "Gini"))
m1p1
prp(m1p1, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn")
