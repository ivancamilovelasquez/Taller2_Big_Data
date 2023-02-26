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
p_load(rvest, tidyverse, ggplot2, robotstxt, psych, stargazer, boot, openxlsx, rio, GGally)


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


# - TEST

# - Todos 

Base_descriptivas <- test[c("mujer","edad", "amo_casa", "hijos_hogar",
                            "estudiante", "primaria", 
                            "secundaria", "media", "superior", 
                            "Ingtotug", "numero_personas", 
                            "exp_trab_actual", "horas_trab_usual", 
                            "num_menores", "Pobre")]

estadisticas_todos <- data.frame(sapply(Base_descriptivas, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_todos, file = "Estadisticos_todos_test.xlsx")


# Gráficos

df <- train[c("mujer","edad", 
                "Ingtotug", "numero_personas", 
                "exp_trab_actual", "horas_trab_usual", 
                "num_menores", "Pobre")]
df$Pobre <- factor(df$Pobre, labels = c("No pobre", "Pobre"))

ggpairs(df, columns = 2:6, ggplot2::aes(colour = Pobre)) +
  theme_minimal() + 
  labs(title = "Gráfico de matriz de dispersión") + 
  scale_y_continuous(labels = scales::number_format()) + 
  scale_x_continuous(labels = scales::number_format()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 8))





