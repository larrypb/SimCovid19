# Actualización de COVID-19 hasta el dia 12/04

# Librerias a utilizar
library(data.table)
library(ggplot2)


# Traemos base de datos 
Covid_Mexico <- read.csv("Actualizacion_covid_19.csv")

# Transformamos a data table 
Dt_Covid <- as.data.table(Covid_Mexico)
attach(Dt_Covid)
dias <- 1:46

# Graficamos
x11()
ggplot(data = Dt_Covid, 
       aes(x = dias, y = Confirmados)) + 
  geom_point(colour = "tomato", size = 2) + theme(panel.background = element_rect(fill = 'gray87', colour = 'gray48'))+
  theme(axis.text.x = element_text(angle = 90, size = 8,hjust = 1/2, vjust = 1))

# modelo matematico
modelo1 <- lm(log(Confirmados) ~ dias)

x11()
ggplot(data = Dt_Covid, 
       aes(x = dias, y = log(Confirmados))) + 
  geom_point(colour = "tomato", size = 2) + 
  theme(panel.background = element_rect(fill = 'gray87', colour = 'gray48'))+
  geom_abline(intercept = modelo1$coefficients[1], 
              slope = modelo1$coefficients[2], color = "deepskyblue4", size = 1)+
  theme(axis.text.x = element_text(angle = 90, size = 8,hjust = 1/2, vjust = 1))

# Predicciones
predicciones <- round(predict(modelo1), 2)
round(exp(predicciones), 0)

# funcion modeladora
x <- 47
y <- exp(0.3072*x + 0.1976)


