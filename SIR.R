# Paquete a utilizar
library(deSolve)
library(data.table)

# Estado inicial del modelo SIR y coeficientes de las variables
coeficientes <- c(S = 100, I = 2, R = 0)
parametros <- c(b = 1/2, g = 0.2)

# Tamaño de población 127 millones de mexicanos
size_mex <- 1.27e+08

# Declaramos funcion para el Sistema de EDO´s

SIR <- function(tiempo, coeficientes, parametros) {
  
  with(as.list(c(coeficientes, parametros)), {
    
    # Sistema de ED  
    dS <- -b * S * I
    dI <-  b * S * I - g * I
    dR <-              g * I
    
    # Tasas de cambio    
    return(list(c(dS, dI, dR)))
  })
}

# Intervalo de tiempo
tiempo <- 1:50

# Llamamos a la función para resolver el sistema de Ecuaciones 
solucion <- as.data.table((ode(y = coeficientes, tiempo, func = SIR, parametros)) * size_mex)

# Eliminamos la variable time
solucion$time <- c()

# Mostramos los 20 primeros datos
#mostrar 10 primeros datos
head(solucion, 20)

# Gráficamos la información obtenida
x11()
matplot(x = tiempo, y = solucion, type = "l",
        xlab = "Tiempo", ylab = "S, I, R", main = "SIR Simulación",
        lwd = 2, lty = 2, bty = "l", 
        col = c("blue3", "darkred", "chartreuse4" ))
legend(40, 8.0e+09, c("Susceptibles", "Infectados", "Recuperados"), 
       pch = 17, col = c("blue3", "darkred", "chartreuse4" ), 
       bty = "n", cex = 1)

