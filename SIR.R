# Instalar paquetes (Primero seleccionan y corren estos paquetes)"
install.packages("data.table")
install.packages("deSolve")


# Despues corren todo el codigo 
# Paquete a utilizar
library(deSolve)
library(data.table)

# Tama?o de poblaci?n 127 millones de mexicanos
size_mex <- 22000000

# Numero de digitos
options(digits = 10)

# Estado inicial del modelo SIR y coeficientes de las variables
coeficientes <- c(S = size_mex-1  , I = 1 , R = 0)
parametros <- c(b = (1/3)/size_mex, g = 1/12)

# Intervalo de tiempo
time <- 0:130

# Declaramos funcion para el Sistema de EDO?s

SIR <- function(time, coeficientes, parametros) {
  
  with(as.list(c(coeficientes, parametros)), {
    
    # Sistema de ED  
    dS <- -b * I * S
    dI <-  b * I * S - g * I
    dR <-              g * I
    
    # Tasas de cambio    
    return(list(c(dS, dI, dR)))
  })
}



# Llamamos a la funci?n para resolver el sistema de Ecuaciones 
solucion <- as.data.table(ode(y = coeficientes, time = time, func = SIR, parametros))

# Eliminamos la variable time
solucion$time <- c()

# Mostramos los 20 primeros datos
#mostrar 10 primeros datos
head(solucion, 20)

# Gr?ficamos la informaci?n obtenida
x11()
with(solucion, {
  # GrÃ fica de susceptibles 
  plot(as.vector(time), S, type = "l", col = "deepskyblue3", xlab = "Dias", 
       ylab = "S.I.R", main = "Simulacion SIR")
  
  # Grafica de infectados
  lines(time, I, col = "darkred")
  
  # Grafica de recupeados
  lines(time, R, col = "chartreuse3")
})

# Legendas
legend("right", c("Susceptibles", "Infectados", "Recuperados"), 
       col = c("deepskyblue3", "darkred", "chartreuse3"), lty = 1, bty = "n", pch = 20)

# calculo de Imax
head(solucion)
Imax <- round(max(solucion$I), 0)

# Fechas de contagio
Covi_19_mex <- read.csv("Actualizacion_covid_19.csv")
attach(Covi_19_mex)
dias <- as.vector(Fecha)
print("El dia y cantidad mÃ xima de contagios: ")
print(c(dias[78],Imax))

# Numero reproductivo basico R0
R0 <- (sum(coeficientes)*parametros["b"])/parametros["g"] 
