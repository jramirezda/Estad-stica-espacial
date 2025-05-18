# Crear el dataframe
datos <- data.frame(
  x = c(5, 5, 8, 5, 5, 8, 5, 5, 8, 5, 5, 8),
  y = c(6, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6),
  hora = c("0:00", "0:00", "0:00", "1:00", "1:00", "1:00", "2:00", "2:00", "2:00", "3:00", "3:00", "3:00"),
  data = c(0.56, 0.78, 0.64, 0.58, 0.80, 0.67, 0.59, 0.81, 0.67, 0.57, 0.79, 0.65)
)

# Ver el dataframe
print(datos)


# Calcular distancias espaciales entre las coordenadas
coords <- as.matrix(datos[, c("x", "y")])
matriz_dist_espacial <- as.matrix(dist(coords))

# Convertir las horas a formato numérico (en horas)
horas_numeric <- as.numeric(substr(datos$hora, 1, 1))
matriz_dist_temporal <- abs(outer(horas_numeric, horas_numeric, "-"))

# quiero obtener, 6,6,0:30
# quiero encontrar las covarianzas pero con el tiempo (un argumento nuevo t-t´)
# si puedo usarel supuesto de separabilidad, y modelo los medelos espaciales y temporales por separado 
# luego hago un modelo aditivo o multiplicativo.
#vector de medias 
ybar=mean(datos$data)

datoscen=datos$data-ybar

# modelo separable si exponencial espacial 

Exponesp <- function(theta, h) {
  silla <- theta[1]
  rango <- theta[2]
  resultado <- silla * (1 - exp(-h / rango))
  return(resultado)
}

#modelo separable si exponencial temporal
Expontem <- function(theta, th) {
  silla <- theta[1]
  rango <- theta[2]
  resultado <- silla * (1 - exp(-th / rango))
  return(resultado)
}

#matriz de covarianzas temporal con cada vetor de datos:


# Parámetros para las funciones de covarianza
theta_esp <- c(1, 2)  # Parámetros espaciales (silla, rango)
theta_tem <- c(1, 1)  # Parámetros temporales (silla, rango)

# Construcción de matrices de covarianza separables
cov_espacial <- Exponesp(theta_esp, matriz_dist_espacial)
cov_temporal <- Expontem(theta_tem, matriz_dist_temporal)

var(datos$data)
# Matriz de covarianza separable multiplicativa
cov_separable_multiplicativa <- cov_espacial * cov_temporal 

# Matriz de covarianza separable aditiva
cov_separable_aditiva <- cov_espacial + cov_temporal

##predicción y varianza del error de preducción 
# Predicción por kriging
# Punto de predicción
x_pred <- 6
y_pred <- 6
t_pred <- 0.5

# Calcular distancias del punto de predicción a los puntos observados
dist_esp_pred <- sqrt((datos$x - x_pred)^2 + (datos$y - y_pred)^2)
dist_tem_pred <- abs(horas_numeric - t_pred)

# Covarianzas del punto de predicción con los datos observados
cov_esp_pred <- Exponesp(theta_esp, dist_esp_pred)
cov_tem_pred <- Expontem(theta_tem, dist_tem_pred)
cov_pred <- cov_esp_pred * cov_tem_pred


zprepro=mean(datos$data)+t(cov_pred)%*% solve(cov_separable_multiplicativa)%*%datoscen
zpresum=mean(datos$data)+t(cov_pred)%*% solve(cov_separable_multiplicativa)%*%datoscen

## error cuadritco medio 
t(cov_pred)%*%solve(cov_separable_multiplicativa)%*%cov_pred
