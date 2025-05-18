# Cargar datos
load("mis_datos_meteorologicos.Rdata")
library(sf)
library(dplyr)
library(geoR)
library(gstat)
library(sp)
library(ggplot2)

# 1) Filtrar datos para la fecha específica
fecha_especifica <- "2025-01-01"
datos_combinados$Date <- as.POSIXct(datos_combinados$Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
datos_filtrados <- subset(datos_combinados, as.Date(Date) == fecha_especifica)

# 2) Transformar coordenadas geográficas a coordenadas planas
coords_sf <- st_as_sf(datos_filtrados, coords = c("lon", "lat"), crs = 4326)
proy_sf <- st_transform(coords_sf, crs = 2154)
datos_transformados <- proy_sf %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(Date, x, y, ffkmh)

# 3) Ajuste de tendencia espacial
modelo_ffkmh <- lm(ffkmh ~ x + y, data = datos_transformados)
datos_transformados <- datos_transformados %>%
  mutate(ffkmh_res = resid(modelo_ffkmh),
         hora = as.numeric(format(Date, "%H")))

# 4) Construcción eficiente de la matriz de covarianza espacio-temporal
cov_exponencial <- function(h, sigmasq, phi) {
  sigmasq * exp(-h / phi)
}

construir_covarianza_st <- function(coords, tiempos, sigmasq_s, phi_s, sigmasq_t, phi_t) {
  n <- length(tiempos)
  
  dist_espacial <- as.matrix(dist(coords))  
  dist_temporal <- abs(outer(tiempos, tiempos, "-"))  
  
  K <- cov_exponencial(dist_espacial, sigmasq_s, phi_s) * cov_exponencial(dist_temporal, sigmasq_t, phi_t)
  return(K)
}

# Parámetros ajustados del modelo espacial
sigmasq_s <- 28.7882  # Variabilidad espacial
phi_s <- 29999.9948    # Rango espacial

# Parámetros ajustados del modelo temporal
sigmasq_t <- 60   # Se asume igual a sigmasq_s (puede ajustarse mejor)
phi_t <- 1    # Se asume igual a phi_s (puede ajustarse mejor)

# Construcción de la matriz de covarianza
coords <- as.matrix(datos_transformados[, c("x", "y")])
tiempos <- datos_transformados$hora
valores <- datos_transformados$ffkmh_res

K_obs <- construir_covarianza_st(coords, tiempos, sigmasq_s, phi_s, sigmasq_t, phi_t)

# 5) Predicción en un nuevo punto (s0, t0)
s0 <- c(600000, 6750000)  # Coordenadas del nuevo punto
t0 <- 7  # Hora de predicción

# Vector de covarianza con el punto de predicción
k_pred <- cov_exponencial(sqrt(rowSums((coords - s0)^2)), sigmasq_s, phi_s) * 
  cov_exponencial(abs(tiempos - t0), sigmasq_t, phi_t)

# Solución del sistema regularizado
epsilon <- 1e-6  # Regularización para evitar problemas numéricos
K_obs_reg <- K_obs + diag(epsilon, nrow(K_obs))

lambda <- solve(K_obs_reg, k_pred)

# Predicción y varianza de predicción
prediccion <- sum(lambda * valores)
varianza <- sigmasq_s - sum(k_pred * lambda)

cat("Predicción en (", s0, ",", t0, ") =", prediccion, "\n")
cat("Varianza de predicción =", varianza, "\n")


#### no separable
# Cargar datos
load("mis_datos_meteorologicos.Rdata")
library(sf)
library(dplyr)
library(geoR)
library(gstat)
library(sp)
library(ggplot2)

# 1) Filtrar datos para la fecha específica
fecha_especifica <- "2025-01-01"
datos_combinados$Date <- as.POSIXct(datos_combinados$Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
datos_filtrados <- subset(datos_combinados, as.Date(Date) == fecha_especifica)

# 2) Transformar coordenadas geográficas a coordenadas planas
coords_sf <- st_as_sf(datos_filtrados, coords = c("lon", "lat"), crs = 4326)
proy_sf <- st_transform(coords_sf, crs = 2154)
datos_transformados <- proy_sf %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(Date, x, y, ffkmh)

# 3) Ajuste de tendencia espacial
modelo_ffkmh <- lm(ffkmh ~ x + y, data = datos_transformados)
datos_transformados <- datos_transformados %>%
  mutate(ffkmh_res = resid(modelo_ffkmh),
         hora = as.numeric(format(Date, "%H")))

# Definir funciones de la covarianza de Gneiting
psi1 <- function(r, a, alpha, beta) {
  (a * r^alpha + 1)^beta  # a > 0, 0 < alpha <= 1, 0 <= beta <= 1
}

phi4 <- function(r, c, gama, v) {
  gama * (2^v) * (exp(c * sqrt(r)) + exp(-c * sqrt(r)))^(-v)  # c > 0, v > 0, gama=1
}

gneiting_cov <- function(h, u, sigma, a, alpha, beta, c, gama, v) {
  (sigma^2 / psi1(abs(u)^2, a, alpha, beta)) * phi4(h^2 / psi1(abs(u)^2, a, alpha, beta), c, gama, v)
}

# Construcción de la matriz de covarianza no separable
construir_covarianza_gneiting <- function(coords, tiempos, params) {
  n <- length(tiempos)
  K <- matrix(0, n, n)  # Matriz de covarianza
  
  sigma <- params[1]
  a <- params[2]
  alpha <- params[3]
  beta <- params[4]
  c <- params[5]
  gama <- 1  # Siempre 1
  v <- params[6]
  
  for (i in 1:n) {
    for (j in 1:n) {
      h <- sqrt(sum((coords[i, ] - coords[j, ])^2))  # Distancia espacial
      u <- abs(tiempos[i] - tiempos[j])  # Diferencia temporal
      K[i, j] <- gneiting_cov(h, u, sigma, a, alpha, beta, c, gama, v)
    }
  }
  
  return(K)
}

# Función de error basada en mínimos cuadrados
error_cov_gneiting <- function(params, coords, tiempos, valores) {
  K_modelo <- construir_covarianza_gneiting(coords, tiempos, params)  # Matriz modelada
  K_empirica <- outer(valores, valores, "*")  # Matriz de covarianza empírica
  
  if (!all(dim(K_modelo) == dim(K_empirica))) {
    return(Inf)  # Penaliza si las dimensiones no coinciden
  }
  
  if (any(is.na(K_modelo)) || any(is.nan(K_modelo)) || any(is.infinite(K_modelo))) {
    return(Inf)  # Penaliza valores no válidos
  }
  
  error <- sum((K_empirica - K_modelo)^2)  # Minimización por mínimos cuadrados
  
  return(error)
}

# Datos para optimización
coords <- as.matrix(datos_transformados[, c("x", "y")])
tiempos <- datos_transformados$hora
valores <- datos_transformados$ffkmh_res

# Valores iniciales para optimización
param_iniciales <- c(sigma = 30, a = 1, alpha = 0.5, beta = 0.5, c = 0.1, v = 1)

# Optimización de parámetros del modelo no separable
ajuste_gneiting <- optim(param_iniciales, 
                         error_cov_gneiting, 
                         coords = coords, 
                         tiempos = tiempos, 
                         valores = valores, 
                         method = "L-BFGS-B", 
                         lower = c(0.001, 0.001, 0.01, 0.01, 0.001, 0.01))

# Parámetros óptimos obtenidos
sigma_opt <- ajuste_gneiting$par[1]
a_opt <- ajuste_gneiting$par[2]
alpha_opt <- ajuste_gneiting$par[3]
beta_opt <- ajuste_gneiting$par[4]
c_opt <- ajuste_gneiting$par[5]
v_opt <- ajuste_gneiting$par[6]

# Matriz de covarianza con los parámetros óptimos
K_obs <- construir_covarianza_gneiting(coords, tiempos, ajuste_gneiting$par)

# Predicción en un nuevo punto (s0, t0)
s0 <- c(600000, 6750000)  # Coordenadas del nuevo punto
t0 <- 7  # Hora de predicción

# Covarianza entre observaciones y nuevo punto
k_pred <- sapply(1:length(tiempos), function(i) {
  h <- sqrt(sum((coords[i, ] - s0)^2))  # Distancia espacial
  u <- abs(tiempos[i] - t0)  # Diferencia temporal
  gneiting_cov(h, u, sigma_opt, a_opt, alpha_opt, beta_opt, c_opt, 1, v_opt)
})

epsilon <- 1e-6  # Pequeño valor para estabilidad numérica
K_obs_reg <- K_obs + diag(epsilon, nrow(K_obs))  # Regularización

lambda <- solve(K_obs_reg, k_pred)  # Resolver sistema

# Predicción de Z*(s0, t0)
prediccion <- sum(lambda * valores)

# Varianza de predicción
varianza <- gneiting_cov(0, 0, sigma_opt, a_opt, alpha_opt, beta_opt, c_opt, 1, v_opt) - sum(k_pred * lambda)

# Resultados
cat("Predicción en (", s0, ",", t0, ") =", prediccion, "\n")
cat("Varianza de predicción =", varianza, "\n")
