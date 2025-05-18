#carga de los datos 
load("mis_datos_meteorologicos.Rdata")
library(sf)
library(dplyr)
# Definir la fecha y hora específica que deseas filtrar
fecha_especifica <- "2025-01-01"
# Filtrar los datos y seleccionar las columnas necesarias
datos_filtrados <- datos_combinados %>%
  filter(Date == fecha_especifica) %>%
  select(Date, lon, lat, ffkmh, Gustkmh)

# Mostrar los primeros registros
head(datos_filtrados)
# Convertir a objeto sf con CRS WGS84 (EPSG:4326) - sistema de coordenadas geográficas
coords_sf <- st_as_sf(datos_filtrados, coords = c("lon", "lat"), crs = 4326)

# Transformar las coordenadas a Lambert-93 (EPSG:2154) - coordenadas planas en metros
proy_sf <- st_transform(coords_sf, crs = 2154)

# Extraer las coordenadas transformadas y quitar la geometría
datos_transformados <- proy_sf %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(Date, x, y, ffkmh, Gustkmh)

# Mostrar los primeros registros
head(datos_transformados)

#grafico
library(sf)
library(ggplot2)

# Ruta del archivo shapefile descargado
shapefile_path <- "ne_110m_admin_1_states_provinces.shp"

# Cargar el mapa mundial
world_map <- st_read(shapefile_path)

# Filtrar solo Francia
france_map <- world_map %>% filter(admin == "France")
france_map <- st_transform(france_map, crs = 2154)

# Crear el mapa con las ubicaciones de las estaciones meteorológicas
ggplot() +
  geom_sf(data = france_map, fill = "gray90", color = "black") +  # Mapa base de Francia
  geom_point(data = datos_transformados, aes(x = x, y = y), 
             color = "red", size = 2) +  # Puntos de las estaciones
  labs(title = "Ubicaciones de Estaciones Meteorológicas en Francia",
       x = "Coordenada X (m)", 
       y = "Coordenada Y (m)") +
  theme_minimal()


## Descriptivos de los datos 
summary(datos_transformados)

boxplot(datos_transformados$ffkmh, main = "Boxplot de Velocidad Promedio del Viento (km/h)")

boxplot(datos_transformados$Gustkmh, main = "Boxplot de Ráfaga Máxima de Viento (km/h)")


library(geoR)
library(dplyr)

library(gstat)
library(sp)

# Convertir los datos en un objeto geodata
geo_francia_ffkmh <- as.geodata(datos_transformados %>% select(x, y,ffkmh))

geo_francia_Gustkmh <- as.geodata(datos_transformados %>% select(x, y,Gustkmh))

# Graficar los datos en 3D
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))  # Ajusta el margen superior
plot(geo_francia_ffkmh, scatter3d = TRUE)
mtext("Distribución Espacial de la Velocidad del Viento en Francia (km/h)", 
      outer = TRUE, cex = 1.5, font = 2)


par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))  # Ajusta el margen superior
plot(geo_francia_Gustkmh, scatter3d = TRUE)
mtext("Distribución Espacial de las Ráfagas Máximas en Francia (km/h)", 
      outer = TRUE, cex = 1.5, font = 2)

library(gstat)
library(sp)
library(dplyr)
library(geoR)
# 1️⃣ Ajustar modelos de regresión para quitar tendencia en ffkmh y Gustkmh
modelo_ffkmh <- lm(ffkmh ~ x + y, data = datos_transformados)
modelo_Gustkmh <- lm(Gustkmh ~ x + y, data = datos_transformados)

# 2️⃣ Obtener los residuales (sin tendencia)
datos_transformados <- datos_transformados %>%
  mutate(ffkmh_res = resid(modelo_ffkmh),
         Gustkmh_res = resid(modelo_Gustkmh))

# 3️⃣ Convertir a objeto espacial con los residuales
datos_sp <- datos_transformados %>% select(x, y, ffkmh_res, Gustkmh_res)
coordinates(datos_sp) <- ~x + y

# Convertir las variables residuales en geodata
geo_francia_ffkmh_res <- as.geodata(datos_transformados %>% select(x, y, ffkmh_res))
geo_francia_Gustkmh_res <- as.geodata(datos_transformados %>% select(x, y, Gustkmh_res))

par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))  # Espacio para el título

# Graficar los residuos sin tendencia espacial
plot(geo_francia_ffkmh_res, scatter3d = FALSE)  # Elimina scatter3d si da problemas
mtext("Distribución Espacial de los Residuos de la Velocidad del Viento en Francia (km/h)", 
      outer = TRUE, cex = 1.5, font = 2)

# Para la otra variable
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))  # Espacio para el título
plot(geo_francia_Gustkmh_res, scatter3d = FALSE)
mtext("Distribución Espacial de los Residuos de las Ráfagas Máximas en Francia (km/h)", 
      outer = TRUE, cex = 1.5, font = 2)


library(geoR)

# Calcular el semivariograma empírico
variograma_ffkmh <- variog(geo_francia_ffkmh_res)
variograma_Gustkmh <- variog(geo_francia_Gustkmh_res)

# Configurar espacio gráfico para mostrar ambos variogramas
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))

# Graficar el semivariograma para ffkmh_res
plot(variograma_ffkmh, pch = 19, col = "blue",
     main = "Semivariograma Empírico - Velocidad del Viento",
     xlab = "Distancia",
     ylab = "Semivarianza")

# Graficar el semivariograma para Gustkmh_res
plot(variograma_Gustkmh, pch = 19, col = "red",
     main = "Semivariograma Empírico - Ráfagas Máximas",
     xlab = "Distancia",
     ylab = "Semivarianza")

# Agregar título general
mtext("Análisis de Semivariogramas Empíricos", outer = TRUE, cex = 1.5, font = 2)

library(geoR)

# Crear los semivariogramas empíricos con una distancia máxima de 600000
variograma_ffkmh <- variog(geo_francia_ffkmh_res, max.dist = 600000)
variograma_Gustkmh <- variog(geo_francia_Gustkmh_res, max.dist = 600000)

# Lista de modelos a probar
modelos <- c("sph", "exp", "gau", "mat")

# Ajustar y graficar cada modelo
par(mfrow = c(2, 2))

for (modelo in modelos) {
  modelo_variograma_ffkmh <- variofit(variograma_ffkmh, cov.model = modelo, 
                                      ini.cov.pars = c(1, 30000), # Semivarianza inicial y rango
                                      nugget = 0) # Sin efecto pepita
  
  plot(variograma_ffkmh, pch = 19, col = "blue",
       main = paste("Ajuste Variograma ffkmh -", modelo),
       xlab = "Distancia", ylab = "Semivarianza")
  lines(modelo_variograma_ffkmh, col = "darkblue", lwd = 2)
  
  modelo_variograma_Gustkmh <- variofit(variograma_Gustkmh, cov.model = modelo, 
                                        ini.cov.pars = c(1, 30000),
                                        nugget = 0)
  
  plot(variograma_Gustkmh, pch = 19, col = "red",
       main = paste("Ajuste Variograma Gustkmh -", modelo),
       xlab = "Distancia", ylab = "Semivarianza")
  lines(modelo_variograma_Gustkmh, col = "darkred", lwd = 2)
  
  print(modelo_variograma_ffkmh)
  print(modelo_variograma_Gustkmh)
}

mtext("Comparación de Modelos de Variograma (Sin Efecto Pepita, Max Dist = 60,000)", 
      outer = TRUE, cex = 1.5, font = 2)

# Ajustar el modelo Matérn a ffkmh
modelo_mat_ffkmh <- variofit(variograma_ffkmh, cov.model = "mat", 
                             ini.cov.pars = c(1, 30000), # Sill y rango inicial
                             kappa = 0.5, # Suavidad inicial
                             nugget = 0) # Sin efecto pepita

# Ajustar el modelo Matérn a Gustkmh
modelo_mat_Gustkmh <- variofit(variograma_Gustkmh, cov.model = "mat", 
                               ini.cov.pars = c(1, 30000),
                               kappa = 0.5, 
                               nugget = 0)

# Mostrar los parámetros estimados
print(modelo_mat_ffkmh)
print(modelo_mat_Gustkmh)



# Cargar librerías necesarias
library(gstat)
library(sp)

# Definir las coordenadas espaciales
coordinates(datos_transformados) <- ~x + y

# Calcular variogramas empíricos
variograma_ffkmh <- variogram(ffkmh ~ 1, datos_transformados)
variograma_Gustkmh <- variogram(Gustkmh ~ 1, datos_transformados)
variograma_cruzado <- variogram(ffkmh ~ Gustkmh, datos_transformados)

# Crear un objeto de tipo 'gstat' para co-kriging
modelo_co_variograma <- gstat(NULL, id = "ffkmh", formula = ffkmh ~ 1, data = datos_transformados) %>%
  gstat(id = "Gustkmh", formula = Gustkmh ~ 1, data = datos_transformados)

# Unir los variogramas en un solo objeto de clase 'gstatVariogram'
variograma_conjunto <- variogram(modelo_co_variograma)

# Definir modelos teóricos de variograma (Matérn sin efecto pepita)
modelo_ffkmh <- vgm(psill = 42.1652, model = "Mat", range = 34062.4704, nugget = 0, kappa = 0.5)
modelo_Gustkmh <- vgm(psill = 83.0845, model = "Mat", range = 100988.6927, nugget = 0, kappa = 0.5)
modelo_cruzado <- vgm(psill = sqrt(42.1652 * 83.0845), model = "Mat", range = 34062.4704, nugget = 0, kappa = 0.5)

# Agregar modelos teóricos al objeto 'gstat'
modelo_co_variograma <- gstat(modelo_co_variograma, id = "ffkmh", model = modelo_ffkmh) %>%
  gstat(id = "Gustkmh", model = modelo_Gustkmh) %>%
  gstat(id = c("ffkmh", "Gustkmh"), model = modelo_cruzado)

# Ajustar el modelo de co-variograma con los variogramas empíricos
modelo_co_variograma_fit <- fit.lmc(variograma_conjunto, modelo_co_variograma, fit.method = 7)

# Mostrar los parámetros ajustados
print(modelo_co_variograma_fit)


# Extraer parámetros del modelo ajustado
params <- modelo_co_variograma_fit$model
print(params)  # Verificar los parámetros extraídos


# Función para calcular covarianza a partir del variograma
calc_cov <- function(h, modelos) {
  cov_value <- 0  # Inicializar covarianza en 0
  
  for (i in 1:nrow(modelos)) {  # Iterar sobre cada componente del modelo
    model_type <- modelos$model[i]
    psill <- modelos$psill[i]
    range <- modelos$range[i]
    nugget <- ifelse(model_type == "Nug", psill, 0)  # El nugget es una constante
    
    if (model_type == "Mat") {
      kappa <- modelos$kappa[i]
      cov_value <- cov_value + psill * (1 - (h / range) * exp(-h / range))
    } else if (model_type == "Nug") {
      cov_value <- cov_value + nugget  # El nugget es simplemente sumado
    } else {
      stop(paste("Modelo no soportado:", model_type))
    }
  }
  
  return(cov_value)
}



# Extraer parámetros del modelo ajustado
params <- modelo_co_variograma_fit$model
print(params)  # Verificar qué contiene 'params'

# Obtener coordenadas como matriz numérica
coords_obs <- as.matrix(coordinates(datos_transformados))
coords_pred <- as.matrix(coordinates(grilla_pred))

# Obtener dimensiones
n <- nrow(coords_obs)
m <- nrow(coords_pred)

# Construir la matriz de covarianza entre puntos observados (C)
C <- matrix(0, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    h <- sqrt(sum((coords_obs[i, ] - coords_obs[j, ])^2))
    C[i, j] <- calc_cov(h, params[[1]])  # Acceder correctamente al modelo
  }
}

# Construir la matriz de covarianza entre los puntos observados y la grilla de predicción (C0)
C0 <- matrix(0, n, m)
for (i in 1:n) {
  for (j in 1:m) {
    h <- sqrt(sum((coords_obs[i, ] - coords_pred[j, ])^2))
    C0[i, j] <- calc_cov(h, params$ffkmh)  # Selecciona el modelo correcto
  }
}

# Construir la matriz de covarianza de los puntos de predicción (C00)
C00 <- matrix(0, m, m)
# Calcular la matriz de distancias entre puntos de predicción
dist_pred <- as.matrix(dist(coords_pred))

# Aplicar la función de covarianza a todas las distancias
C00 <- matrix(calc_cov(dist_pred, params$ffkmh), nrow = m, ncol = m)

epsilon <- 1e-6  # Pequeño valor para estabilizar la inversa
C_reg <- C + diag(epsilon, n)
lambda <- solve(C_reg) %*% C0

# Resolver el sistema de cokriging: lambda = solve(C) %*% C0
lambda <- solve(C) %*% C0  # Pesos de interpolación

# Calcular la predicción en la grilla
z_obs <- datos_transformados$ffkmh  # Valores observados
z_pred <- t(lambda) %*% z_obs  # Predicciones

# Calcular la varianza de predicción (incertidumbre)
varianza_pred <- diag(C00 - t(lambda) %*% C0)

# Agregar predicciones a la grilla
grilla_pred$prediccion <- z_pred
grilla_pred$varianza <- varianza_pred

# Mostrar resultados
head(grilla_pred)



library(ggplot2)

# Convertir grilla_pred a data.frame y asegurar que los nombres de columna sean correctos
grilla_pred <- as.data.frame(grilla_pred)
colnames(grilla_pred) <- make.names(colnames(grilla_pred))  # Asegurar nombres válidos

# Verificar estructura de los datos
str(grilla_pred)

# Gráfico de predicción
p1 <- ggplot(grilla_pred, aes(x = x, y = y, fill = prediccion)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  coord_fixed() +
  labs(title = "Predicción de ffkmh",
       fill = "Predicción") +
  theme_minimal()

# Gráfico de varianza (incertidumbre)
p2 <- ggplot(grilla_pred, aes(x = x, y = y, fill = varianza)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno") +
  coord_fixed() +
  labs(title = "Varianza de Predicción",
       fill = "Varianza") +
  theme_minimal()

# Mostrar gráficos
print(p1)
print(p2)
