#regresión y análisis de Áreas 
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(viridis)
library(spdep)
library(spatialreg)
library(car)       # Para VIF
library(lmtest) 

Datos=read.csv("sids2.csv")
# Cargar los datos espaciales
sids2 <- st_read("sids2.shp")

# Quitar geometría antes de pivotar
data_long <- sids2 %>%
  st_drop_geometry() %>%
  select(AREA, BIR74, SID74, NWBIR74, BIR79, SID79, NWBIR79, SIDR74, SIDR79, NWR74, NWR79) %>%
  pivot_longer(cols = -AREA, names_to = "Variable", values_to = "Valor")

# Unir los datos transformados con la geometría original (solución al many-to-many)
sids2_long <- data_long %>%
  left_join(sids2 %>% select(AREA, geometry) %>% distinct(), by = "AREA")

summary(sids2 %>% 
          st_drop_geometry() %>% 
          select(BIR74, SID74, NWBIR74, BIR79, SID79, NWBIR79, SIDR74, SIDR79, NWR74, NWR79))

# Crear gráficos separados para cada variable
output_dir <- "graficos_variables"  # Carpeta donde guardar los gráficos
dir.create(output_dir, showWarnings = FALSE)  # Crear carpeta si no existe

for (var in unique(sids2_long$Variable)) {
  plot_data <- sids2_long %>% filter(Variable == var)
  
  p <- ggplot(data = plot_data) +
    geom_sf(aes(fill = Valor, geometry = geometry), color = "black", size = 0.1) +
    scale_fill_viridis(option = "C", direction = -1) +
    theme_minimal() +
    labs(title = paste("Observaciones por Área -", var), fill = "Valor")
  
  # Guardar gráfico
  ggsave(filename = paste0(output_dir, "/", var, ".png"), plot = p, width = 8, height = 6, dpi = 300)
}
#centroides
Centros=st_centroid(sids2)
#distancias 
m_distancias=st_distance(Centros)

#matriz de vecindades queen 
vecindades=poly2nb(sids2, queen = TRUE)

#diferentes matrices de ponderación 
# Alternativamente, usa diferentes estilos de ponderación
sids2.lw <- nb2listw(vecindades, zero.policy = TRUE )
sids2.lwb <- nb2listw(vecindades, style = "B",zero.policy = TRUE)
sids2.lwc <- nb2listw(vecindades, style = "C",zero.policy = TRUE)
sids2.lwu <- nb2listw(vecindades, style = "U",zero.policy = TRUE)
sids2.lww <- nb2listw(vecindades, style = "W",zero.policy = TRUE)

## Modelo suponiendo independencia:
# Ajustar modelo OLS (sin correlación espacial)
modelo_ols <- lm(SIDR79 ~ NWBIR79 + NWR79 + AREA + PERIMETER + BIR79, data = sids2)
summary(modelo_ols)

# Obtener residuos del modelo OLS
residuos <-as.numeric(residuals(modelo_ols))

# Lista de matrices de pesos espaciales
matrices_pesos <- list("W" = sids2.lw, "B" = sids2.lwb, "C" = sids2.lwc, 
                       "U" = sids2.lwu, "WW" = sids2.lww)

# Aplicar test de Moran para evaluar autocorrelación espacial en los residuos
resultados_moran <- lapply(matrices_pesos, function(w) moran.test(residuos, listw = w, zero.policy = TRUE))

# Extraer p-valores y seleccionar la mejor matriz de pesos (menor p-valor)
pvalores <- sapply(resultados_moran, function(r) r$p.value)
mejor_matriz_nombre <- names(which.min(pvalores))
mejor_matriz <- matrices_pesos[[mejor_matriz_nombre]]

cat("\n🔹 La mejor matriz de pesos seleccionada es:", mejor_matriz_nombre, "\n")

# Ajustar el modelo SAR (Spatial Autoregressive Model)
modelo_sar <- lagsarlm(SIDR79 ~ NWBIR79 + NWR79 + AREA + PERIMETER + BIR79, 
                       data = sids2, listw = mejor_matriz, zero.policy = TRUE)
summary(modelo_sar)

# Ajustar el modelo SDEM (Spatial Durbin Error Model)
modelo_sdem <- errorsarlm(SIDR79 ~ NWBIR79 + NWR79 + AREA + PERIMETER + BIR79, 
                          data = sids2, listw = mejor_matriz, etype = "emixed", zero.policy = TRUE)
summary(modelo_sdem)

# Ajustar el modelo Manski (SAC - Spatial Auto-Covariance)
modelo_manski <- sacsarlm(SIDR79 ~ NWBIR79 + NWR79 + AREA + PERIMETER + BIR79, 
                          data = sids2, listw = mejor_matriz, type = "sacmixed", zero.policy = TRUE)
summary(modelo_manski)

# Comparar los cuatro modelos con AIC
cat("\n--- Comparación de AIC entre modelos ---\n")
AIC_values <- AIC(modelo_ols, modelo_sar, modelo_sdem, modelo_manski)
print(AIC_values)


## con lo anterior el mejor modelo es el SAR
## construimos cual es el modelo mas adecuado en terminos 
## de las variables explicativas

# Ajustar el modelo SAR (Spatial Autoregressive Model)
modelo_sar_1 <- lagsarlm(SIDR79 ~ NWR79 + AREA + PERIMETER + BIR79, 
                       data = sids2, listw = mejor_matriz, zero.policy = TRUE)
summary(modelo_sar_1)


modelo_sar_2 <- lagsarlm(SIDR79 ~ NWR79  + PERIMETER + BIR79, 
                         data = sids2, listw = mejor_matriz, zero.policy = TRUE)
summary(modelo_sar_2)

modelo_sar_3 <- lagsarlm(SIDR79 ~ NWR79   + BIR79, 
                         data = sids2, listw = mejor_matriz, zero.policy = TRUE)
summary(modelo_sar_3)

AIC( modelo_sar,modelo_sar_1, modelo_sar_2, modelo_sar_3)

sids2$fit <- as.numeric(fitted(modelo_sar_3))

# Graficar mapa con valores ajustados
ggplot(sids2) +
  geom_sf(aes(fill = fit), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "C") +
  ggtitle("Valores ajustados mediante Modelo SAR") +
  theme_minimal()

# Extraer residuos
residuos <- residuals(modelo_sar_3)
residuos <- as.numeric(residuos)  # Convertir a numérico si es necesario

### 1. Normalidad de los residuos
# Histograma
ggplot(data.frame(residuos), aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histograma de los residuos", x = "Residuos", y = "Densidad") +
  theme_minimal()

# QQ-Plot
ggplot(data.frame(residuos), aes(sample = residuos)) +
  stat_qq() + 
  stat_qq_line() + 
  labs(title = "QQ-Plot de los residuos") +
  theme_minimal()

# Pruebas de normalidad
shapiro_test <- shapiro.test(residuos)  # Shapiro-Wilk
ks_test <- ks.test(residuos, "pnorm", mean(residuos), sd(residuos))  # Kolmogorov-Smirnov

cat("\n🔹 Prueba de normalidad Shapiro-Wilk: p-valor =", shapiro_test$p.value, "\n")
cat("\n🔹 Prueba de Kolmogorov-Smirnov: p-valor =", ks_test$p.value, "\n")

### 2. Homocedasticidad (Varianza constante)
# Gráfico de residuos vs. valores ajustados
valores_ajustados <- as.numeric(fitted(modelo_sar_3))

ggplot(data.frame(residuos, valores_ajustados), aes(x = valores_ajustados, y = residuos)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Residuos vs. Valores ajustados", x = "Valores ajustados", y = "Residuos") +
  theme_minimal()

# Prueba de Breusch-Pagan
residuos_sar <- residuals(modelo_sar_3)
ajustados_sar <- fitted(modelo_sar_3)

# Modelo auxiliar para Breusch-Pagan
modelo_bp <- lm(residuos_sar^2 ~ ajustados_sar)
bp_stat <- summary(modelo_bp)$r.squared * length(residuos_sar)
bp_pvalor <- 1 - pchisq(bp_stat, df = 1)

cat("\n🔹 Prueba de Breusch-Pagan (manual): p-valor =", bp_pvalor, "\n")


### 3. Autocorrelación espacial de los residuos (Moran's I)
moran_test <- moran.test(residuos, listw = mejor_matriz, zero.policy = TRUE)
cat("\n🔹 Prueba de Moran's I: p-valor =", moran_test$p.value, "\n")

### 4. Multicolinealidad (Factor de inflación de varianza - VIF)
# Nota: VIF se calcula para un modelo de regresión lineal
modelo_ols_reducido <- lm(SIDR79 ~ NWR79 + BIR79, data = sids2)
vif_values <- vif(modelo_ols_reducido)
print(vif_values)

# Interpretación:
# VIF > 10 -> Alta colinealidad
# VIF > 5  -> Posible problema de colinealidad