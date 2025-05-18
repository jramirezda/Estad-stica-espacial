# Cargar la librería geoR
library(geoR)

# Leer el archivo y especificar que la coma es el separador decimal
aquifer <- read.table("aquifer (1).txt", header = TRUE, sep = "", dec = ",", stringsAsFactors = FALSE)

# Verificar las primeras filas de los datos
head(aquifer)

# Verificar los tipos de datos
str(aquifer)

# Convertir el dataframe a un objeto geodata adecuado para geoR
# Suponiendo que 'easting' y 'northing' son las columnas de las coordenadas
# y 'head' es la variable dependiente en este caso
aquifer_geo <- as.geodata(aquifer, coords.col = c("easting", "northing"), data.col = "head")

# Resumen de los datos geoespaciales
summary(aquifer_geo)

# Graficar los datos geoespaciales
plot(aquifer_geo, qt.col = c("purple", "pink", "green", "yellow"))

# Ajustar el modelo de regresión lineal (modelo 1)
modelo1 <- lm(head ~ easting + northing, data = aquifer)
summary(modelo1)

# Crear un nuevo dataframe con los residuos del modelo
res1 <- cbind.data.frame(aquifer$easting, aquifer$northing, modelo1$residuals)

# Asignar nombres a las columnas del nuevo dataframe
colnames(res1) <- c("easting", "northing", "residuals")

# Ver las primeras filas del nuevo dataframe
head(res1)

res1=as.geodata(res1)
# Graficar los residuos
plot(res1, qt.col = c("purple", "pink", "green", "yellow"))



trend.spatial()##automatico hacer esto con el mejor modelo.

modelo2=lm(data~easting+I(easting^2)+northing, data = aquifer)
summary(modelo2)

modelo3=lm(data~easting+northing+I(northing^2), data = aquifer)
summary(modelo3)

modelo4=lm(data~easting+I(easting^2)+northing+I(northing^2), data = aquifer)
summary(modelo4)


##polinomial 
