install.packages("climate")
library(remotes)
library(climate)
install_github("bczernecki/climate")

library(timechange)
o = metetimechangeo = meteo_ogimet(date = c(Sys.Date() - 5, Sys.Date() - 1), 
                 interval = "hourly",
                 coords = FALSE, 
                 station = 12330)
head(o)



###
library(climate)
library(dplyr)
library(lubridate)

# Definir las fechas (ajusta según tus necesidades)
fecha_inicio <- Sys.Date() - 60
fecha_fin <- Sys.Date() - 1

# Obtener una lista de las estaciones en Francia (ajusta el número de estaciones)
estaciones_francia <- nearest_stations_ogimet(country = "France", 
                                              date = Sys.Date(),
                                              no_of_stations = 100)

# Crear una lista para almacenar los datos de cada estación
datos_estaciones <- list()

# Iterar sobre las estaciones y descargar los datos
for (i in 1:nrow(estaciones_francia)) {
  station_id <- estaciones_francia$wmo_id[i]
  datos_estaciones[[i]] <- meteo_ogimet(station = station_id,
                                        interval = "hourly",
                                        date = c(fecha_inicio, fecha_fin))
}

# Combinar los datos de todas las estaciones en un solo data frame
datos_combinados <- bind_rows(datos_estaciones)
library(dplyr)

datos_estaciones <- lapply(datos_estaciones, function(df) {
  df$station_ID <- as.character(df$station_ID)
  return(df)
})

final_data <- bind_rows(data_list)

# Ordenar por fecha y hora
datos_combinados <- datos_combinados %>%
  arrange(Date)

# Agregar columna con el nombre de la estación (si está disponible en los datos)
# ... (aquí puedes agregar más columnas si lo deseas)

# Visualizar los primeros registros
head(datos_combinados)
save(datos_combinados, file = "mis_datos_meteorologicos.RData")

load("mis_datos_meteorologicos.RData")
# Convertir station_ID a caracter
datos_combinados$station_ID <- paste0("0", datos_combinados$station_ID)
datos_combinados$station_ID <- as.character(datos_combinados$station_ID)

# Unir los dataframes
datos_combinados <- datos_combinados %>%
  left_join(estaciones_francia, by = c("station_ID" = "wmo_id"))
