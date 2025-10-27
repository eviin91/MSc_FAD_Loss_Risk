
#---------------------------------------------------
#FUERA DEL BUFFER: VESSEL DENSITY EN TODA MALLORCA
#---------------------------------------------------
library(raster)
library(sf)
library(rnaturalearth)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)

#VALENTINA:

# Cargar raster
r_raster <- raster("C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_Vessel_Density2017-2023_Lostbuoys/EMODnet_Vessel_Density_2017-2023_Lostbuoys/vesseldensity_08_20221101.tif")

# Opción: no eliminar ceros, solo valores negativos imposibles
r_raster[r_raster < 0] <- NA

# Cargar mapa del mundo y transformarlo al CRS del raster
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
world_laea <- st_transform(world, crs(r_raster))

# Crear bbox de Mallorca en lon/lat y transformarlo al CRS del raster
mallorca_lonlat <- st_as_sfc(st_bbox(c(xmin = 2, xmax = 4, ymin = 39, ymax = 40.5), crs = 4326))
mallorca_laea <- st_transform(mallorca_lonlat, crs(r_raster))

# Obtener bbox numérico para recortar
mallorca_bbox <- st_bbox(mallorca_laea)

# Recortar raster a Mallorca
crop_mallorca <- crop(r_raster, 
                      extent(mallorca_bbox["xmin"], mallorca_bbox["xmax"],
                             mallorca_bbox["ymin"], mallorca_bbox["ymax"]))

traffic_mean_mallorca <- mean(values(crop_mallorca), na.rm = TRUE)
traffic_mean_mallorca

#ULISES
library(terra)
library(sf)
library(dplyr)

#-----------------------------------------
# Definir bounding box de Mallorca
#-----------------------------------------
mallorca_lonlat <- st_as_sfc(
  st_bbox(c(xmin = 2, xmax = 4, ymin = 39, ymax = 40.5), crs = 4326)
)

#-----------------------------------------
# Carpeta con los rasters de EMODnet
#-----------------------------------------
r_raster <- "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/"

# Seleccionar todos los archivos de julio (2017–2023)
traffic_files_july <- list.files(
  r_raster, pattern = "202307|201707|201807|201907|202007|202107|202207", 
  full.names = TRUE
)
traffic_files_july <- sort(traffic_files_july)

#-----------------------------------------
# Calcular la media dentro del bbox de Mallorca
#-----------------------------------------
traffic_df <- data.frame()

for (f in traffic_files_july) {
  r <- rast(f)
  r[r < 0] <- NA  # eliminar valores imposibles
  
  # Reproyectar bbox a CRS del raster
  mallorca_proj <- st_transform(mallorca_lonlat, crs(r))
  
  # Recortar raster a Mallorca
  crop_r <- crop(r, vect(mallorca_proj))
  
  # Calcular media del tráfico
  mean_density <- mean(values(crop_r), na.rm = TRUE)
  
  # Extraer fecha del nombre del archivo
  date_str <- sub(".*_(\\d{8}).tif$", "\\1", basename(f))
  date <- as.Date(paste0(substr(date_str,1,6),"01"), format="%Y%m%d")
  
  traffic_df <- rbind(traffic_df, data.frame(date = date, mean_density = mean_density))
}

# Media total de tráfico (Mallorca, julio 2017–2023)
traffic_mean_mallorca <- mean(traffic_df$mean_density, na.rm = TRUE)
traffic_mean_mallorca


#-----------------------------------------
# CON PEQUEÑO BUFFER (Ulises)
#-----------------------------------------
ulises_point <- st_sfc(
  st_point(c(mean(c(2 + 39.817/60, 2 + 37.367/60)), 
             mean(c(39 + 52.333/60, 39 + 57.217/60)))),
  crs = 4326
)

ulises_buffer <- st_transform(ulises_point, 32631) %>%  # reproyectar a UTM 31N
  st_buffer(500) %>%                   # 500 m
  st_transform(4326)                   # volver a lon/lat

#-----------------------------------------
# Carpeta con los rasters de EMODnet
#-----------------------------------------
r_raster <- "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/"

# Seleccionar todos los archivos de julio de cada año
traffic_files_july <- list.files(
  r_raster, pattern = "202307|201707|201807|201907|202007|202107|202207|202307", 
  full.names = TRUE
)
traffic_files_july <- sort(traffic_files_july)  # por orden cronológico

#-----------------------------------------
# Función para extraer media dentro del buffer
#-----------------------------------------
extract_mean_density_buffer <- function(file_path, buffer) {
  r <- rast(file_path)
  
  # Reproyectar solo si es necesario
  if (crs(r, proj=TRUE) != "EPSG:4326") {
    r <- project(r, "EPSG:4326")
  }
  
  crop_r <- crop(r, vect(buffer))
  vals <- values(crop_r)
  vals[vals < 0] <- NA
  mean_density <- mean(vals, na.rm = TRUE)
  
  # Extraer fecha del archivo
  date_str <- sub(".*_(\\d{8}).tif$", "\\1", basename(file_path))
  date <- as.Date(paste0(substr(date_str,1,6),"01"), format="%Y%m%d")
  
  return(data.frame(date = date, mean_density = mean_density))
}

#-----------------------------------------
# Aplicar la función a todos los archivos
#-----------------------------------------
traffic_df <- do.call(rbind, lapply(traffic_files_july, extract_mean_density_buffer, buffer = ulises_buffer))

#-----------------------------------------
# Resumen estadístico
#-----------------------------------------
summary(traffic_df$mean_density)
sd(traffic_df$mean_density, na.rm=TRUE)

#Resumen sin tener en cuenta 2021 (outlier)
traffic_df_filtered <- traffic_df %>%
  filter(date != as.Date("2021-07-01"))

traffic_summary_filtered <- traffic_df_filtered %>%
  summarise(
    mean_density_avg = mean(mean_density, na.rm = TRUE),
    mean_density_sd  = sd(mean_density, na.rm = TRUE),
    min_density      = min(mean_density, na.rm = TRUE),
    max_density      = max(mean_density, na.rm = TRUE)
  )

traffic_summary_filtered


#-----------------------------------------
# Gráfico de comparación
#-----------------------------------------


ggplot(traffic_df, aes(x = date, y = mean_density)) +
  geom_point(size = 3, color = "#1f78b4") +  # puntos normales
  geom_line(color = "#1f78b4", linetype = "dashed") +
  geom_point(
    data = subset(traffic_df, date == as.Date("2021-07-01")),
    aes(x = date, y = mean_density),
    color = "red",
    size = 3
  ) +
  geom_hline(yintercept = mean_density_no2021, color = "red", linetype = "dotted", size = 1) +
  scale_x_date(
    breaks = traffic_df$date, 
    date_labels = "%Y"
  ) +
  labs(
    x = "Year", 
    y = "Mean Vessel Density (July)", 
    title = "July Vessel Density 2017-2023",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

library(terra)
library(sf)
library(dplyr)

#-----------------------------------------
# Coordenadas y buffer de la boya Valentina
#-----------------------------------------
valentina_point <- st_sfc(
  st_point(c(2.668416667, 39.86131667)),
  crs = 4326
)

valentina_buffer <- st_transform(valentina_point, 32631) %>%  # reproyectar a UTM 31N
  st_buffer(500) %>%                        # buffer de 500 m
  st_transform(4326)                        # volver a lon/lat

#-----------------------------------------
# Archivo raster de noviembre 2022
#-----------------------------------------
valentina_file <- "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20221101.tif"

#-----------------------------------------
# Leer raster y extraer valor medio dentro del buffer
#-----------------------------------------
r_valentina <- rast(valentina_file)

# Reproyectar solo si no está en EPSG:4326
if (crs(r_valentina, proj=TRUE) != "EPSG:4326") {
  r_valentina <- project(r_valentina, "EPSG:4326")
}

# Recortar y calcular media
crop_valentina <- crop(r_valentina, vect(valentina_buffer))
vals_valentina <- values(crop_valentina)
vals_valentina[vals_valentina < 0] <- NA
mean_density_val22 <- mean(vals_valentina, na.rm = TRUE)

mean_density_val22
