#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# VESSEL DENSITY MAP OF ONE MONTH

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Librerías
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(scales)

# Cargar raster
r_raster <- raster("C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif")

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

# Convertir a data.frame para ggplot
r_df_mallorca <- as.data.frame(crop_mallorca, xy = TRUE)
colnames(r_df_mallorca)[3] <- "value"
r_df_mallorca <- na.omit(r_df_mallorca)

# Plot transformed to log1p con alto contraste


ggplot() +
  geom_raster(data = r_df_mallorca, aes(x = x, y = y, fill = value)) +
  geom_sf(data = world_laea, fill = "grey80", color = "white") +
  scale_fill_viridis(
    option = "plasma",
    limits = c(0, 11),           # rango completo de tu paleta
    trans = "sqrt",               # estira valores bajos y comprime altos
    breaks = c(0, 1, 2, 4, 6, 8, 11),
    labels = comma
  ) +
  coord_sf(xlim = c(mallorca_bbox["xmin"], mallorca_bbox["xmax"]),
           ylim = c(mallorca_bbox["ymin"], mallorca_bbox["ymax"]),
           expand = FALSE) +
  theme_minimal() +
  labs(fill = "log1p August Vessel density (h/km2)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# PUT TOGETHER 5 MONTHS IN ONE PLOT

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Librerías
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(dplyr)
library(purrr)
library(patchwork)

# Archivos raster (Agosto–Diciembre)
archivos <- c(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230901.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231001.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231101.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231201.tif"
)

# Nombres de los meses correspondientes
meses <- c("August", "September", 
           "October", "November", 
           "December")

# Mapa base del mundo
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

# Bounding box de Mallorca
mallorca_lonlat <- st_as_sfc(st_bbox(c(xmin = 2, xmax = 4, ymin = 39, ymax = 40.5), crs = 4326))

# Función para procesar cada raster
procesar_raster <- function(ruta, nombre_mes) {
  r <- raster(ruta)
  r[r < 0] <- NA
  
  # reproyectar mundo al CRS del raster
  world_laea <- st_transform(world, crs(r))
  mallorca_laea <- st_transform(mallorca_lonlat, crs(r))
  mallorca_bbox <- st_bbox(mallorca_laea)
  
  crop_mallorca <- crop(r, 
                        extent(mallorca_bbox["xmin"], mallorca_bbox["xmax"],
                               mallorca_bbox["ymin"], mallorca_bbox["ymax"]))
  
  r_df <- as.data.frame(crop_mallorca, xy = TRUE)
  colnames(r_df)[3] <- "value"
  r_df <- na.omit(r_df)
  r_df$mes <- nombre_mes
  return(list(df = r_df, world = world_laea, bbox = mallorca_bbox))
}

# Procesar todos los archivos
procesados <- map2(archivos, meses, procesar_raster)

# Unir dataframes
df_all <- bind_rows(map(procesados, "df"))
bbox_all <- procesados[[1]]$bbox
world_laea <- procesados[[1]]$world
df_all$mes <- factor(df_all$mes, levels = month.name)


# Plot facetado log1p alto contraste
ggplot() +
  geom_raster(data = df_all, aes(x = x, y = y, fill = log1p(value))) +
  geom_sf(data = world_laea, fill = "grey80", color = "white") +
  scale_fill_viridis(option = "viridis", 
                     limits = c(0, 11),           # rango completo de tu paleta
                     trans = "sqrt",               # estira valores bajos y comprime altos
                     breaks = c(0, 1, 2, 4, 6, 8, 11),
                     labels = comma) +
  coord_sf(xlim = c(bbox_all["xmin"], bbox_all["xmax"]),
           ylim = c(bbox_all["ymin"], bbox_all["ymax"]),
           expand = FALSE) +
  facet_wrap(~ mes) +
  theme_minimal() +
  labs(fill = "Vessel Density h/km2 (log1p)")

  
#Sacar el mapa promedio

library(raster)
library(sf)
library(ggplot2)
library(viridis)

archivos <- c(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230901.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231001.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231101.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231201.tif"
)


# Cargar todos los raster en una lista
rasters <- lapply(archivos, raster)

# Convertir valores negativos a NA en todos los raster
rasters <- lapply(rasters, function(x) { x[x < 0] <- NA; x })

# Calcular el promedio mensual
r_promedio <- stack(rasters) %>% calc(fun = mean, na.rm = TRUE)

# Recortar a Mallorca (usando tu bbox ya transformado)
crop_mallorca <- crop(r_promedio, extent(mallorca_bbox["xmin"], mallorca_bbox["xmax"],
                                         mallorca_bbox["ymin"], mallorca_bbox["ymax"]))

 # Convertir a data.frame para ggplot
r_df_mallorca <- as.data.frame(crop_mallorca, xy = TRUE)
colnames(r_df_mallorca)[3] <- "value"
r_df_mallorca <- na.omit(r_df_mallorca)


#Plot con log1p


ggplot() +
  geom_raster(data = r_df_mallorca, aes(x = x, y = y, fill = value)) +
  geom_sf(data = world_laea, fill = "grey80", color = "white") +
  scale_fill_viridis(
    option = "plasma",
    limits = c(0, 11),           # rango completo de tu paleta
    trans = "sqrt",               # estira valores bajos y comprime altos
    breaks = c(0, 1, 2, 4, 6, 8, 11),
    labels = comma
  ) +
  coord_sf(xlim = c(mallorca_bbox["xmin"], mallorca_bbox["xmax"]),
           ylim = c(mallorca_bbox["ymin"], mallorca_bbox["ymax"]),
           expand = FALSE) +
  theme_minimal() +
  labs(fill = "Densidad promedio mensual\n(h/km²/mes)")

# Plot (leyenda con valores extraños)
ggplot() +
  geom_raster(data = r_df_mallorca, aes(x = x, y = y, fill = value)) +
  geom_sf(data = world_laea, fill = "grey80", color = "white") +
  scale_fill_viridis(option = "plasma", trans = "log", na.value = NA) +
  coord_sf(xlim = c(mallorca_bbox["xmin"], mallorca_bbox["xmax"]),
           ylim = c(mallorca_bbox["ymin"], mallorca_bbox["ymax"]),
           expand = FALSE) +
  theme_minimal() +
  labs(fill = "Densidad promedio mensual\n(horas/km²/mes)")
------------------------------------------------------------------------

library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(purrr)
library(patchwork)

# Archivos raster (Agosto–Diciembre)
archivos <- c(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230901.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231001.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231101.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231201.tif"
)

meses <- c("August", "September", "October", "November", "December")

# 1. Leer y apilar
rasters <- stack(archivos)
rasters[rasters < 0] <- NA   # quitar valores imposibles

# 2. Calcular min y max global
valores <- values(rasters)
min_global <- min(valores, na.rm = TRUE)
max_global <- max(valores, na.rm = TRUE)

# 3. Normalizar
rasters_norm <- (rasters - min_global) / (max_global - min_global)

# 4. Pasar a data.frame para ggplot
df_all <- as.data.frame(rasters_norm, xy = TRUE)
colnames(df_all)[-(1:2)] <- meses
df_long <- tidyr::pivot_longer(df_all, cols = meses, names_to = "mes", values_to = "value")

# 5. Plot facetado normalizado
ggplot(df_long, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_viridis(name = "Vessel Density (norm)", option = "plasma") +
  facet_wrap(~ mes) +
  theme_minimal() +
  labs(title = "Normalized Vessel Density (Aug–Dec 2023)")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# NORMALIZED VESSEL DENSITY MAPS (AUG–DEC 2023)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(raster)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)

# 1. Buscar automáticamente los TIFF en la subcarpeta
archivos <- list.files(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023",
  pattern = "2023(08|09|10|11|12).*\\.tif$",
  full.names = TRUE
)

# Verificar archivos encontrados
print(archivos)

# 2. Cargar y apilar
rasters <- stack(archivos)
rasters[rasters < 0] <- NA   # quitar valores imposibles

# 3. Calcular min y max global
valores <- values(rasters)
min_global <- min(valores, na.rm = TRUE)
max_global <- max(valores, na.rm = TRUE)

# 4. Normalizar (0–1)
rasters_norm <- (rasters - min_global) / (max_global - min_global)

# 5. Pasar a data.frame para ggplot
meses <- c("August", "September", "October", "November", "December")
df_all <- as.data.frame(rasters_norm, xy = TRUE)
colnames(df_all)[-(1:2)] <- meses
df_long <- tidyr::pivot_longer(df_all, cols = all_of(meses),
                               names_to = "mes", values_to = "value")
# 6. Plot facetado
ggplot(df_long, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_viridis(name = "Vessel Density (norm)", option = "plasma") +
  facet_wrap(~ mes) +
  theme_minimal() +
  labs(title = "Normalized Vessel Density (Aug–Dec 2023)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MAPA PROMEDIO NORMALIZADO
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

r_mean_norm <- calc(rasters_norm, mean, na.rm = TRUE)

df_mean <- as.data.frame(r_mean_norm, xy = TRUE)
colnames(df_mean)[3] <- "value"

ggplot(df_mean, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_viridis(name = "Mean Vessel Density (norm)", option = "plasma") +
  theme_minimal() +
  labs(title = "Normalized Mean Vessel Density (Aug–Dec 2023)")

