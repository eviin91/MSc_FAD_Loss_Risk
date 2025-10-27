# Librerías
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(dplyr)
library(purrr)
library(patchwork)


# Archivos raster
archivos <- c(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230901.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231001.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231101.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231201.tif"
)



meses <- c("August", "September", "October", "November", "December")

# Bounding box de Mallorca
mallorca_lonlat <- st_as_sfc(st_bbox(c(xmin = 2, xmax = 4, ymin = 39, ymax = 40.5), crs = 4326))

# Leer y recortar los rasters

mallorca_laea <- st_transform(mallorca_lonlat, crs(r))
bbox_m <- st_bbox(mallorca_laea)



i = 5

file <- archivos[[i]]
r <- raster(file)
# r[r < 0] <- NA
r <- crop(r, extent(bbox_m["xmin"], bbox_m["xmax"], bbox_m["ymin"], bbox_m["ymax"]))
plot(r)

r_stack[[i]] <- r

# Asignar nombres a cada capa
names(r_stack) <- meses

plot(r_stack)

library(raster)

raster::writeRaster(r_stack, "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/barcos.tif")

r_stack <- terra::rast("C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/barcos.tif")
terra::plot(r_stack)

r_stack <- log1p(r_stack)
terra::plot(r_stack)








# ship 
r_stack1
# waves
r_stack2

terra::plot(r_stack1)
terra::plot(r_stack2)


r_final <- r_stack1 + r_stack2 + r_stack3
terra::plot(r_final)



vmin <- min(r_stack)  # mínimo global del stack
vmax <- max(r_stack)  # máximo global del stack

# Normalizar y escalar a 0–5
values(r_stack) <- round((values(r_stack) - vmin) / (vmax - vmin) * 5)

# Visualizar
terra::plot(r_stack)


terra::plot(r)

vmin <- min(r)
vmax <- max(r)

# Reclasificar todos los valores a 0–5 según su posición en el rango
r <- round((r - 0) / (25 - 0) * 5)




































# Archivos raster (Agosto–Diciembre)
archivos <- c(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230901.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231001.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231101.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231201.tif"
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

str(df_)

stack()

# Plot facetado log1p alto contraste
ggplot() +
  geom_raster(data = df_all, aes(x = x, y = y, fill = log1p(value))) +
  geom_sf(data = world_laea, fill = "grey80", color = "white") +
  scale_fill_viridis(option = "plasma", 
                     limits = c(0, 11),           # rango completo de tu paleta
                     trans = "sqrt",               # estira valores bajos y comprime altos
                     breaks = c(0, 1, 2, 4, 6, 8, 11),
                     labels = comma) +
  coord_sf(xlim = c(bbox_all["xmin"], bbox_all["xmax"]),
           ylim = c(bbox_all["ymin"], bbox_all["ymax"]),
           expand = FALSE) +
  scale_y_continuous(breaks = seq(39, 40.5, 0.5)) +
  scale_x_continuous(breaks = seq(2, 4, 0.5)) +
  facet_wrap(~ mes) +
  labs(fill = "Vessel Density h/km2 (log1p)") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_text(size = 6),
    axis.text.y = element_text(angle = 90, vjust = 0.5)  # 🔹 latitudes en vertical
  )
  




















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
# NORMALIZED VESSEL DENSITY MAPS (AUG–DEC 2023) HIGH CONTRAST
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
library(terra)
library(sf)
library(rnaturalearth)
library(viridis)

#-------------------------------------------
# Archivos y nombres de meses
archivos <- c(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230901.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231001.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231101.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231201.tif"
)
meses <- c("August","September","October","November","December")

# Bounding box Mallorca
mallorca_sf <- st_as_sfc(st_bbox(c(xmin=2, xmax=4, ymin=39, ymax=40.5), crs=4326))

# Mundo
world <- rnaturalearth::ne_countries(scale="large", returnclass="sf")


# Función para procesar un raster de un mes
process_plot <- function(archivo, mes) {
  
  # Leer raster y eliminar valores negativos
  r <- rast(archivo)
  r[r < 0] <- NA
  
  # Recorte Mallorca
  mallorca_laea <- st_transform(mallorca_sf, crs(r))
  r_crop <- crop(r, vect(mallorca_laea))
  
  # Log1p y normalización 0-1
  r_log <- log1p(r_crop)
  vals <- global(r_log, fun=c("min","max"), na.rm=TRUE)
  r_norm <- (r_log - vals[1,"min"]) / (vals[1,"max"] - vals[1,"min"])
  
  # Transformación sqrt para alto contraste
  r_final <- r_norm^0.5
  
  # Reproyectar mundo y recortar al raster
  world_laea <- st_transform(world, crs(r_final))
  r_bbox_sf <- st_as_sfc(st_bbox(r_final))
  world_crop <- st_crop(world_laea, r_bbox_sf)
  
  # Plot
  plot(r_final, col=viridis(100, option="plasma"), 
       main=paste("Vessel Density:", mes),
       axes=TRUE, legend=TRUE)
  plot(st_geometry(world_crop), add=TRUE, col="grey80", border="white")
}

output_dir <- "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/VesselDensityPlots/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Generar mapas de todos los meses
for(i in seq_along(archivos)) {
  process_plot(archivos[i], meses[i])
}


# Función GRÁFICO MEAN POR MES PARA VALORES >0 SIN NA
library(dplyr)

monthly_mean_density <- df_all %>%
  group_by(mes) %>%
  summarise(mean_density = mean(value, na.rm = TRUE)) %>%
  arrange(factor(mes, levels = month.name))

monthly_mean_density


library(ggplot2)
library(scales)  # para format comma

# Usando monthly_mean_density
ggplot(monthly_mean_density, aes(x = mes, y = mean_density)) +
  geom_col(fill = "grey50", width = 0.5) +  # Barras más finas
  labs(
    x = NULL,
    y = "Mean Vessel Density (h/km²)",
    title = "Mean Vessel Density in Mallorca (Aug–Dec)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),   # Sin grid
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12)
  )

#Tabla con SD, max min etc.
monthly_summary_density <- df_all %>%
  group_by(mes) %>%
  summarise(
    mean_density = mean(value, na.rm = TRUE),
    sd_density   = sd(value, na.rm = TRUE),
    min_density  = min(value, na.rm = TRUE),
    max_density  = max(value, na.rm = TRUE)
  ) %>%
  arrange(factor(mes, levels = month.name))

monthly_summary_density

#Total densities
library(dplyr)

monthly_total_density <- df_all %>%
  group_by(mes) %>%
  summarise(
    total_density = sum(value, na.rm = TRUE)
  ) %>%
  arrange(match(mes, month.name))

monthly_total_density


#5 CLASSES#

# Crear columna mes como factor para controlar orden
mes_levels <- c("August","September","October","November","December","Mean Aug-Dec")

df_all$mes <- factor(df_all$mes, levels = mes_levels[1:5])  # meses originales

# Calcular mean August-December
df_mean <- df_all %>%
  group_by(x, y) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mes = "Mean Aug-Dec") %>%
  mutate(density_cat = case_when(
    value < 1           ~ "Very Low",
    value >= 1 & value < 4  ~ "Low",
    value >= 4 & value < 7  ~ "Moderate",
    value >= 7 & value < 10  ~ "High",
    value >= 10             ~ "Very High"
  )) %>%
  mutate(density_cat = factor(density_cat, 
                              levels = c("Very Low","Low","Moderate","High","Very High")))

# Unir con df_all original
df_all_aug_dec <- bind_rows(df_all, df_mean)

# Ajustar factor para que aparezca en el orden deseado
df_all_aug_dec$mes <- factor(df_all_aug_dec$mes, levels = mes_levels)

# Plot
ggplot(df_all_aug_dec, aes(x = x, y = y, fill = density_cat)) +
  geom_raster() +
  geom_sf(data = world_laea, fill = "grey80", color = "white", inherit.aes = FALSE) +
  scale_fill_manual(
    values = c(
      "Very Low"  = "#66AA33",
      "Low"       = "#A6D854",
      "Moderate"  = "#FFDD33",
      "High"      = "#F46D43",
      "Very High" = "#D73027"
    ),
    name = "Vessel Density"
  ) +
  coord_sf(
    xlim = c(bbox_all["xmin"], bbox_all["xmax"]),
    ylim = c(bbox_all["ymin"], bbox_all["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(~ mes) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold", size = 10)  # títulos en negrita
  )
