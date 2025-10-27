#Tener cargados:
#VHM0_monthly (correr hasta linea 214 MeanWave_Months)
#vel_map_list2 (correr hasta linea 112 MeanCurrent_Normalized)

# ======================================================
# ÍNDICE DE RIESGO PONDERADO MENSUAL - MALLORCA 
# ======================================================

# --- PASO 0: LIBRERÍAS Y PREPARACIÓN ---
library(terra)
library(stars)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)
library(rnaturalearth)
library(rnaturalearthdata)

# Pesos definidos (ya calibrados)
w <- c(wave=0.885, curr=0.0002, traf=0.115)

# Bounding box Mallorca
mallorca_sf <- st_as_sfc(st_bbox(c(xmin=2, xmax=4, ymin=39, ymax=40.5), crs=4326))
mallorca_vect <- vect(mallorca_sf)
#Mallorca contour
world <- ne_countries(scale="medium", returnclass="sf")
spain <- world[world$name=="Spain", ]
mallorca_bbox <- st_bbox(c(xmin=lon_range[1], xmax=lon_range[2],
                           ymin=lat_range[1], ymax=lat_range[2]), crs=4326)
mallorca_contour <- st_crop(spain, mallorca_bbox)

# Meses y archivos correspondientes
meses <- c("August", "September", "October", "November", "December")
vessel_archivos <- c(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230901.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231001.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231101.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231201.tif"
)

# --- PASO 1: RASTER TEMPLATE (basado en VHM0) ---
x_vals <- sort(unique(VHM0_monthly$x))
y_vals <- sort(unique(VHM0_monthly$y))
template <- rast(
  xmin = min(x_vals), xmax = max(x_vals),
  ymin = min(y_vals), ymax = max(y_vals),
  ncols = length(x_vals), nrows = length(y_vals),
  crs = "EPSG:4326"
)
# --- FUNCION AUXILIAR PARA RELLENAR NAs ---
fill_NA_focal <- function(r) {
  focal(r, w = matrix(1,3,3), fun = mean, na.rm = TRUE, NAonly = TRUE)
}
# --- PASO 2: BUCLE MENSUAL ---
risk_rasters <- list()

for(i in seq_along(meses)){
  mes <- meses[i]
  message("Procesando ", mes, " ...")
  
  # --- OLEAJE ---
  wave_df <- VHM0_monthly %>% filter(month == (7 + i))
  wave_vect <- vect(wave_df, geom=c("x","y"), crs="EPSG:4326")
  wave_rast <- rasterize(wave_vect, template, field="VHM0_norm_avg", fun=mean)
  vals_wave <- values(wave_rast); vals_wave <- vals_wave[!is.na(vals_wave)]
  wave_rast <- (wave_rast - min(vals_wave)) / (max(vals_wave) - min(vals_wave))
  wave_rast <- fill_NA_focal(wave_rast)
  
  # --- CORRIENTES ---
  curr_df <- vel_map_list2[[i]]
  curr_vect <- vect(curr_df, geom=c("x","y"), crs="EPSG:4326")
  curr_rast <- rasterize(curr_vect, template, field="vel_norm", fun=mean)
  vals_curr <- values(curr_rast); vals_curr <- vals_curr[!is.na(vals_curr)]
  curr_rast <- (curr_rast - min(vals_curr)) / (max(vals_curr) - min(vals_curr))
  curr_rast <- fill_NA_focal(curr_rast)
  
  # ===TRÁFICO MARÍTIMO ===
  vessel_r <- rast(vessel_archivos[i])
  vessel_r[vessel_r < 0] <- NA
  
  # Proyectar Mallorca al CRS del raster
  mallorca_vect_proj <- project(mallorca_vect, crs(vessel_r))
  vessel_crop <- crop(vessel_r, mallorca_vect_proj)
  # Rellenar NAs cercanos a bordes
  vessel_crop <- fill_NA_focal(vessel_crop)
  # Normalizar
  vals_vessel <- values(vessel_crop); vals_vessel <- vals_vessel[!is.na(vals_vessel)]
  vessel_rast <- (vessel_crop - min(vals_vessel)) / (max(vals_vessel) - min(vals_vessel))
  # Resample al template
  vessel_rast <- project(vessel_rast, "EPSG:4326", method="bilinear")
  vessel_rast <- resample(vessel_rast, template, method="bilinear")
  
  # --- MONTHLY RISK INDEX MAPS ---
  risk_rast <- wave_rast*w['wave'] + curr_rast*w['curr'] + vessel_rast*w['traf']
  risk_rast <- mask(risk_rast, mallorca_vect)
  names(risk_rast) <- paste0("Risk_", mes)
  risk_rasters[[mes]] <- risk_rast
  
  # --- Plot mensual ---
  risk_df <- as.data.frame(risk_rast, xy=TRUE, na.rm=TRUE)
  names(risk_df)[3] <- "RiskIndex"
  print(
    ggplot(risk_df) +
      geom_raster(aes(x=x, y=y, fill=RiskIndex)) +
      geom_sf(data = mallorca_contour, fill = "grey80", color = "white", inherit.aes = FALSE) +
      scale_fill_gradientn(
        colors = c("green", "yellow", "red"),   # paleta deseada
        limits = c(0, 1),                       # mantiene leyenda de 0 a 1
        na.value = "transparent", name= NULL
      ) +
      coord_sf(xlim = c(2, 4), ylim = c(39, 40.5), expand = FALSE) +
      labs(title=paste("Risk Index -", mes)) +
      theme_minimal() +
      labs(
        y = NULL,
        x = NULL
      )
  )
}

# Combinar los 5 gráficos
library(patchwork)

monthly_plots <- list()

for (i in seq_along(meses)) {
  mes <- meses[i]
  
  risk_df <- as.data.frame(risk_rasters[[mes]], xy=TRUE, na.rm=TRUE)
  names(risk_df)[3] <- "RiskIndex"
  
  p <- ggplot(risk_df) +
    geom_raster(aes(x=x, y=y, fill=RiskIndex)) +
    geom_sf(data = mallorca_contour, fill = "grey80", color = "white", inherit.aes = FALSE) +
    scale_fill_gradientn(
      colors = c("green", "yellow", "red"),
      limits = c(0, 1),
      na.value = "transparent", name=NULL
    ) +
    coord_sf(xlim = c(2, 4), ylim = c(39, 40.5), expand = FALSE) +
    labs(title=paste("Risk Index -", mes)) +
    theme_minimal() +
    labs(y=NULL, x=NULL)
  
  monthly_plots[[i]] <- p
}
combined_plot <- wrap_plots(monthly_plots, ncol=3, guides="collect") &
  theme(legend.position = "right") &
  scale_x_continuous(breaks=seq(2,4,by=1)) &
  scale_y_continuous(breaks=seq(39,40.5,by=1))

print(combined_plot)


# --- MEAN RISK INDEX MAP FINAL ---
risk_mean <- mean(rast(risk_rasters))
names(risk_mean) <- "Risk_Mean"
risk_mean_df <- as.data.frame(risk_mean, xy=TRUE, na.rm=TRUE)

library(ggplot2)
library(viridis)
library(scales)

# Rescale los valores para que la mayor parte de la variación 0-0.6 se estire
risk_mean_df$Risk_Rescaled <- rescale(risk_mean_df$Risk_Mean, to = c(0,1), from = c(0,0.6))

ggplot(risk_mean_df) +
  geom_raster(aes(x = x, y = y, fill = Risk_Rescaled)) +
  geom_sf(data = mallorca_contour, fill = "grey80", color = "white", inherit.aes = FALSE) +
  scale_fill_gradientn(
    colors = c("green", "yellow", "red"),   # paleta deseada
    limits = c(0, 1),                       # mantiene leyenda de 0 a 1
    na.value = "transparent", name= NULL
  ) +
  coord_sf(xlim = c(2, 4), ylim = c(39, 40.5), expand = FALSE) +
  labs(title = "Mean Risk Index (Aug-Dec 2023)") +
  theme_minimal() +
  labs(
    y = NULL,
    x = NULL
  )

#-------------------------------
#INDICE CON HIGH, MODERATE, LOW
#-------------------------------

library(terra)

# 1️⃣ Crear plantilla a partir del raster de oleaje
template <- rast(
  VHM0_freq_filled[, c("x", "y", "freq_above_p90")],
  type = "xyz",
  crs = "EPSG:4326"
)
#Rasterizar wave

wave_r <- rasterize(
  vect(VHM0_freq_filled, geom = c("x", "y"), crs = "EPSG:4326"),
  template,
  field = "freq_above_p90",
  fun = mean
)
#Rasterizar current
currents_df <- bind_rows(meses_df)  # Combina los meses si están en lista

currents_df <- currents_df %>%
  mutate(
    risk_num = case_when(
      vel <= 0.10 ~ 0.1,
      vel > 0.10 & vel <= 0.20 ~ 0.5,
      vel > 0.20 ~ 1
    )
  )

curr_r <- rasterize(
  vect(currents_df, geom = c("x", "y"), crs = "EPSG:4326"),
  template,
  field = "risk_num",
  fun = mean
)
#Rasterizar vessel
vessel_df <- df_all %>%
  mutate(
    vessel_num = case_when(
      density_cat == "Low" ~ 0.1,
      density_cat == "Moderate" ~ 0.5,
      density_cat == "High" ~ 1
    )
  )

vessel_r <- rasterize(
  vect(vessel_df, geom = c("x", "y"), crs = "EPSG:4326"),
  template,
  field = "vessel_num",
  fun = mean
)
#Alinear resoluciones y extension
curr_r   <- resample(curr_r, wave_r, method = "near")
vessel_r <- resample(vessel_r, wave_r, method = "near")
#Pesos
w <- c(wave = 0.885, curr = 0.0002, traf = 0.115)
# Índice de riesgo ponderado
risk_index_r <- (wave_r * w["wave"]) +
  (curr_r * w["curr"]) +
  (vessel_r * w["traf"])
#(opcional) Normalizar 0-1
risk_index_r <- risk_index_r / global(risk_index_r, "max", na.rm = TRUE)
#Clasificación categorías
risk_cat_r <- classify(risk_index_r, 
                       rbind(
                         c(0.0, 0.33, 1),   # Low
                         c(0.33, 0.66, 2),  # Moderate
                         c(0.66, 1.0, 3)    # High
                       ))
df_plot <- as.data.frame(risk_cat_r, xy = TRUE, na.rm = TRUE)
names(df_plot)[3] <- "RiskIndex"

df_plot$RiskIndex <- factor(df_plot$RiskIndex, 
                            levels = 1:3,
                            labels = c("Low", "Moderate", "High"))

