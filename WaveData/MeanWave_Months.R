library(stars)
library(ggplot2)
library(viridis)
library(sf)
library(rnaturalearth)
library(patchwork)
library(terra)
library(dplyr)
library(tidyr)
library(ncdf4)
library(lubridate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOT MONTHLY MEAN Hm0 FOR MALLORCA (AUG-DEC) WITH FILLED NAs AND FADs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# 1. Load NetCDF
waveruta <- "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/cmems_mod_med_wav_anfc_4.2km_PT1H-i_1757335704289.nc"
wave_data <- read_stars(waveruta)
names(wave_data) <- "VHM0"
st_crs(wave_data) <- 4326  # WGS84

# 2. Select Mallorca
mallorca_bbox <- st_bbox(c(xmin=2, xmax=4, ymin=39, ymax=40.5), crs=st_crs(wave_data))
VHM0_mallorca <- st_crop(wave_data, mallorca_bbox)

# 3. Extract dates
fechas <- st_get_dimension_values(VHM0_mallorca, "time")

# 4. Create monthly indexes
index_aug <- which(fechas >= as.POSIXct("2023-08-15") & fechas < as.POSIXct("2023-09-01"))
index_sep <- which(format(fechas, "%Y-%m") == "2023-09")
index_oct <- which(format(fechas, "%Y-%m") == "2023-10")
index_nov <- which(format(fechas, "%Y-%m") == "2023-11")
index_dec <- which(format(fechas, "%Y-%m") == "2023-12")

# 5. Function to calculate monthly mean
calc_monthly_mean <- function(data, idx){
  st_apply(data[,,,idx], c("x","y"), mean, na.rm=TRUE)
}

mean_aug <- calc_monthly_mean(VHM0_mallorca, index_aug)
mean_sep <- calc_monthly_mean(VHM0_mallorca, index_sep)
mean_oct <- calc_monthly_mean(VHM0_mallorca, index_oct)
mean_nov <- calc_monthly_mean(VHM0_mallorca, index_nov)
mean_dec <- calc_monthly_mean(VHM0_mallorca, index_dec)

# 6. Fill NAs using focal mean
fill_month <- function(star_obj){
  waver <- rast(star_obj)
  r_filled <- focal(waver, w = matrix(1,3,3), fun = mean, na.rm = TRUE, NAonly = TRUE)
  st_as_stars(r_filled)
}

mean_aug_filled <- fill_month(mean_aug)
mean_sep_filled <- fill_month(mean_sep)
mean_oct_filled <- fill_month(mean_oct)
mean_nov_filled <- fill_month(mean_nov)
mean_dec_filled <- fill_month(mean_dec)

mean_list_filled <- list(
  mean_aug_filled,
  mean_sep_filled,
  mean_oct_filled,
  mean_nov_filled,
  mean_dec_filled
)


# 7. Load Mallorca coast and FADs
world <- ne_countries(scale="medium", returnclass="sf")
spain <- world[world$name=="Spain", ]
mallorca_sf <- st_crop(spain, mallorca_bbox)

# 8. Compute global min/max for scale-fixed plots
all_means <- c(
  as.vector(mean_aug_filled[[1]]),
  as.vector(mean_sep_filled[[1]]),
  as.vector(mean_oct_filled[[1]]),
  as.vector(mean_nov_filled[[1]]),
  as.vector(mean_dec_filled[[1]])
)
hm0_min <- min(all_means, na.rm = TRUE)
hm0_max <- max(all_means, na.rm = TRUE)

# 9. Plot function
library(patchwork)

plot_mean_FADs_void <- function(star_obj, month_name){
  waveFADdf <- as.data.frame(star_obj, long = TRUE)
  
  ggplot(waveFADdf, aes(x = x, y = y, fill = focal_mean)) +   
    geom_tile() +
    geom_sf(data = mallorca_sf, fill = "grey80", color = "white", inherit.aes = FALSE) +
    #geom_sf(data = fads, color = "red", size = 1, inherit.aes = FALSE) +
    scale_fill_viridis_c(option = "magma", na.value = "transparent",
                         limits = c(hm0_min, hm0_max), name = "Hm0 (m)") +
    coord_sf(xlim = c(mallorca_bbox["xmin"], mallorca_bbox["xmax"]),
             ylim = c(mallorca_bbox["ymin"], mallorca_bbox["ymax"]),
             expand = FALSE) +
    labs(title = month_name) +
    theme_void() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      plot.margin = margin(2,2,2,2)   # 🔹 pequeño margen alrededor de cada plot
    )
}

filledwaveFADplots_void <- mapply(plot_mean_FADs_void, mean_list_filled, months_eng, SIMPLIFY = FALSE)

combinedwaveFADfilled_void <- wrap_plots(filledwaveFADplots_void, ncol = 3, guides = "collect") & 
  theme(legend.position = "right")

combinedwaveFADfilled_void

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Hm NORMALIZING VALUES (to 0-1, not to a normal distribution)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Abrir NetCDF directamente como raster stack
VHM0_rast <- rast("C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/cmems_mod_med_wav_anfc_4.2km_PT1H-i_1757335704289.nc", 
                  subds="VHM0")

# 2. Comprobar nombres de variables y dimensiones
names(VHM0_rast)
dim (VHM0_rast)

# Convertir SpatRaster a data.frame, manteniendo todas las capas
VHM0_df <- as.data.frame(VHM0_rast, xy = TRUE)

#Número de capas
n_layers <- nlyr(VHM0_rast)

# Vector de fechas: ejemplo de hora en hora
time_vec <- seq(
  from = as.POSIXct("2023-08-15 00:00", tz = "UTC"),
  by = "1 hour",
  length.out = n_layers
)

# Convertir a data.frame largo
VHM0_long <- VHM0_df %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "time_index",
    values_to = "VHM0"
  ) %>%
  mutate(
    time_index = as.integer(gsub("VHM0_", "", time_index)),
    time = time_vec[time_index]
  )

# Monthly mean normalized

VHM0_monthly <- VHM0_long %>%
  mutate(month = month(time)) %>%              # extraer número de mes
  group_by(x, y, month) %>%
  summarise(VHM0_avg = mean(VHM0, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(VHM0_norm_avg = (VHM0_avg - min(VHM0_avg, na.rm = TRUE)) / 
           (max(VHM0_avg, na.rm = TRUE) - min(VHM0_avg, na.rm = TRUE)))


month_names <- c("January","February","March","April","May","June",
                 "July","August","September","October","November","December")

for(m in 8:12) {   # agosto a diciembre
  waveplot_df <- VHM0_monthly %>% filter(month == m)
  
  pwave <- ggplot(waveplot_df, aes(x = x, y = y, fill = VHM0_norm_avg)) +
    geom_raster() +
    geom_sf(data = mallorca_sf, fill = "grey80", color = "white", inherit.aes = FALSE) +
    scale_fill_viridis_c(name = "mean Hm (norm)", limits = c(0,1)) +  # escala fija
    coord_sf(xlim = c(mallorca_bbox["xmin"], mallorca_bbox["xmax"]),
             ylim = c(mallorca_bbox["ymin"], mallorca_bbox["ymax"])) +
    theme_minimal() +
    labs(title = paste("Normalized mean Hm0 -", month_names[m])) +
    theme(axis.title = element_blank())
  
  print(pwave)
}


~~~~~~~~~
  # combine the 5 plots
~~~~~~~~~

library(ggplot2)
library(patchwork)
library(dplyr)
library(lubridate)

# Lista de meses y nombres
months_num <- 8:12
months_name <- c("August","September","October","November","December")

# Crear lista de plots
waveplots <- lapply(seq_along(months_num), function(i) {
  m <- months_num[i]
  waveplot_df <- VHM0_monthly %>% filter(month == m)
  
  ggplot(waveplot_df, aes(x = x, y = y, fill = VHM0_norm_avg)) +
    geom_raster() +
    geom_sf(data = mallorca_sf, fill = "grey80", color = "white", inherit.aes = FALSE) +
    scale_fill_viridis_c(name = "mean Hm (norm)", limits = c(0,1), option = "plasma") +
    coord_sf(xlim = c(mallorca_bbox["xmin"], mallorca_bbox["xmax"]),
             ylim = c(mallorca_bbox["ymin"], mallorca_bbox["ymax"])) +
    theme_minimal() +
    labs(title = months_name[i]) +
    theme(axis.title = element_blank())
})


combinedwave <- wrap_plots(waveplots, ncol = 3)
combinedwave

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# SIGNIFICANT WAVE HEIGHT GRAPH: 15-08-2024/31-12-2024

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(dplyr)
library(ggplot2)
library(stars)
library(units)

# 1. Colapsar espacialmente → media en todo el dominio para cada hora
ts_wave <- st_apply(wave_data, "time", mean, na.rm = TRUE)

# 2. Pasar a data.frame
df_ts <- as.data.frame(ts_wave, long = TRUE) %>%
  rename(Hm0 = 1)   # renombra la columna de valores
df <- as.data.frame(wave_data, long = TRUE)
names(df)


df_ts <- df %>%
  group_by(time) %>%
  summarise(Hm0 = mean(`cmems_mod_med_wav_anfc_4.2km_PT1H.i_1754301485032.nc`, na.rm = TRUE)) %>%
  mutate(Hm0 = as.numeric(Hm0))  # <-- importante

# 3. Graficar la serie temporal
ggplot(df_ts, aes(x = time, y = Hm0)) +
  geom_line(color = "steelblue") +
  labs(x = "Fecha", y = "Hm0 (m)",
       title = "Serie temporal de altura significativa de ola (Hm0)") +
  theme_minimal()

#GRAPH MEAN WAVE HEIGHT 2023

library(stars)
library(dplyr)
library(ggplot2)

# 1. Compute spatial mean and sd for each month
compute_stats <- function(raster_month, month_name){
  vals <- as.vector(raster_month[[1]])  # flatten raster values
  vals <- vals[!is.na(vals)]            # remove NAs
  data.frame(
    month = month_name,
    mean = mean(vals),
    sd = sd(vals)
  )
}

stats_aug <- compute_stats(mean_aug, "Aug")
stats_sep <- compute_stats(mean_sep, "Sep")
stats_oct <- compute_stats(mean_oct, "Oct")
stats_nov <- compute_stats(mean_nov, "Nov")
stats_dec <- compute_stats(mean_dec, "Dec")

stats_df <- bind_rows(stats_aug, stats_sep, stats_oct, stats_nov, stats_dec)
stats_df <- stats_df %>%
  mutate(
    min_vel = mean - sd,
    max_vel = mean + sd
  )
# 2. Bar plot with error bars

ggplot(stats_df, aes(x=month, y=mean)) +
  geom_bar(stat="identity", fill = "grey60", width=0.5) +
  geom_errorbar(aes(ymin = min_vel, ymax = max_vel), width = 0.1, color = "black") +
  ylab("Mean Significant Wave Height (m)") +
  xlab(NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

#Frequecy plot
library(ggplot2)
library(dplyr)

# Example: using VHM0_long
# Remove NAs
wave_data_clean <- VHM0_long %>% filter(!is.na(VHM0))

# Plot histogram
ggplot(wave_data_clean, aes(x = VHM0)) +
  geom_histogram(binwidth = 0.1, fill = "grey80", color = "black") +
  labs(
    x = "Significant Wave Height (m)",
    y = "Frequency",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),  # remove vertical major grid
    panel.grid.minor.x = element_blank(),   # remove vertical minor grid
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# FRECUENCIA DE Hm0 ≥ P90 (AGO–DIC) — 5 CLASES + MEDIA AGOSTO–DICIEMBRE (TÍTULOS CENTRADOS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(dplyr)
library(terra)
library(ggplot2)
library(sf)
library(patchwork)

#--------------------------------------------#
# 1. Calcular frecuencia ≥ P90 por mes
#--------------------------------------------#
calc_monthly_freq <- function(data, idx, p90=2.195){
  subdata <- data[,,,idx]
  st_apply(subdata, c("x","y"), function(x) mean(x >= p90, na.rm=TRUE))
}

freq_aug <- calc_monthly_freq(VHM0_mallorca, index_aug)
freq_sep <- calc_monthly_freq(VHM0_mallorca, index_sep)
freq_oct <- calc_monthly_freq(VHM0_mallorca, index_oct)
freq_nov <- calc_monthly_freq(VHM0_mallorca, index_nov)
freq_dec <- calc_monthly_freq(VHM0_mallorca, index_dec)

#--------------------------------------------#
# 2. Calcular media agosto–diciembre
#--------------------------------------------#
freq_mean_augdec <- (freq_aug + freq_sep + freq_oct + freq_nov + freq_dec) / 5

#--------------------------------------------#
# 3. Rellenar NAs con media focal 3x3
#--------------------------------------------#
fill_month <- function(star_obj){
  waver <- rast(star_obj)
  r_filled <- focal(waver, w = matrix(1,3,3), fun = mean, na.rm = TRUE, NAonly = TRUE)
  st_as_stars(r_filled)
}

freq_aug_filled  <- fill_month(freq_aug)
freq_sep_filled  <- fill_month(freq_sep)
freq_oct_filled  <- fill_month(freq_oct)
freq_nov_filled  <- fill_month(freq_nov)
freq_dec_filled  <- fill_month(freq_dec)
freq_mean_filled <- fill_month(freq_mean_augdec)

#--------------------------------------------#
# 4. Función de ploteo con títulos centrados
#--------------------------------------------#
plot_freq_FADs <- function(star_obj, month_name){
  df <- as.data.frame(star_obj, long = TRUE)
  names(df)[3] <- "freq"
  df <- df[!is.na(df$freq), ]
  
  df <- df %>%
    mutate(freq_class5 = cut(
      freq,
      breaks = c(0, 0.02, 0.05, 0.10, 0.20, 1),
      labels = c("0–2%", "2–5%", "5–10%", "10–20%", ">20%"),
      include.lowest = TRUE
    ))
  
  ggplot(df, aes(x = x, y = y, fill = freq_class5)) +
    geom_tile() +
    geom_sf(data = mallorca_sf, fill = "grey80", color = "white", inherit.aes = FALSE) +
    scale_fill_manual(
      values = c(
        "0–2%"   = "#66AA33",
        "2–5%"   = "#A6D854",
        "5–10%"  = "#FFDD33",
        "10–20%" = "#F46D43",
        ">20%"   = "#D73027"
      ),
      name = "Freq. Hm0 ≥ 2.195 m"
    ) +
    coord_sf(
      xlim = c(mallorca_bbox["xmin"], mallorca_bbox["xmax"]),
      ylim = c(mallorca_bbox["ymin"], mallorca_bbox["ymax"])
    ) +
    labs(title = month_name) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold")  # 🔹 centrado y negrita
    )
}

#--------------------------------------------#
# 5. Combinar plots
#--------------------------------------------#
freq_list_filled <- list(
  freq_aug_filled, freq_sep_filled, freq_oct_filled,
  freq_nov_filled, freq_dec_filled, freq_mean_filled
)

months_eng <- c("August", "September", "October", "November", "December", "Mean Aug–Dec")

freqplots <- mapply(plot_freq_FADs, freq_list_filled, months_eng, SIMPLIFY = FALSE)

combined_freqplots <- wrap_plots(freqplots, ncol = 3, guides = "collect") &
  theme(legend.position = "right")

combined_freqplots


#Plot p90 continuo

library(terra)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# --- Obtener contorno ---
world <- ne_countries(scale = "medium", returnclass = "sf")

# --- Función de plot ---
plot_freq_p90 <- function(r, title, mallorca_sf, world){
  plot(r,
       col = viridis(20, option="magma"),
       main = title,
       axes = FALSE, box = FALSE,
       legend = TRUE)
  plot(st_geometry(world), add = TRUE, col = "grey90", border = "white")
  plot(st_geometry(mallorca_sf), add = TRUE, col = "grey80", border = "white", lwd = 1.2)
}

# --- Rellenar NAs con media focal ---
w <- matrix(1, 3, 3)
r_stack2_filled_list <- list()
for (i in 1:nlyr(r_stack2)) {
  r_stack2_filled_list[[i]] <- focal(r_stack2[[i]], w = w, fun = mean, na.rm = TRUE, NAonly = TRUE)
}
r_stack2_filled <- rast(r_stack2_filled_list)
names(r_stack2_filled) <- names(r_stack2)

# --- Plot: 2 filas x 3 columnas ---
months_name <- names(r_stack2_filled)
par(mfrow = c(2, 3), mar = c(1, 1, 2, 4))  # margen dcha más amplio para la leyenda

for (i in 1:5) {
  plot_freq_p90(r_stack2_filled[[i]], months_name[i], mallorca_sf, world)
}

# --- Mapa del promedio ---
r_mean <- app(r_stack2_filled, mean, na.rm = TRUE)
plot_freq_p90(r_mean, "Mean Aug–Dec", mallorca_sf, world)
