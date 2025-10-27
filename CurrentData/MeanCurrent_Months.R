#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# EXTRACT ALL SOCIB DATA OPENDAP 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(ncdf4)
library(stars)
library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)


#Define bounding box for Mallorca

mallorca_bbox <- st_bbox(c(xmin=2, xmax=4, ymin=39, ymax=40.5), crs=st_crs(4326))

#Define dates

dates <- seq.Date(as.Date("2023-08-15"), as.Date("2023-12-12"), by = "day")

# Function to read and crop a single day

read_day_currents <- function(date, bbox) {
    url <- paste0(
      "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
      format(date, "%Y/%m/"),
      "roms_wmop_surface_", format(date, "%Y%m%d"), ".nc"
    )
    
    nc <- nc_open(url)
    
    lon <- ncvar_get(nc, "lon_uv")
    lat <- ncvar_get(nc, "lat_uv")
    time <- ncvar_get(nc, "ocean_time")
    
    u <- ncvar_get(nc, "u")
    v <- ncvar_get(nc, "v")
    
    nc_close(nc)
    
    # create stars object
    day_cube <- st_as_stars(list(u=u, v=v),
                            dimensions = st_dimensions(
                              x = seq_len(length(lon)),
                              y = seq_len(length(lat)),
                              time = as.POSIXct(time, origin = "1968-05-23", tz="UTC")
                            ))
    
    # assign real coordinates
    dims <- st_dimensions(day_cube)
    dims$x$values <- lon
    dims$y$values <- lat
    st_dimensions(day_cube) <- dims
    
    # assign WGS84 CRS
    st_crs(day_cube) <- 4326
    
    # crop to Mallorca
    day_cube <- st_crop(day_cube, bbox)
    
    return(day_cube)
  }

#Loop over all days with progress bar

all_days <- vector("list", length(dates))
start_time <- Sys.time()
bar_len <- 50  # longitud de la barra
green <- "\033[32m"
reset <- "\033[0m"

for (i in seq_along(dates)) {
  d <- dates[i]

  all_days[[i]] <- read_day_currents(d, mallorca_bbox)
  
  # Barra de progreso
  pct <- i / length(dates)
  n_fill <- round(bar_len * pct)
  bar <- paste0(rep("=", n_fill), rep("-", bar_len - n_fill), collapse = "")
  
  # ETA en minutos y segundos
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  remaining <- elapsed / i * (length(dates) - i)
  rem_min <- floor(remaining / 60)
  rem_sec <- round(remaining %% 60)
  
  # Mostrar todo en la misma línea con color verde para la barra
  cat(sprintf("\r[%s%s%s] %3.0f%% | ETA: %02d:%02d | Día: %s",
              green, bar, reset, pct*100, rem_min, rem_sec, d))
  flush.console()
}

cat("\n")

# CARGAR DIRECTAMENTE PARA NO CORRER EL CÓDIGO ANTERIOR:
all_days <- readRDS("currents_mallorca_2023.rds")

# stack along the time dimension
currents_cube <- do.call(c, c(all_days, along="time"))


# (SKIP) MEAN INSTANT VELOCITIES PER MONTH: AUGUST


# Bounding box Mallorca
lon_range <- c(2, 4)
lat_range <- c(39, 40.5)

# Fechas de agosto- dic
dates_aug <- seq.Date(as.Date("2023-08-15"), as.Date("2023-08-31"), by="day")
dates_sept <- seq.Date(as.Date("2023-09-01"), as.Date("2023-09-30"), by="day")
dates_oct <- seq.Date(as.Date("2023-10-01"), as.Date("2023-10-31"), by="day")
dates_nov <- seq.Date(as.Date("2023-11-01"), as.Date("2023-11-30"), by="day")
dates_dec <- seq.Date(as.Date("2023-12-01"), as.Date("2023-12-31"), by="day")

vel_sum <- NULL
total_steps <- 0
coords_lon <- coords_lat <- NULL


for (i in seq_along(dates_aug)) {
  d <- dates_aug[i]
  message("Procesando: ", as.Date(d, origin="1970-01-01"))
  
  url <- paste0(
    "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
    format(d, "%Y/%m/"),
    "roms_wmop_surface_", format(d, "%Y%m%d"), ".nc"
  )
  
  # Abrir NetCDF remoto
  nc <- nc_open(url)
  
  lon <- ncvar_get(nc, "lon_uv")
  lat <- ncvar_get(nc, "lat_uv")
  u <- ncvar_get(nc, "u")
  v <- ncvar_get(nc, "v")
  
  nc_close(nc)
  
  # Índices Mallorca
  idx_lon <- which(lon >= lon_range[1] & lon <= lon_range[2])
  idx_lat <- which(lat >= lat_range[1] & lat <= lat_range[2])
  
  u_crop <- u[idx_lon, idx_lat, , drop=FALSE]
  v_crop <- v[idx_lon, idx_lat, , drop=FALSE]
  
  # Módulo de velocidad
  vel <- sqrt(u_crop^2 + v_crop^2)
  
  # Sumar sobre timesteps
  vel_day_sum <- apply(vel, c(1,2), sum)
  
  if (is.null(vel_sum)) {
    vel_sum <- vel_day_sum
    coords_lon <- lon[idx_lon]
    coords_lat <- lat[idx_lat]
  } else {
    vel_sum <- vel_sum + vel_day_sum
  }
  
  total_steps <- total_steps + dim(vel)[3]
  
  # Limpiar memoria
  rm(u, v, u_crop, v_crop, vel, vel_day_sum)
  gc()
}

#GUARDAR TRAS CORRER CÓDIGO
save(vel_sum, coords_lon, coords_lat, total_steps, file = "mallorca_vel_sum.RData")

#CARGAR
load("mallorca_vel_sum.RData")

# Velocidad media mensual
vel_aug_mean <- vel_sum / total_steps


# Crear dimensiones de forma correcta
dims <- st_dimensions(
  x = seq_along(coords_lon),
  y = seq_along(coords_lat)
)

# Asignar coordenadas reales
dims$x$values <- coords_lon
dims$y$values <- coords_lat

# Convertir a stars 2D
vel_aug_stars <- st_as_stars(vel_aug_mean, dimensions = dims) %>%
  st_set_crs(4326)

# Visualización
library(ggplot2)
library(sf)
library(viridis)

# df: data.frame con columnas x (lon), y (lat) y vel (velocidad)

library(ggplot2)

ggplot(df, aes(x = x, y = y, fill = vel)) +
  geom_raster() +
  geom_sf(data = mallorca_sf, fill = "grey80", color = "white", inherit.aes = FALSE) +
  coord_sf(xlim = c(2, 4), ylim = c(39, 40.5), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Currents August 2023", fill = "Velocity (m/s)") +
  theme_void() +               # elimina fondo y grid
  theme_minimal()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MEAN INSTANT VELOCITIES AUG-DEC (WITH WHITE SQUARES)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(ncdf4)
library(stars)
library(sf)
library(ggplot2)
library(viridis)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)

lon_range <- c(2, 4)
lat_range <- c(39, 40.5)

meses <- list(
  August    = seq.Date(as.Date("2023-08-15"), as.Date("2023-08-31"), by="day"),
  September = seq.Date(as.Date("2023-09-01"), as.Date("2023-09-30"), by="day"),
  October   = seq.Date(as.Date("2023-10-01"), as.Date("2023-10-31"), by="day"),
  November  = seq.Date(as.Date("2023-11-01"), as.Date("2023-11-30"), by="day"),
  December  = seq.Date(as.Date("2023-12-01"), as.Date("2023-12-31"), by="day")
)

# Natural Earth countries
world <- ne_countries(scale = "medium", returnclass = "sf")

# FUNCTION TO PROCESS MONTH

procesar_mes <- function(dates, lon_range, lat_range) {
  vel_sum <- NULL
  total_steps <- 0
  coords_lon <- coords_lat <- NULL
  
  for (d in dates) {
    d <- as.Date(d, origin="1970-01-01")  # force it to be date
    message("Processing day: ", format(d, "%Y-%m-%d"))
    
    url <- paste0(
      "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
      format(d, "%Y/%m/"),
      "roms_wmop_surface_", format(d, "%Y%m%d"), ".nc"
    )
    
    nc <- nc_open(url)
    lon <- ncvar_get(nc, "lon_uv")
    lat <- ncvar_get(nc, "lat_uv")
    u <- ncvar_get(nc, "u")
    v <- ncvar_get(nc, "v")
    nc_close(nc)
    
    # Indexes inside bbox
    idx_lon <- which(lon >= lon_range[1] & lon <= lon_range[2])
    idx_lat <- which(lat >= lat_range[1] & lat <= lat_range[2])
    
    u_crop <- u[idx_lon, idx_lat, , drop=FALSE]
    v_crop <- v[idx_lon, idx_lat, , drop=FALSE]
    
    # Calculate velocity module
    vel <- sqrt(u_crop^2 + v_crop^2)
    
    # Sum over timesteps
    vel_day_sum <- apply(vel, c(1,2), sum)
    
    if (is.null(vel_sum)) {
      vel_sum <- vel_day_sum
      coords_lon <- lon[idx_lon]
      coords_lat <- lat[idx_lat]
    } else {
      vel_sum <- vel_sum + vel_day_sum
    }
    
    total_steps <- total_steps + dim(vel)[3]
    
    # Clean memory
    rm(u, v, u_crop, v_crop, vel, vel_day_sum)
    gc()
  }
  
  # Calculate mean month velocity
  vel_mean <- vel_sum / total_steps
  
  # Convert vel_mean to stars object
  
  vel_stars <- st_as_stars(vel_mean, dimensions = dims) %>% st_set_crs(4326)
  
  # Fill coastal NAs
  r <- rast(vel_stars)
  r_filled <- focal(r, w = matrix(1,3,3), fun = mean, na.rm = TRUE, NAonly = TRUE)
  vel_stars_filled <- st_as_stars(r_filled)
  
  # Convert to dataframe for ggplot
  df <- as.data.frame(vel_stars_filled, long=TRUE)
  names(df)[3] <- "vel"
  
  return(df)
}

# Function to GENERATE MAPS without white squares (No global scale)

# Lista para guardar data.frames de cada mes
meses_df <- list()
meses_plots <- list()

for (mes in names(meses)) {
  # Procesar mes y guardar data.frame
  df_mes <- procesar_mes(meses[[mes]], lon_range, lat_range)
  meses_df[[mes]] <- df_mes  
  
  # Crear plot y guardar
  p <- ggplot(df_mes, aes(x = x, y = y, fill = vel)) +
    geom_raster() +
    geom_sf(data = world, fill = "grey80", color = "white", inherit.aes = FALSE) +
    coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = paste("Currents", mes, "2023"), fill = "Velocity (m/s)") +
    theme_minimal()
  
  meses_plots[[mes]] <- p
}

#GUARDAR
save(meses_df, meses_plots, file = "meses_currents.RData")

#CARGAR
load("meses_currents.RData")

# Previsualizar todos los mapas
for (mes in names(meses_plots)) {
  print(meses_plots[[mes]])
}

#VER LOS 5 MESES + PROMEDIO

library(ggplot2)
library(patchwork)
library(dplyr)
library(stars)
library(terra)
library(sf)
library(viridis)
library(rnaturalearth)

# Bounding box Mallorca
lon_range <- c(2, 4)
lat_range <- c(39, 40.5)

# Natural Earth countries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Añadir promedio de los 5 meses
df_mean <- do.call(rbind, meses_df) %>%
  group_by(x, y) %>%
  summarise(vel = mean(vel, na.rm = TRUE)) %>%
  ungroup()
meses_df[["Mean Aug–Dec"]] <- df_mean

# Crear plots sin ejes y con título centrado en negrita
meses_plots <- lapply(names(meses_df), function(mes) {
  df_mes <- meses_df[[mes]]
  
  ggplot(df_mes, aes(x = x, y = y, fill = vel)) +
    geom_raster() +
    geom_sf(data = world, fill = "grey80", color = "white", inherit.aes = FALSE) +
    coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = mes, fill = NULL) +
    theme_void() +
    theme(
      plot.title = element_text(size= 10, face = "bold", hjust = 0.5),
      legend.position = "right"
    )
})

# Combinar todos los mapas
wrap_plots(meses_plots, ncol = 3)


# MAPAS CLASIFICACION 5 CLASES (VERY LOW → VERY HIGH) +PROMEDIO
#-----------------------------------------------------

# 1️⃣ Calcular la media espacial por celda para todos los meses
# Convertimos a un solo data.frame
all_months_df <- bind_rows(meses_df, .id = "month_name")

# Media por celda
mean_df <- all_months_df %>%
  group_by(x, y) %>%
  summarise(vel = mean(vel, na.rm = TRUE), .groups = "drop")

# Añadir columna para clasificar igual que los demás
mean_df <- mean_df %>%
  mutate(risk_class = case_when(
    vel <= 0.1 ~ "Very Low",
    vel > 0.1 & vel <= 0.15 ~ "Low",
    vel > 0.15 & vel <= 0.20 ~ "Moderate",
    vel > 0.20 & vel <= 0.25 ~ "High",
    vel > 0.25 ~ "Very High"
  )) %>%
  mutate(risk_class = factor(
    risk_class,
    levels = c("Very High", "High", "Moderate", "Low", "Very Low")
  ))

# 2️⃣ Añadir al listado de meses
meses_df[["Mean Aug–Dec"]] <- mean_df

# 3️⃣ Generar plots (incluyendo el promedio)
plots_risk_void <- lapply(names(meses_df), function(mes) {
  df_mes <- meses_df[[mes]]
  
  ggplot(df_mes, aes(x = x, y = y, fill = risk_class)) +
    geom_raster() +
    geom_sf(data = world, fill = "grey80", color = "white", inherit.aes = FALSE) +
    coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    scale_fill_manual(
      values = risk_colors,
      labels = risk_labels,       
      na.value = NA,
      drop = TRUE,
      name = "Current speed (m/s)"
    ) +
    labs(title = mes) +
    theme_void() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),  # título centrado y en negrita
      legend.position = "right"
    )
})

# 4️⃣ Combinar todos los mapas y mantener la leyenda
wrap_plots(plots_risk_void, ncol = 3, guides = "collect") & 
  theme(legend.position = "right")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MEAN INSTANT VELOCITIES AUG-DEC WITHOUT WHITE SQUARES GLOBAL SCALE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# 1. Process all months and save in a list
vel_dfs <- lapply(names(meses), function(mes) procesar_mes(meses[[mes]], lon_range, lat_range))
names(vel_dfs) <- names(meses)

# 2. Calculate global range of velocities
vel_min <- min(sapply(vel_dfs, function(df) min(df$vel, na.rm = TRUE)))
vel_max <- max(sapply(vel_dfs, function(df) max(df$vel, na.rm = TRUE)))

# 3. Generate maps with fixed scale
library(patchwork)

# Ajustar todos los plots
plots_void <- lapply(names(vel_dfs), function(mes) {
  df_mes <- vel_dfs[[mes]]
  
  ggplot(df_mes, aes(x = x, y = y, fill = vel)) +
    geom_raster() +
    geom_sf(data = world, fill = "grey80", color = "white", inherit.aes = FALSE) +
    coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    scale_fill_viridis_c(option = "magma", limits = c(vel_min, vel_max), name = "Velocity (m/s)") +
    labs(title = mes) +   # 🔹 Solo el nombre del mes
    theme_void() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5)
    )
})

# Combinar todos y mantener una sola leyenda
wrap_plots(plots_void, ncol = 3, guides = "collect") & theme(legend.position = "right")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MEAN INSTANT VELOCITIES NORMALIZED
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. Process all months and save in a list
procesar_mes <- function(dates, lon_range, lat_range) {
  library(ncdf4)
  library(stars)
  library(sf)
  
  vel_sum <- NULL
  total_steps <- 0
  coords_lon <- coords_lat <- NULL
  
  for (d in dates) {
    # Abrir NetCDF remoto
    url <- paste0(
      "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
      format(d, "%Y/%m/"),
      "roms_wmop_surface_", format(d, "%Y%m%d"), ".nc"
    )
    
    nc <- nc_open(url)
    lon <- ncvar_get(nc, "lon_uv")
    lat <- ncvar_get(nc, "lat_uv")
    u <- ncvar_get(nc, "u")
    v <- ncvar_get(nc, "v")
    nc_close(nc)
    
    # Índices dentro de Mallorca
    idx_lon <- which(lon >= lon_range[1] & lon <= lon_range[2])
    idx_lat <- which(lat >= lat_range[1] & lat <= lat_range[2])
    
    u_crop <- u[idx_lon, idx_lat, , drop=FALSE]
    v_crop <- v[idx_lon, idx_lat, , drop=FALSE]
    
    # Módulo de velocidad
    vel <- sqrt(u_crop^2 + v_crop^2)
    
    # Sumar sobre timesteps
    vel_day_sum <- apply(vel, c(1,2), sum)
    
    if (is.null(vel_sum)) {
      vel_sum <- vel_day_sum
      coords_lon <- lon[idx_lon]
      coords_lat <- lat[idx_lat]
    } else {
      vel_sum <- vel_sum + vel_day_sum
    }
    
    total_steps <- total_steps + dim(vel)[3]
    
    # Limpiar memoria
    rm(u, v, u_crop, v_crop, vel, vel_day_sum)
    gc()
  }
  
  # Crear dimensiones stars dentro de la función
  dims <- st_dimensions(
    x = seq_along(coords_lon),
    y = seq_along(coords_lat)
  )
  dims$x$values <- coords_lon
  dims$y$values <- coords_lat
  
  # Convertir a stars y luego a dataframe para ggplot
  vel_stars <- st_as_stars(vel_sum / total_steps, dimensions = dims) %>% st_set_crs(4326)
  df <- as.data.frame(vel_stars, long = TRUE)
  names(df)[3] <- "vel"
  
  return(df)
}


vel_dfs <- lapply(names(meses), function(mes) procesar_mes(meses[[mes]], lon_range, lat_range))
names(vel_dfs) <- names(meses)


# Normalize velocities using min-max normalization
normalize_data <- function(data) {
  return((data - min(data, na.rm = TRUE)) / (max(data, na.rm = TRUE) - min(data, na.rm = TRUE)))
}

# Normalize the mean velocity for each month (not sure if this is necessary)
vel_dfs_normalized <- lapply(vel_dfs, function(df) {
  df$vel_normalized <- normalize_data(df$vel)
  return(df)
})

# 1. Calculate global min and max across all months
vel_min_global <- min(sapply(vel_dfs, function(df) min(df$vel, na.rm = TRUE)))
vel_max_global <- max(sapply(vel_dfs, function(df) max(df$vel, na.rm = TRUE)))

# 2. Normalize using global min and max
vel_dfs_normalized_global <- lapply(vel_dfs, function(df) {
  df$vel_normalized <- (df$vel - vel_min_global) / (vel_max_global - vel_min_global)
  df
})

# Plot

library(gridExtra)


plots_normalized_global <- list()
for (mes in names(vel_dfs_normalized_global)) {
  df_mes <- vel_dfs_normalized_global[[mes]]
  
  p <- ggplot(df_mes, aes(x = x, y = y, fill = vel_normalized)) +
    geom_tile() +
    geom_sf(data = world, fill = "grey80", color = "white", inherit.aes = FALSE) +
    coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    scale_fill_viridis_c(option = "plasma", limits = c(0,1), na.value = "transparent") + #fills NA values
    scale_y_continuous(breaks = seq(39, 40.5, 0.5)) +
    scale_x_continuous(breaks = seq(2, 4, 0.5))
    labs(title = paste("Normalized Currents", mes, "2023"),
         fill = "Normalized velocity") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_text(size = 6),
      axis.text.y = element_text(angle = 90, vjust = 0.5)
    )
  
  plots_normalized_global[[mes]] <- p
}

# Visualizar todos juntos
grid.arrange(grobs = plots_normalized_global, ncol = 3)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# MEAN SURFACE CURRENT VELOCITY PER MONTH (SOCIB WMOP - MALLORCA AREA)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(ncdf4)
library(dplyr)
library(ggplot2)
library(lubridate)

#----------------------------------
# Settings
#----------------------------------
lon_range <- c(2, 4)
lat_range <- c(39, 40.5)

dates <- seq.Date(as.Date("2023-08-15"), as.Date("2023-12-31"), by = "day")

#----------------------------------
# Function to compute daily mean velocity
#----------------------------------
read_day_velocity <- function(date, lon_range, lat_range) {
  url <- paste0(
    "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
    format(date, "%Y/%m/"),
    "roms_wmop_surface_", format(date, "%Y%m%d"), ".nc"
  )
  
  nc <- try(nc_open(url), silent = TRUE)
  if (inherits(nc, "try-error")) return(NA_real_)
  
  lon <- ncvar_get(nc, "lon_uv")
  lat <- ncvar_get(nc, "lat_uv")
  u <- ncvar_get(nc, "u")
  v <- ncvar_get(nc, "v")
  nc_close(nc)
  
  # Subset to bounding box
  idx_lon <- which(lon >= lon_range[1] & lon <= lon_range[2])
  idx_lat <- which(lat >= lat_range[1] & lat <= lat_range[2])
  u_crop <- u[idx_lon, idx_lat, , drop = FALSE]
  v_crop <- v[idx_lon, idx_lat, , drop = FALSE]
  
  # Compute speed magnitude
  vel <- sqrt(u_crop^2 + v_crop^2)
  
  # Return mean velocity for the day
  mean(vel, na.rm = TRUE)
}

#----------------------------------
# Loop over all days
#----------------------------------
daily_vel <- sapply(dates, function(d) {
  message("Processing ", d)
  read_day_velocity(d, lon_range, lat_range)
})

#----------------------------------
# Combine results
#----------------------------------
df_vel <- data.frame(
  date = dates,
  velocity = daily_vel
) %>%
  filter(!is.na(velocity)) %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE),
         year = year(date))

#----------------------------------
# Summarize per month
#----------------------------------
df_monthly <- df_vel %>%
  group_by(year, month) %>%
  summarise(
    mean_vel = mean(velocity, na.rm = TRUE),
    min_vel  = min(velocity, na.rm = TRUE),
    max_vel  = max(velocity, na.rm = TRUE),
    .groups = "drop"
  )
df_monthly$month <- as.character(df_monthly$month)
df_monthly$month_en <- df_monthly$month
df_monthly$month_en[df_monthly$month_en == "ago"] <- "Aug"
df_monthly$month_en[df_monthly$month_en == "sep"] <- "Sep"
df_monthly$month_en[df_monthly$month_en == "oct"] <- "Oct"
df_monthly$month_en[df_monthly$month_en == "nov"] <- "Nov"
df_monthly$month_en[df_monthly$month_en == "dic"] <- "Dec"
df_monthly$month_en <- factor(df_monthly$month_en,
                              levels = c("Aug","Sep","Oct","Nov","Dec"))


#----------------------------------
# Plot: mean ± range per month
#----------------------------------
ggplot(df_monthly, aes(x = month_en, y = mean_vel)) +
  geom_bar(stat="identity", fill = "grey60", width=0.5) +
  geom_errorbar(aes(ymin = min_vel, ymax = max_vel), width = 0.1, color = "black") +
  labs(
    x = NULL,
    y = "Current Velocity (m/s)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),  # remove vertical major grid
    panel.grid.minor.x = element_blank(),   # remove vertical minor grid
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()
  )
#Distribution plot

ggplot(df_vel, aes(x = velocity)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black") +
  labs(x = "Current Velocity (m/s)", y = "Frequency") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),  # remove vertical major grid
    panel.grid.minor.x = element_blank(),   # remove vertical minor grid
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()
  )

#CARGAR OBJETOS PARA NO REPROCESAR:

vel_dfs <- readRDS("vel_dfs.rds")







