#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Paquetes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(ncdf4)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Parámetros
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
lon_range_uli25 <- c(2, 4)
lat_range_uli25 <- c(39, 40.5)

dates_interval_uli25 <- seq.Date(
  as.Date("2025-07-24"),
  as.Date("2025-08-02"),
  by = "day"
)

world <- ne_countries(scale = "medium", returnclass = "sf")
spain <- world[world$name == "Spain", ]
mallorca_bbox_uli25 <- st_bbox(
  c(
    xmin = lon_range_uli25[1], xmax = lon_range_uli25[2],
    ymin = lat_range_uli25[1], ymax = lat_range_uli25[2]
  ),
  crs = 4326
)
mallorca_sf_uli25 <- st_crop(spain, mallorca_bbox_uli25)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Función: calcular media de corrientes en m/s (espacial)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
procesar_intervalo_uli25 <- function(dates_interval, lon_range, lat_range) {
  vel_sum_uli25 <- NULL
  total_steps_uli25 <- 0
  coords_lon_uli25 <- coords_lat_uli25 <- NULL
  
  for (i in seq_along(dates_interval)) {
    d <- dates_interval[i]
    message("Procesando: ", as.Date(d, origin = "1970-01-01"))
    
    url_uli25 <- paste0(
      "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
      format(d, "%Y/%m/"),
      "roms_wmop_surface_", format(d, "%Y%m%d"), ".nc"
    )
    
    nc_uli25 <- nc_open(url_uli25)
    
    lon_uli25 <- ncvar_get(nc_uli25, "lon_uv")
    lat_uli25 <- ncvar_get(nc_uli25, "lat_uv")
    u_uli25 <- ncvar_get(nc_uli25, "u")
    v_uli25 <- ncvar_get(nc_uli25, "v")
    
    nc_close(nc_uli25)
    
    # Índices Mallorca
    idx_lon_uli25 <- which(lon_uli25 >= lon_range[1] & lon_uli25 <= lon_range[2])
    idx_lat_uli25 <- which(lat_uli25 >= lat_range[1] & lat_uli25 <= lat_range[2])
    
    u_crop_uli25 <- u_uli25[idx_lon_uli25, idx_lat_uli25, , drop = FALSE]
    v_crop_uli25 <- v_uli25[idx_lon_uli25, idx_lat_uli25, , drop = FALSE]
    
    # Módulo de velocidad
    vel_uli25 <- sqrt(u_crop_uli25^2 + v_crop_uli25^2)
    
    # Sumar sobre timesteps
    vel_day_sum_uli25 <- apply(vel_uli25, c(1, 2), sum)
    
    if (is.null(vel_sum_uli25)) {
      vel_sum_uli25 <- vel_day_sum_uli25
      coords_lon_uli25 <- lon_uli25[idx_lon_uli25]
      coords_lat_uli25 <- lat_uli25[idx_lat_uli25]
    } else {
      vel_sum_uli25 <- vel_sum_uli25 + vel_day_sum_uli25
    }
    
    total_steps_uli25 <- total_steps_uli25 + dim(vel_uli25)[3]
    
    rm(u_uli25, v_uli25, u_crop_uli25, v_crop_uli25, vel_uli25, vel_day_sum_uli25)
    gc()
  }
  
  # Velocidad media por celda
  vel_mean_uli25 <- vel_sum_uli25 / total_steps_uli25
  
  currentsdf_uli25 <- expand.grid(
    x = as.numeric(coords_lon_uli25),
    y = as.numeric(coords_lat_uli25)
  )
  currentsdf_uli25$velocity <- as.numeric(vel_mean_uli25)
  
  return(currentsdf_uli25)
}

#--------------------------------------------------------------
# Ejecutar función (media espacial)
#--------------------------------------------------------------
currentsdf_uli25 <- procesar_intervalo_uli25(
  dates_interval_uli25,
  lon_range_uli25,
  lat_range_uli25
)

# Media espacial por timestep (serie temporal)

df_list_uli25 <- list()

for (i in seq_along(dates_interval_uli25)) {
  day_uli25 <- dates_interval_uli25[i]
  
  url_uli25 <- paste0(
    "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
    format(day_uli25, "%Y/%m/"),
    "roms_wmop_surface_", format(day_uli25, "%Y%m%d"), ".nc"
  )
  
  nc_uli25 <- nc_open(url_uli25)
  lon_uli25 <- ncvar_get(nc_uli25, "lon_uv")
  lat_uli25 <- ncvar_get(nc_uli25, "lat_uv")
  u_uli25 <- ncvar_get(nc_uli25, "u")
  v_uli25 <- ncvar_get(nc_uli25, "v")
  time_uli25 <- ncvar_get(nc_uli25, "ocean_time")
  time_origin_uli25 <- as.POSIXct("1968-05-23 00:00:00", tz = "UTC")
  time_posix_uli25 <- time_origin_uli25 + time_uli25
  nc_close(nc_uli25)
  
  idx_lon_uli25 <- which(lon_uli25 >= lon_range_uli25[1] & lon_uli25 <= lon_range_uli25[2])
  idx_lat_uli25 <- which(lat_uli25 >= lat_range_uli25[1] & lat_uli25 <= lat_range_uli25[2])
  u_crop_uli25 <- u_uli25[idx_lon_uli25, idx_lat_uli25, , drop = FALSE]
  v_crop_uli25 <- v_uli25[idx_lon_uli25, idx_lat_uli25, , drop = FALSE]
  
  mean_vel_uli25 <- apply(
    sqrt(u_crop_uli25^2 + v_crop_uli25^2),
    3, mean, na.rm = TRUE
  )
  
  df_day_uli25 <- data.frame(
    time = time_posix_uli25,
    velocity = mean_vel_uli25
  )
  
  df_list_uli25[[i]] <- df_day_uli25
}

df_time_uli25 <- do.call(rbind, df_list_uli25)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Gráfico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Sys.setlocale("LC_TIME", "C")  # meses en inglés

ggplot(df_time_uli25, aes(x = time, y = velocity)) +
  geom_line(color = "#1f78b4", size = 1) +
  annotate("rect",
           xmin = as.POSIXct("2025-07-29 04:30:00", tz = "UTC"),
           xmax = as.POSIXct("2025-07-30 04:26:00", tz = "UTC"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  scale_x_datetime(
    limits = c(
      as.POSIXct("2025-07-24 00:00:00", tz = "UTC"),
      as.POSIXct("2025-07-30 00:00:00", tz = "UTC")
    ),
    date_breaks = "1 day",
    date_labels = "%d-%b"
  ) +
  labs(x = NULL,
       y = "Mean Surface Current Velocity (m/s)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank()
  )

#plot desde que se puso hasta que se perdió

ggplot(df_time_uli25, aes(x = time, y = velocity)) +
  geom_line(color = "#1f78b4", size = 1) +
  annotate("rect",
           xmin = as.POSIXct("2025-07-29 04:30:00", tz = "UTC"),
           xmax = as.POSIXct("2025-07-30 04:26:00", tz = "UTC"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  # Línea vertical verde de colocación
  geom_vline(xintercept = as.POSIXct("2025-07-24 00:00:00", tz = "UTC"),
             color = "green", linetype = "dashed", size = 1) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%d-%b"
  ) +
  coord_cartesian(
    xlim = c(as.POSIXct("2025-07-24 00:00:00", tz = "UTC"),
             as.POSIXct("2025-07-29 23:59:59", tz = "UTC"))
  ) +
  labs(x = NULL, y = "Mean Surface Current Velocity (m/s)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank()
  )
