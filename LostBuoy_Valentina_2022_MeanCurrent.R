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
lon_range_val22 <- c(2, 4)
lat_range_val22 <- c(39, 40.5)

dates_interval_val22 <- seq.Date(
  as.Date("2022-08-23"),
  as.Date("2022-12-03"),
  by = "day"
)

world <- ne_countries(scale = "medium", returnclass = "sf")
spain <- world[world$name == "Spain", ]
mallorca_bbox_val22 <- st_bbox(
  c(
    xmin = lon_range_val22[1], xmax = lon_range_val22[2],
    ymin = lat_range_val22[1], ymax = lat_range_val22[2]
  ),
  crs = 4326
)
mallorca_sf_val22 <- st_crop(spain, mallorca_bbox_val22)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Función: calcular media de corrientes en m/s (espacial)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
procesar_intervalo_val22 <- function(dates_interval, lon_range, lat_range) {
  vel_sum_val22 <- NULL
  total_steps_val22 <- 0
  coords_lon_val22 <- coords_lat_val22 <- NULL
  
  for (i in seq_along(dates_interval)) {
    d <- dates_interval[i]
    message("Procesando: ", as.Date(d, origin = "1970-01-01"))
    
    url_val22 <- paste0(
      "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
      format(d, "%Y/%m/"),
      "roms_wmop_surface_", format(d, "%Y%m%d"), ".nc"
    )
    
    nc_val22 <- nc_open(url_val22)
    
    lon_val22 <- ncvar_get(nc_val22, "lon_uv")
    lat_val22 <- ncvar_get(nc_val22, "lat_uv")
    u_val22 <- ncvar_get(nc_val22, "u")
    v_val22 <- ncvar_get(nc_val22, "v")
    
    nc_close(nc_val22)
    
    # Índices Mallorca
    idx_lon_val22 <- which(lon_val22 >= lon_range[1] & lon_val22 <= lon_range[2])
    idx_lat_val22 <- which(lat_val22 >= lat_range[1] & lat_val22 <= lat_range[2])
    
    u_crop_val22 <- u_val22[idx_lon_val22, idx_lat_val22, , drop = FALSE]
    v_crop_val22 <- v_val22[idx_lon_val22, idx_lat_val22, , drop = FALSE]
    
    # Módulo de velocidad
    vel_val22 <- sqrt(u_crop_val22^2 + v_crop_val22^2)
    
    # Sumar sobre timesteps
    vel_day_sum_val22 <- apply(vel_val22, c(1, 2), sum)
    
    if (is.null(vel_sum_val22)) {
      vel_sum_val22 <- vel_day_sum_val22
      coords_lon_val22 <- lon_val22[idx_lon_val22]
      coords_lat_val22 <- lat_val22[idx_lat_val22]
    } else {
      vel_sum_val22 <- vel_sum_val22 + vel_day_sum_val22
    }
    
    total_steps_val22 <- total_steps_val22 + dim(vel_val22)[3]
    
    rm(u_val22, v_val22, u_crop_val22, v_crop_val22, vel_val22, vel_day_sum_val22)
    gc()
  }
  
  # Velocidad media por celda
  vel_mean_val22 <- vel_sum_val22 / total_steps_val22
  
  currentsdf_val22 <- expand.grid(
    x = as.numeric(coords_lon_val22),
    y = as.numeric(coords_lat_val22)
  )
  currentsdf_val22$velocity <- as.numeric(vel_mean_val22)
  
  return(currentsdf_val22)
}

#--------------------------------------------------------------
# Ejecutar función (media espacial)
#--------------------------------------------------------------
currentsdf_val22 <- procesar_intervalo_val22(
  dates_interval_val22,
  lon_range_val22,
  lat_range_val22
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Media espacial por timestep (serie temporal)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
df_list_val22 <- list()

for (i in seq_along(dates_interval_val22)) {
  day_val22 <- dates_interval_val22[i]
  
  url_val22 <- paste0(
    "https://thredds.socib.es/thredds/dodsC/operational_models/oceanographical/hydrodynamics/wmop_surface/",
    format(day_val22, "%Y/%m/"),
    "roms_wmop_surface_", format(day_val22, "%Y%m%d"), ".nc"
  )
  
  nc_val22 <- nc_open(url_val22)
  lon_val22 <- ncvar_get(nc_val22, "lon_uv")
  lat_val22 <- ncvar_get(nc_val22, "lat_uv")
  u_val22 <- ncvar_get(nc_val22, "u")
  v_val22 <- ncvar_get(nc_val22, "v")
  time_val22 <- ncvar_get(nc_val22, "ocean_time")
  time_origin_val22 <- as.POSIXct("1968-05-23 00:00:00", tz = "UTC")
  time_posix_val22 <- time_origin_val22 + time_val22
  nc_close(nc_val22)
  
  idx_lon_val22 <- which(lon_val22 >= lon_range_val22[1] & lon_val22 <= lon_range_val22[2])
  idx_lat_val22 <- which(lat_val22 >= lat_range_val22[1] & lat_val22 <= lat_range_val22[2])
  u_crop_val22 <- u_val22[idx_lon_val22, idx_lat_val22, , drop = FALSE]
  v_crop_val22 <- v_val22[idx_lon_val22, idx_lat_val22, , drop = FALSE]
  
  mean_vel_val22 <- apply(
    sqrt(u_crop_val22^2 + v_crop_val22^2),
    3, mean, na.rm = TRUE
  )
  
  df_day_val22 <- data.frame(
    time = time_posix_val22,
    velocity = mean_vel_val22
  )
  
  df_list_val22[[i]] <- df_day_val22
}

df_time_val22 <- do.call(rbind, df_list_val22)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Gráfico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Sys.setlocale("LC_TIME", "C")  # meses en inglés

ggplot(df_time_val22, aes(x = time, y = velocity)) +
  geom_line(color = "#1f78b4", size = 1) +
  annotate("rect",
           xmin = as.POSIXct("2022-11-29 04:28:00", tz = "UTC"),
           xmax = as.POSIXct("2022-11-29 06:28:00", tz = "UTC"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  scale_x_datetime(
    limits = c(
      as.POSIXct("2022-11-25 00:00:00", tz = "UTC"),
      as.POSIXct("2022-12-03 00:00:00", tz = "UTC")
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
    panel.grid = element_blank()   # 👈 elimina todas las líneas de grid
  )

#PLOT DESDE QUE SE PUSO HASTA QUE SE PERDIÓ

Sys.setlocale("LC_TIME", "C")  # meses en inglés
df_time_val22_sub <- df_time_val22 %>%
  dplyr::filter(time <= as.POSIXct("2022-11-29 06:28:00", tz = "UTC"))


# Crear secuencia de fechas cada 7 días
breaks_val22 <- seq(
  from = as.POSIXct("2022-08-23 08:43:00", tz = "UTC"),
  to   = as.POSIXct("2022-11-29 08:43:00", tz = "UTC"),
  by   = "7 days"
)

ggplot(df_time_val22_sub, aes(x = time, y = velocity)) +
  geom_line(color = "#1f78b4", size = 0.5) +
  
  # Área roja de pérdida de boya
  annotate("rect", 
           xmin = as.POSIXct("2022-11-29 04:28:00", tz = "UTC"),
           xmax = as.POSIXct("2022-11-29 06:28:00", tz = "UTC"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.5) +
  
  # Línea vertical verde de colocación
  geom_vline(xintercept = as.POSIXct("2022-08-23 08:43:00", tz = "UTC"),
             color = "green", linetype = "dashed", size = 1) +
  
  # Eje x con breaks personalizados cada 7 días
  scale_x_datetime(
    breaks = breaks_val22,
    date_labels = "%d-%b"
  ) +
  
  coord_cartesian(
    xlim = c(as.POSIXct("2022-08-23 08:43:00", tz = "UTC"),
             as.POSIXct("2022-11-29 06:28:00", tz = "UTC"))
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
