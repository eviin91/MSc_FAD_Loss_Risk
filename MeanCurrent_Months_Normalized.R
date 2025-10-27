#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Paquetes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(ncdf4)
library(stars)
library(terra)
library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Parámetros generales
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
lon_range <- c(2, 4)
lat_range <- c(39, 40.5)

meses <- list(
  August    = seq.Date(as.Date("2023-08-15"), as.Date("2023-08-31"), by="day"),
  September = seq.Date(as.Date("2023-09-01"), as.Date("2023-09-30"), by="day"),
  October   = seq.Date(as.Date("2023-10-01"), as.Date("2023-10-31"), by="day"),
  November  = seq.Date(as.Date("2023-11-01"), as.Date("2023-11-30"), by="day"),
  December  = seq.Date(as.Date("2023-12-01"), as.Date("2023-12-31"), by="day")
)

world <- ne_countries(scale="medium", returnclass="sf")
spain <- world[world$name=="Spain", ]
mallorca_bbox <- st_bbox(c(xmin=lon_range[1], xmax=lon_range[2],
                           ymin=lat_range[1], ymax=lat_range[2]), crs=4326)
mallorca_sf <- st_crop(spain, mallorca_bbox)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Función: rellenar NAs usando promedio de vecinos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
fill_NAcurrents <- function(star_obj) {
  rcurrents <- rast(star_obj)
  r_filledcurrents <- focal(rcurrents, w = matrix(1,3,3), fun = mean, na.rm = TRUE, NAonly = TRUE)
  st_as_stars(r_filledcurrents)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Función: procesar un mes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
procesar_mes2 <- function(dates, lon_range, lat_range) {
  vel_sum <- NULL
  total_steps <- 0
  coords_lon <- coords_lat <- NULL
  
  for (d in dates) {
    d <- as.Date(d)
    message("Procesando: ", format(d, "%Y-%m-%d"))
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
    
    idx_lon <- which(lon >= lon_range[1] & lon <= lon_range[2])
    idx_lat <- which(lat >= lat_range[1] & lat <= lat_range[2])
    
    u_crop <- u[idx_lon, idx_lat, , drop=FALSE]
    v_crop <- v[idx_lon, idx_lat, , drop=FALSE]
    
    vel <- sqrt(u_crop^2 + v_crop^2)
    vel_day_sum <- apply(vel, c(1,2), sum)
    
    if (is.null(vel_sum)) {
      vel_sum <- vel_day_sum
      coords_lon <- lon[idx_lon]
      coords_lat <- lat[idx_lat]
    } else {
      vel_sum <- vel_sum + vel_day_sum
    }
    
    total_steps <- total_steps + dim(vel)[3]
    rm(u, v, u_crop, v_crop, vel, vel_day_sum); gc()
  }
  
  vel_mean <- vel_sum / total_steps
  
  # Crear objeto stars
  dims <- st_dimensions(x = seq_along(coords_lon),
                        y = seq_along(coords_lat))
  dims$x$values <- coords_lon
  dims$y$values <- coords_lat
  vel_stars <- st_as_stars(vel_mean, dimensions = dims) %>%
    st_set_crs(4326)
  
  # Rellenar NAs
  vel_stars_filled <- fill_NAcurrents(vel_stars)
  
  # Normalizar 0-1
  currentsdf <- as.data.frame(vel_stars_filled, long = TRUE)
  currentsdf$vel_norm <- (currentsdf[[3]] - min(currentsdf[[3]], na.rm=TRUE)) / (max(currentsdf[[3]], na.rm=TRUE) - min(currentsdf[[3]], na.rm=TRUE))
  
  return(currentsdf)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Generar mapas de todos los meses (tarda)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vel_map_list2 <- list()

for (i in seq_along(meses)) {
  message("Procesando: ", meses[i])  # muestra el mes en la consola
  vel_map_list2[[i]] <- procesar_mes2(meses[[i]], lon_range, lat_range)
}


#SAVE
saveRDS(vel_map_list2, file = "vel_map_list2.rds")

#LOAD
vel_map_list2 <- readRDS("vel_map_list2.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Iterar sobre todos los meses y generar plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
for (i in seq_along(meses)) {
  currentsdf <- vel_map_list2[[i]]
  month_name <- names(meses)[i]
  
  pcurrents <- ggplot(currentsdf, aes(x = x, y = y, fill = vel_norm)) +
    geom_tile() +
    geom_sf(data = mallorca_sf, fill = "grey80", color = "white", inherit.aes = FALSE) +
    scale_fill_viridis_c(option = "plasma", na.value = "transparent", limits = c(0,1)) +
    coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    scale_y_continuous(breaks = seq(39, 40.5, 0.5)) +
    scale_x_continuous(breaks = seq(2, 4, 0.5)) +
    labs(title = paste("Currents", month_name, "2023"), fill = "Velocity (norm.)") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text  = element_text(size = 9),
          axis.text.y = element_text(angle = 90, vjust = 0.5))
  
  print(pcurrents)
}

#Combine 5 in one plot
library(patchwork)

currentplots <- list()

for (i in seq_along(vel_map_list2)) {
  currentsdf <- vel_map_list2[[i]]
  month_name <- names(meses)[i]
  
  pcurrents <- ggplot(df, aes(x = x, y = y, fill = vel_norm)) +
    geom_tile() +
    geom_sf(data = mallorca_sf, fill = "grey80", color = "white", inherit.aes = FALSE) +
    scale_fill_viridis_c(option = "plasma", na.value = "transparent", limits = c(0,1)) +
    coord_sf(xlim = lon_range, ylim = lat_range, expand = FALSE) +
    scale_y_continuous(breaks = seq(39, 40.5, 0.5)) +
    scale_x_continuous(breaks = seq(2, 4, 0.5)) +
    labs(title = month_name, fill = "Velocity (norm.)") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text  = element_text(size = 9),
          axis.text.y = element_text(angle = 90, vjust = 0.5))
  
  currentplots[[i]] <- pcurrents
}

# Combinar los 5 mapas en un solo panel
combinedcurrents <- wrap_plots(currentplots, ncol = 3)  # 2 columns
combinedcurrents


#Save

ggsave("C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/Maps/MeanCurrentPlots/CurrentVelocyty_norm.png", combinedcurrents, width = 12, height = 8, dpi = 300)
ggsave("C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/Maps/MeanCurrentPlots/CurrentVelocity_norm.pdf", combinedcurrents, width = 12, height = 8)

