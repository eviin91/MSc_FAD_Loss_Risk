#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# BOYA ULISES 2025: PROMEDIO DE HM PARA PERIODO CONCRETO
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(stars)
library(ggplot2)

# 1. Cargar el archivo NetCDF
ruta_uli25 <- "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/Ulises_Waves_250720_250801.nc"
wave_data_uli25 <- read_stars(ruta_uli25, proxy = FALSE)

# 2. Extraer los valores de tiempo del NetCDF
fechas_uli25 <- st_get_dimension_values(wave_data_uli25, "time")

# 3. Crear índice con el intervalo
index_uli25 <- which(fechas_uli25 >= as.POSIXct("2025-07-24 00:00:00", tz = "UTC") &
                       fechas_uli25 <= as.POSIXct("2025-07-30 00:00:00", tz = "UTC"))

# 4. Subconjunto
wave_sub_uli25 <- wave_data_uli25[,,,index_uli25]

# 5. Media espacial por hora
df_hora_uli25 <- st_apply(wave_sub_uli25, "time", function(x) mean(x, na.rm = TRUE))

# 6. Convertir a data.frame
df_hora_uli25 <- data.frame(
  time = st_get_dimension_values(wave_sub_uli25, "time"),
  Hm0 = as.numeric(df_hora_uli25[[1]])
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Gráfico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

Sys.setlocale("LC_TIME", "C")  # fuerza meses en inglés

ggplot(df_hora_uli25, aes(x = time, y = Hm0)) +
  geom_line(color = "#1f78b4", size = 1) +
  annotate("rect", 
           xmin = as.POSIXct("2025-07-29 04:30:00", tz = "UTC"),
           xmax = as.POSIXct("2025-07-30 04:26:00", tz = "UTC"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  scale_x_datetime(
    breaks = "1 day",
    date_labels = "%d-%b"
  ) +
  labs(x = NULL,
       y = "Mean Significant Wave Height (m)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank()   # 👈 elimina todas las líneas de grid
  )

#Plot desde que se colocó hasta que se perdió

ggplot(df_hora_uli25, aes(x = time, y = Hm0)) +
  geom_line(color = "#1f78b4", size = 1) +
  
  # periodo de pérdida de la boya
  annotate("rect", 
           xmin = as.POSIXct("2025-07-29 04:30:00", tz = "UTC"),
           xmax = as.POSIXct("2025-07-30 04:26:00", tz = "UTC"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  
  # línea vertical de colocación
  geom_vline(xintercept = as.POSIXct("2025-07-24 00:00:00", tz = "UTC"),
             color = "green", linetype = "dashed", size = 1) +
  
  # texto explicativo
  annotate("text", x = as.POSIXct("2025-07-24 06:00:00", tz = "UTC"), 
           y = max(df_hora_uli25$Hm0, na.rm = TRUE), 
           label = NULL, color = "green", angle = 90, vjust = -0.5, size = 4) +
  
  scale_x_datetime(
    breaks = "1 day",
    date_labels = "%d-%b"
  ) +
  labs(x = NULL,
       y = "Mean Significant Wave Height (m)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank()
  )


