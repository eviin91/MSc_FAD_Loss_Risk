# 1. Cargar el archivo NetCDF de la boya Valentina 2022
ruta_val22 <- "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/Valentina_Waves_220820_221201.nc"
wave_val22 <- read_stars(ruta_val22, proxy = FALSE)

# 2. Extraer los valores de tiempo
fechas_val22 <- st_get_dimension_values(wave_val22, "time")

# 3. Crear índice con el intervalo deseado (25 nov – 3 dic 2022)
index_val22 <- which(
  fechas_val22 >= as.POSIXct("2022-08-23 08:43:00", tz = "UTC") &
    fechas_val22 <= as.POSIXct("2022-11-29 06:28:00", tz = "UTC")
)

# 4. Subconjunto temporal
wave_sub_val22 <- wave_val22[,,, index_val22]

# 5. Calcular la media espacial por hora
wave_mean_val22 <- st_apply(
  wave_sub_val22, "time", function(x) mean(x, na.rm = TRUE)
)

# 6. Convertir a data.frame con nombres de columnas claros
df_hora_val22 <- data.frame(
  time = st_get_dimension_values(wave_sub_val22, "time"),
  Hm0  = as.numeric(wave_mean_val22[[1]])
)

# 7. Plot
ggplot(df_hora_val22, aes(x = time, y = Hm0)) +
  geom_line(color = "#1f78b4", size = 1) +
  annotate("rect", 
           xmin = as.POSIXct("2022-11-29 04:28:00", tz = "UTC"),
           xmax = as.POSIXct("2022-11-29 06:28:00", tz = "UTC"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.2) +
  scale_x_datetime(
    limits = range(df_hora_val22$time),
    date_breaks = "1 day",
    date_labels = "%d-%b"
  ) +
  labs(x = NULL, y = "Mean Significant Wave Height (m)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )

#PLOT DESDE QUE SE COLOCÓ HASTA QUE SE PERDIÓ
# Crear secuencia de fechas cada 7 días
breaks_val22 <- seq(
  from = as.POSIXct("2022-08-23 08:43:00", tz = "UTC"),
  to   = as.POSIXct("2022-11-29 08:43:00", tz = "UTC"),
  by   = "7 days"
)

ggplot(df_hora_val22, aes(x = time, y = Hm0)) +
  geom_line(color = "#1f78b4", size = 1) +
  # Área roja de pérdida
  annotate("rect", 
           xmin = as.POSIXct("2022-11-29 04:28:00", tz = "UTC"),
           xmax = as.POSIXct("2022-11-29 06:28:00", tz = "UTC"),
           ymin = -Inf, ymax = Inf,
           fill = "red", alpha = 0.5) + #alpha controla opacidad de la línea
  # Línea vertical verde de colocación
  geom_vline(xintercept = as.POSIXct("2022-08-23 08:43:00", tz = "UTC"),
             color = "green", linetype = "dashed", size = 1) +
  # Eje x cada 7 días y límite hasta el 29/11 06:28
  # Eje x con breaks personalizados cada 7 días
  scale_x_datetime(
    breaks = breaks_val22,
    date_labels = "%d-%b"
  ) +
  labs(x = NULL, y = "Mean Significant Wave Height (m)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank()
  )

