# R CUADRADA OLEAJE VALENTINA Y ULISES

library(dplyr)

# Añadimos una columna con el nombre de la boya
df_hora_val22 <- df_hora_val22 %>% mutate(boya = "Valentina")
df_hora_uli25 <- df_hora_uli25 %>% mutate(boya = "Ulises")

# Marcamos si la boya está activa (0) o pérdida (1) usando la hora de pérdida
df_hora_val22 <- df_hora_val22 %>%
  mutate(perdida = ifelse(time >= as.POSIXct("2022-11-29 04:28:00", tz="UTC"), 1, 0))

df_hora_uli25 <- df_hora_uli25 %>%
  mutate(perdida = ifelse(time >= as.POSIXct("2025-07-30 04:26:00", tz="UTC"), 1, 0))

# Combinamos los dos datasets
df_combined <- bind_rows(df_hora_val22, df_hora_uli25)

# Modelo lineal: Hm0 vs pérdida
model_global <- lm(perdida ~ Hm0, data = df_combined)

summary(model_global)$r.squared
#R2=0.004

#R CUADRADA CORRIENTES VALENTINA Y ULISES
library(dplyr)

# Añadimos columna de nombre de boya
df_time_val22 <- df_time_val22 %>% mutate(boya = "Valentina")
df_time_uli25 <- df_time_uli25 %>% mutate(boya = "Ulises")

# Marcamos pérdida de la boya
df_time_val22 <- df_time_val22 %>%
  mutate(perdida = ifelse(time >= as.POSIXct("2022-11-29 04:28:00", tz="UTC"), 1, 0))

df_time_uli25 <- df_time_uli25 %>%
  mutate(perdida = ifelse(time >= as.POSIXct("2025-07-30 04:26:00", tz="UTC"), 1, 0))

# Combinamos los datasets
df_corrientes_combined <- bind_rows(df_time_val22, df_time_uli25)

# Modelo lineal: velocidad vs pérdida
model_corrientes <- lm(perdida ~ velocity, data = df_corrientes_combined)

summary(model_corrientes)$r.squared
#1.035*e-06

#R CUADRADA VESSEL DENSITY VALENTINA Y ULISES

# ejemplo: añadir traffic al df_combined (ya contiene time, Hm0, velocity, perdida)
library(lubridate)

# supongamos que tienes:
# traffic_valentina = 0.1744643  (nov/2022)
# traffic_ulises    = 8.63724    (jul/2025)

df_combined <- df_combined %>%
  mutate(
    traffic = case_when(
      boya == "Valentina" & month(time) == 11 & year(time) == 2022 ~ 0.1744643,
      boya == "Ulises"    & month(time) == 7  & year(time) == 2025 ~ 8.63724,
      TRUE ~ NA_real_
    )
  )

# si hay filas fuera de los meses de interés (NA), y quieres mantener solo hasta pérdida:
df_combined <- df_combined %>% filter(!is.na(traffic))

# Modelo lineal con traffic (misma lógica que antes)
model_traffic <- lm(perdida ~ traffic, data = df_combined)
summary(model_traffic)$r.squared
#[1] 0.0005195848

