#--- Crear un SpatRaster stack (agosto–diciembre) -------------------------------#
library(sf)
library(terra)
library(stars)
library(dplyr)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)

#--------------
#VESSELDENSITY
#--------------
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

r_stack1[[i]] <- r

# Asignar nombres a cada capa
names(r_stack1) <- meses

raster::writeRaster(r_stack1, "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/barcos.tif")

r_stack1 <- terra::rast("C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/barcos.tif")

r_stack1 <- log1p(r_stack1)
terra::plot(r_stack1)


#CRS ARREGLADO

library(terra)
library(sf)

# 1️⃣ Raster de referencia (waves/currents)
r_ref <- rast(nrows=156, ncols=207, xmin=2, xmax=4, ymin=39, ymax=40.5, crs="EPSG:4326")

# 2️⃣ Bounding box de Mallorca
mallorca_bbox <- ext(2, 4, 39, 40.5)

# 3️⃣ Archivos de vessel density y nombres de meses
vessel_archivos <- c(
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230801.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20230901.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231001.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231101.tif",
  "C:/Users/eviin/Downloads/CAIB-Llampuga/TFM_maps/CopernicusData/EMODnet_HA_Vessel_Density_all_2017-2023/vesseldensity_all_20231201.tif"
)
meses <- c("August", "September", "October", "November", "December")

# 4️⃣ Lista para almacenar los rasters procesados
r_stack_list <- list()

# 5️⃣ Procesar cada archivo
for(i in seq_along(vessel_archivos)) {
  r <- rast(vessel_archivos[i])
  
  # Valores negativos a NA
  r[r < 0] <- NA
  
  # Reproyectar a lon/lat (EPSG:4326)
  r_latlon <- project(r, "EPSG:4326", method="bilinear")
  
  # Recortar a Mallorca
  r_crop <- crop(r_latlon, mallorca_bbox)
  
  # Remuestrear para alinear con raster de referencia
  r_aligned <- resample(r_crop, r_ref, method="bilinear")
  
  # Guardar en la lista
  r_stack_list[[i]] <- r_aligned
}

# 6️⃣ Convertir la lista a SpatRaster (stack)
r_stack1 <- rast(r_stack_list)
names(r_stack1) <- meses
r_stack1 <- log1p(r_stack1)

# 7️⃣ Revisar
plot(r_stack1)
terra::plot(r_stack1)
summary(r_stack1)


#--------
#WAVES
#--------

# Bounding box de Mallorca
mallorca_bbox <- st_bbox(c(xmin=2, xmax=4, ymin=39, ymax=40.5), crs = 4326)
mallorca_ext <- ext(mallorca_bbox["xmin"], mallorca_bbox["xmax"],
                    mallorca_bbox["ymin"], mallorca_bbox["ymax"])

# Bounding box de Mallorca
mallorca_ext <- ext(2, 4, 39, 40.5)

# Añadir columna de mes
VHM0_long <- VHM0_long %>%
  mutate(month = month(time, label = TRUE, abbr = FALSE))  # "agosto", "septiembre", etc.


# Filtrar datos por mes y crear rasters
df_aug <- VHM0_long %>% filter(month == "agosto")
r_aug <- rast(df_aug[, c("x","y","VHM0")], type="xyz")
r_aug <- crop(r_aug, mallorca_ext)

df_sep <- VHM0_long %>% filter(month == "septiembre")
r_sep <- rast(df_sep[, c("x","y","VHM0")], type="xyz")
r_sep <- crop(r_sep, mallorca_ext)

df_oct <- VHM0_long %>% filter(month == "octubre")
r_oct <- rast(df_oct[, c("x","y","VHM0")], type="xyz")
r_oct <- crop(r_oct, mallorca_ext)

df_nov <- VHM0_long %>% filter(month == "noviembre")
r_nov <- rast(df_nov[, c("x","y","VHM0")], type="xyz")
r_nov <- crop(r_nov, mallorca_ext)

df_dec <- VHM0_long %>% filter(month == "diciembre")
r_dec <- rast(df_dec[, c("x","y","VHM0")], type="xyz")
r_dec <- crop(r_dec, mallorca_ext)

# Combinar en un stack
r_stack_waves <- c(r_aug, r_sep, r_oct, r_nov, r_dec)
names(r_stack_waves) <- c("August","September","October","November","December")

plot(r_stack_waves)
summary(r_stack_waves)

#Para p90
library(terra)
library(dplyr)

# Bounding box de Mallorca
mallorca_ext <- ext(2, 4, 39, 40.5)

# Meses en español
meses <- c("agosto","septiembre","octubre","noviembre","diciembre")

# Lista para guardar rasters
r_stack_list <- list()

for (m in meses) {
  
  # Filtrar datos del mes y dentro del bbox de Mallorca
  df_mes <- VHM0_long %>%
    filter(month == m,
           x >= 2, x <= 4,
           y >= 39, y <= 40.5)
  
  # Calcular P90 del mes
  p90_mes <- quantile(df_mes$VHM0, 0.9, na.rm = TRUE)
  
  # Frecuencia por celda ≥ P90
  freq_mes <- df_mes %>%
    group_by(x, y) %>%
    summarise(freq = mean(VHM0 >= p90_mes, na.rm = TRUE), .groups = "drop")
  
  # Crear raster directamente
  r_mes <- rast(freq_mes[, c("x","y","freq")], type = "xyz")
  
  # Recortar al bbox de Mallorca
  r_mes <- crop(r_mes, mallorca_ext)
  
  # Guardar en lista
  r_stack_list[[m]] <- r_mes
}

# Crear stack final
r_stack2 <- rast(r_stack_list)
names(r_stack2) <- c("August","September","October","November","December")

# Visualizar
plot(r_stack2)
summary(r_stack2)
library(terra)

# Calcular mínimo y máximo global del stack
global_min <- global(r_stack2, "min", na.rm = TRUE)[1,1]
global_max <- global(r_stack2, "max", na.rm = TRUE)[1,1]

# Graficar cada capa con la misma escala
plot(r_stack2, zlim = c(global_min, global_max))

#-------
#CURRENTS
#-------

# AGOSTO
df_aug <- meses_df[["August"]]
r_aug <- rast(df_aug[, c("x","y","vel")], type="xyz")
r_aug <- focal(r_aug, w = matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)

# SEPTIEMBRE
df_sep <- meses_df[["September"]]
r_sep <- rast(df_sep[, c("x","y","vel")], type="xyz")
r_sep <- focal(r_sep, w = matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)

# OCTUBRE
df_oct <- meses_df[["October"]]
r_oct <- rast(df_oct[, c("x","y","vel")], type="xyz")
r_oct <- focal(r_oct, w = matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)

# NOVIEMBRE
df_nov <- meses_df[["November"]]
r_nov <- rast(df_nov[, c("x","y","vel")], type="xyz")
r_nov <- focal(r_nov, w = matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)

# DICIEMBRE
df_dec <- meses_df[["December"]]
r_dec <- rast(df_dec[, c("x","y","vel")], type="xyz")
r_dec <- focal(r_dec, w = matrix(1,3,3), fun=mean, na.rm=TRUE, NAonly=TRUE)

# Crear stack
r_stack3 <- c(r_aug, r_sep, r_oct, r_nov, r_dec)
names(r_stack3) <- c("August","September","October","November","December")

# Visualizar
plot(r_stack3)

#----------------------------------
#Normalizar valores stacks 1, 2 y 3
#----------------------------------
r_stack1_aligned <- resample(r_stack1, r_ref, method="bilinear")
r_stack3_aligned <- resample(r_stack3, r_ref, method="bilinear")

# Vessel density
r_stack1_norm <- (r_stack1_aligned - minmax(r_stack1_aligned)[1]) / 
  (minmax(r_stack1_aligned)[2] - minmax(r_stack1_aligned)[1])

# Waves
r_stack2_norm <- (r_stack2 - minmax(r_stack2)[1]) / 
  (minmax(r_stack2)[2] - minmax(r_stack2)[1])
crs(r_stack2_norm) <- "EPSG:4326"


# Currents
r_stack3_norm <- (r_stack3_aligned - minmax(r_stack3_aligned)[1]) / 
  (minmax(r_stack3_aligned)[2] - minmax(r_stack3_aligned)[1])


#Risk map

r_ref <- r_stack2  # waves, por ejemplo
r_stack1_norm_aligned <- resample(r_stack1_norm, r_ref, method="bilinear")
r_stack3_norm_aligned <- resample(r_stack3_norm, r_ref, method="bilinear")
r_risk33 <- r_stack1_norm_aligned*0.33 + r_stack2_norm*0.33 + r_stack3_norm_aligned*0.33
r_riskR2 <- r_stack1_norm_aligned*0.115 + r_stack2_norm*0.885 + r_stack3_norm_aligned*0.0002

# Rellenar NAs en cada capa
w <- matrix(1,3,3)  # ventana 3x3
r_risk_filled <- r_risk33
for(i in 1:nlyr(r_risk33)){
  r_risk_filled[[i]] <- focal(r_risk33[[i]], w=w, fun=mean, na.rm=TRUE, NAonly=TRUE)
}

# Mallorca en alta definición
mallorca_sf <- ne_countries(scale = "large", returnclass = "sf")
mallorca_sf <- mallorca_sf[mallorca_sf$name == "Spain", ]

# Vector de nombres de meses
meses <- c("August","September","October","November","December")

# Configurar panel 2x3
par(mfrow=c(2,3), mar=c(1,1,3,1))

# Rango común para la leyenda
zlim <- range(values(r_risk_filled), na.rm=TRUE)

# Plot
for(i in 1:5){
  r <- r_risk_filled[[i]]
  plot(r, 
       col=viridis(20, option="inferno"), 
       axes=FALSE, box=FALSE, 
       main=meses[i],
       zlim=zlim)
  plot(st_geometry(mallorca_sf), add=TRUE, col="grey80", border=NA)
}

# Media de todos los meses
r_risk_mean <- app(r_risk_filled, mean, na.rm=TRUE)

# Plot único
plot(r_risk_mean, 
     col=viridis(20, option="inferno"), 
     axes=FALSE, box=FALSE, 
     main="Mean (Aug–Dec)")
plot(st_geometry(mallorca_sf), add=TRUE, col="grey80", border=NA)



#LOW, MODERATE, HIGH + FADs (3 CLASES)

# --- Raster sets ---
r_risk33 <- r_stack1_norm_aligned*0.33 + r_stack2_norm*0.33 + r_stack3_norm_aligned*0.33
r_riskR2 <- r_stack1_norm_aligned*0.115 + r_stack2_norm*0.885 + r_stack3_norm_aligned*0.0002

# --- Clasificar y rellenar NAs ---
classify_and_fill <- function(r){
  w <- matrix(1,3,3)
  r_cat <- classify(r, rbind(
    c(0, 0.33, 1),
    c(0.33, 0.66, 2),
    c(0.66, 1, 3)
  ))
  levels(r_cat) <- data.frame(ID=1:3, Risk=c("Low","Moderate","High"))
  for(i in 1:nlyr(r_cat)){
    r_cat[[i]] <- focal(r_cat[[i]], w=w, fun=mean, na.rm=TRUE, NAonly=TRUE)
  }
  return(r_cat)
}

r_risk33_cat <- classify_and_fill(r_risk33)
r_riskR2_cat <- classify_and_fill(r_riskR2)

# --- Nombres de capas ---
meses <- c("August","September","October","November","December")
names(r_risk33_cat) <- meses
names(r_riskR2_cat) <- meses

# --- Raster promedio ---
r_risk33_mean <- app(r_risk33_cat, mean, na.rm=TRUE)
r_riskR2_mean  <- app(r_riskR2_cat, mean, na.rm=TRUE)

# --- Geometrías ---
mallorca_sf <- ne_countries(scale="large", returnclass="sf")
mallorca_sf <- mallorca_sf[mallorca_sf$name=="Spain", ]
fad_geom <- st_geometry(st_read("C:/Users/eviin/Downloads/CAIB-Llampuga/Mapa_llampuga_2023.kml"))
st_crs(fad_geom) <- 4326

# --- Función para plotear 5 meses + promedio con FADs ---

plot_risk_with_fads <- function(r_cat, r_mean, title_mean="Mean", cols=NULL){
  if(is.null(cols)) cols <- c("Low"="#66AA33","Moderate"="#FFDD33","High"="#f46d43")
  
  par(mfrow=c(2,3), mar=c(1,1,3,1))
  for(i in 1:5){
    r <- r_cat[[i]]
    plot(r, col=cols, legend=FALSE, axes=FALSE, box=FALSE, main=names(r))
    plot(st_geometry(mallorca_sf), add=TRUE, col="grey80", border="white")
    plot(fad_geom, add=TRUE, pch=16, col="red", cex=0.7)
  }
  
  # Sexto mapa: promedio
  plot(r_mean, col=cols, legend=FALSE, axes=FALSE, box=FALSE, main=title_mean)
  plot(st_geometry(mallorca_sf), add=TRUE, col="grey80", border="white")
  plot(fad_geom, add=TRUE, pch=16, col="red", cex=0.7)
  
  # Leyenda a parte
  par(mfrow=c(1,1))
  plot.new()
  legend("center",
         legend=c("Low","Moderate","High","FADs"),
         fill=c("#66AA33","#FFDD33","#f46d43",NA),
         border=NA,
         pch=c(NA,NA,NA,16),
         col=c(NA,NA,NA,"red"),
         bty="n", cex=1.1)
}

# --- Plotear ---
plot_risk_with_fads(r_risk33_cat, r_risk33_mean, title_mean="Mean Aug–Dec (Equal weights)")
plot_risk_with_fads(r_riskR2_cat, r_riskR2_mean, title_mean="Mean Aug–Dec (Weighted)")


#5 CLASSES
# --- Paleta verde → rojo ---
cols_5_gr <- c("Very Low"="#006400",
               "Low"="#66CC66",
               "Moderate"="#FFFF66",
               "High"="#FF9933",
               "Very High"="#FF3333")

# --- Función de plot con nueva paleta ---
plot_riskR2_5_greenred <- function(r_cat, r_mean, title_mean="Mean", cols=NULL){
  if(is.null(cols)) cols <- cols_5_gr
  
  par(mfrow=c(3,2), mar=c(1,1,3,1))
  for(i in 1:5){
    r <- r_cat[[i]]
    plot(r, col=cols, legend=FALSE, axes=FALSE, box=FALSE, main=names(r))
    plot(st_geometry(mallorca_sf), add=TRUE, col="grey80", border="white")
    plot(fad_geom, add=TRUE, pch=16, col="red", cex=0.7)
  }
  
  # Sexto mapa: promedio
  plot(r_mean, col=cols, legend=FALSE, axes=FALSE, box=FALSE, main=title_mean)
  plot(st_geometry(mallorca_sf), add=TRUE, col="grey80", border="white")
  plot(fad_geom, add=TRUE, pch=16, col="red", cex=0.7)
  
  # Leyenda a parte
  par(mfrow=c(1,1))
  plot.new()
  legend("center",
         legend=c(names(cols),"FADs"),
         fill=c(cols,NA),
         border=NA,
         pch=c(rep(NA,5),16),
         col=c(rep(NA,5),"red"),
         bty="n", cex=1.1)
}

# --- Plotear ---
plot_riskR2_5_greenred(r_riskR2_5, r_riskR2_mean5, title_mean="Mean Aug–Dec")

###################

# --- Reclasificación a 5 clases ---
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
labels <- c("Very Low", "Low", "Moderate", "High", "Very High")

# Clasificar r_riskR2 en 5 clases (Very Low → Very High)
classify_5 <- function(r){
  w <- matrix(1,3,3)
  r_cat <- classify(r, rbind(
    c(0, 0.2, 1),
    c(0.2, 0.4, 2),
    c(0.4, 0.6, 3),
    c(0.6, 0.8, 4),
    c(0.8, 1, 5)
  ))
  levels(r_cat) <- data.frame(ID=1:5, Risk=c("Very Low","Low","Moderate","High","Very High"))
  for(i in 1:nlyr(r_cat)){
    r_cat[[i]] <- focal(r_cat[[i]], w=w, fun=mean, na.rm=TRUE, NAonly=TRUE)
  }
  return(r_cat)
}

# Crear objeto y su media
r_riskR2_5 <- classify_5(r_riskR2)
r_riskR2_mean5 <- app(r_riskR2_5, mean, na.rm=TRUE)


r_riskR2_5cat <- classify_5(r_riskR2)
r_riskR2_5_mean <- app(r_riskR2_5cat, mean, na.rm=TRUE)

# --- Colores ---
cols5 <- c("Very Low"="#006400",
           "Low"="#66CC66",
           "Moderate"="#FFFF66",
           "High"="#FF9933",
           "Very High"="#FF3333")

# --- Función para plotear ---
plot_risk_5cat <- function(r_cat, r_mean, fad_geom, mallorca_sf){
  meses <- c("August","September","October","November","December")
  
  par(mfrow=c(2,3), mar=c(1,1,3,1))
  
  # Mapas de meses
  for(i in 1:5){
    r <- r_cat[[i]]
    plot(r, col=cols5, legend=FALSE, axes=FALSE, box=FALSE, main=meses[i])
    plot(st_geometry(mallorca_sf), add=TRUE, col="grey80", border="white")
    plot(fad_geom, add=TRUE, pch=16, col="black", cex=0.7)
  }
  
  # Promedio
  plot(r_mean, col=cols5, legend=FALSE, axes=FALSE, box=FALSE, main="Mean Aug–Dec")
  plot(st_geometry(mallorca_sf), add=TRUE, col="grey80", border="white")
  plot(fad_geom, add=TRUE, pch=16, col="black", cex=0.7)
  
  # Leyenda aparte
  par(mfrow=c(1,1))
  plot.new()
  legend("center",
         legend=c(labels, "FAD lines"),
         fill=c(cols5, NA),
         border=NA,
         lty=c(rep(NA,5),1),
         col=c(rep(NA,5),"black"),
         bty="n", cex=1.1)
}

# --- Plotear ---
plot_risk_5cat(r_riskR2_5cat, r_riskR2_5_mean, fad_geom, mallorca_sf)


#FAD RISK PER MONTH (%)


meses <- c("August","September","October","November","December")

for(m in meses){
  vals <- terra::extract(r_riskR2_5[[m]], fad_vect)[,2]
  counts <- table(vals, useNA="ifany")
  perc <- round(counts / sum(!is.na(vals)) * 100, 2)
  
  cat("\n---", m, "---\n")
  df <- data.frame(Class = names(counts),
                   Count = as.numeric(counts),
                   Percent = as.numeric(perc))
  print(df)
}

#Mean
# --- Definir breaks y etiquetas ---
breaks <- c(0, 1, 2, 3, 4, 5)  # para reclasificar promedio a enteros 1–5
labels <- c("Very Low", "Low", "Moderate", "High", "Very High")

# --- Reclasificar promedio ---
r_mean_cat <- classify(r_riskR2_5_mean, cbind(breaks[-length(breaks)], breaks[-1], 1:5))
levels(r_mean_cat) <- data.frame(ID=1:5, Risk=labels)

# --- Extraer valores en posiciones de los FADs ---
vals_mean <- terra::extract(r_mean_cat, fad_vect)[,2]

# --- Contar por clase y calcular porcentajes ---
counts <- table(vals_mean, useNA="ifany")
perc <- round(counts / sum(!is.na(vals_mean)) * 100, 2)

# --- Mostrar tabla ---
df_mean <- data.frame(Class = names(counts),
                      Count = as.numeric(counts),
                      Percent = as.numeric(perc))
df_mean
