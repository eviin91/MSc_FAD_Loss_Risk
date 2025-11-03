# MSc_MCA_FAD_Loss_Risk

# Spatial Risk Assessment of aFAD Loss in Mallorca

This repository contains the data processing and analysis workflow used to assess the **risk of artisanal Fish Aggregating Device (aFAD) loss** around Mallorca (Spain) using a **GIS-based Multi-Criteria Analysis (MCA)**.  
The study integrates **environmental (waves, currents)** and **anthropogenic (vessel density)** variables to produce spatial–temporal risk maps that support sustainable fisheries management.

---

## Repository Structure
├── SourceData/ # Raw datasets (NetCDF, shapefiles, .csv, etc.)
├── CurrentData/ # R scripts for current data preprocessing and MCA computation
├── WaveData/ # R scripts for wave data preprocessing and MCA computation
├── VesselData/ # R scripts for vessel data preprocessing and MCA computation
├── RiskData/ # R scripts for final risk index of aFAD loss and map computation
├── outputs/ # Generated maps and results
└── README.md # Project documentation

## Requirements

- **R (≥ 4.3)**  
- **QGIS (optional, ≥ 3.30)**
- Required R packages:
  ```r
  install.packages(c("terra", "sf", "dplyr", "tidyr", "ggplot2", "viridis", "classInt", "lubridate", "stars"))

## WorkFlow Overview

Data acquisition
Wave height (VHM0) from Copernicus Marine Service (CMEMS)
Current velocity from Metocean Data Repository of the Balearic Islands Coastal Observing and Forecasting System, via the Western Mediterranean Operational Model (SOCIB) 
Vessel density data from EMODnet Human Activities
Preprocessing in R
Extraction and normalization of raster variables
Calculation of the 90th percentile (p90) for wave height and current velocity
Spatial clipping to the Mallorca domain
Multi-Criteria Analysis (MCA)
Weighted linear combination of standardized variables
Weights derived from regression-based importance (R² method)
Risk Classification
Risk map classified into five categories: Very Low → Very High
Visualization
Raster outputs plotted in R and finalized in QGIS for cartographic refinement (optional)

