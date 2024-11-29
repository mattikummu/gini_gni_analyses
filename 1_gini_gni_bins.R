# This script creates the inequality-income bins

rm(list = ls())


library(terra)
library(sf)

library(reldist)
library(stringr)

library(tmap)

library(tidyverse)
library(dplyr) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1 Read data ----
r_ref_wgs84_5arcmin <- rast(ncol=360*12, nrow=180*12)

r_popCount <- terra::rast("../subnatGini/data_gis/r_pop_GHS_1990_2022_5arcmin.tif")

# # population for 2019-2021
# r_popCount_ext <- extend(subset(r_popCount, (nlyr(r_popCount)-1):nlyr(r_popCount)),r_ref_wgs84_5arcmin)
# 
# # for year 2021, let's use 2020
# r_popCount_ext <- c(r_popCount_ext,subset(r_popCount_ext,2))

r_popCount_ext_mean <-  mean(subset(r_popCount,30:32), na.rm=T)


#### 2 Create masks ----


# land mask

r_adm0 <- subset(terra::rast("../subnatGini/results/rast_adm0_gini_disp_1990_2021.tif"),32)

landMask = r_adm0
landMask[!is.na(landMask)] <- 1




# Create a pop mask to mask out any areas without population

r_popCount_ext_mean[is.na(r_popCount_ext_mean) & landMask == 1] = 0
popMask <- r_popCount_ext_mean
# mask out zero values (i.e. areas with zero pop)
popMask[popMask == 0] <- NA

# All areas with pop to 1
popMask[popMask > 0] <- 1

# 
# landMask <- r_popCount_ext_mean
# # mask out zero values (i.e. areas with zero pop)
# 
# # All areas with land to 1
# landMask[!is.na(landMask)] <- 1

#### 3 Create pop weighted bins ----

source('functions/f_create_bins.R')

#  country boarders 
sf_adm0 <- read_sf("../subnatGini/data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.2, keep_shapes = T) %>%
  st_as_sf() %>% 
  filter(!iso_a3 == 'ATA')

#### 4 Raster map function ----

source('functions/f_plot_rasterMap.R')


##### 5 Inequality and income bins ----


if (dir.exists('figures/')) {
  
} else {
  dir.create('figures/')  
}

if (dir.exists('results/')) {
  
} else {
  dir.create('results/')  
}


# what size bin matrix to create (N x N)
N = 4

# Use slope for inequality and %-change for income
r_ginislope <- rast('../subnatGini/results/rast_slope_gini_disp_1990_2021.tif')*32 # multiply by 32 to get slope over the whole study period
r_incomeperc <- rast('../subnatGini/results/rast_slope_adm1_perc_gnic.tif')

# define variables
xRast <-  r_ginislope
yRast <-  r_incomeperc
pop_count <-  r_popCount_ext_mean
usedMask <-  landMask
N = 4

# use the function to create bins
inequality_income_bins_raster <- f_create_bins(r_ginislope, r_incomeperc, r_popCount_ext_mean, landMask,  N)

# plot(gini_income_bins_raster)
# save bins
writeRaster(inequality_income_bins_raster, filename=paste0('results/inequality_income_bins_raster_',N,'.tif'), overwrite = T)



