#### This script plots inequality-income bins 


#install.packages(c('paletteer','rasterVis'))


library(terra)
library(rnaturalearth)

library(paletteer)
library(tmap)
library(rasterVis)
library(ggplot2)
library(sf)

library(tidyverse)
library(dplyr) 


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### 1 create outline ----



r_adm0 <- subset(terra::rast("../subnatGini/results/rast_adm0_gini_disp_1990_2021.tif"),32)

shoreline = r_adm0
shoreline[!is.na(shoreline)] <- 1

shoreline_rob <- terra::project(x = shoreline, y = "+proj=robin", method = "near")

sf_shoreline <- terra::as.polygons(shoreline_rob) |> st_as_sf()
plot(sf_shoreline)

# # breaks:
# nBreaks = 8

# increase the number of max.categories in tmap
tmap_options(max.categories = 100)


sf_adm0 <- read_sf("../subnatGini/data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.35, keep_shapes = T) %>%
  st_as_sf() %>% 
  filter(!iso_a3 == 'ATA')

# 
# seed.in = c(seq(11,18), seq(21,28), seq(31,38), seq(41,48), 
#             seq(51,58), seq(61,68), seq(71,78), seq(81,88))



#### 2 set colour palette ----

source('functions/f_colmat.R')


#### 3 plot bins function ----

source('functions/f_plotBins.R')


#### 4 inequality - income bins ----

# read bins
gini_gni_bins <- terra::rast('results/inequality_income_bins_raster_4.tif')

nBrks = 4
# names
xNameSet = "Inequality (GINI) increases"
yNameSet = "Income (GNI) increases"

# Create the colour matrix
col.matrix_unempl_gnic <- f_colmat(nbreaks = nBrks, breakstyle = "quantile",
                                 xlab = xNameSet, ylab = yNameSet, 
                                 bottomright = "#AC2C7A", upperright = "#391415",
                                 bottomleft = "#D3D3D3", upperleft = "#5C9938",
                                 saveLeg = FALSE, plotLeg = TRUE)


p_incEqua <- f_plotBins(rBins = gini_gni_bins,
                        colPalette = col.matrix_unempl_gnic,
                        xName = xNameSet,
                        yName = yNameSet,
                        nBreaks = nBrks)
p_incEqua



cowplot::save_plot(plot=p_incEqua, filename = paste0('figures/Fig3c_bins_N',nBrks,'_','GINI','_','GNI','.pdf')) #base_height = 6, base_width = 24


