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

#### Read data ----
r_ref_wgs84_5arcmin <- rast(ncol=360*12, nrow=180*12)

r_popCount <- terra::rast("data_gis/r_pop_GHS_1990_2022_5arcmin.tif")

# # population for 2019-2021
# r_popCount_ext <- extend(subset(r_popCount, (nlyr(r_popCount)-1):nlyr(r_popCount)),r_ref_wgs84_5arcmin)
# 
# # for year 2021, let's use 2020
# r_popCount_ext <- c(r_popCount_ext,subset(r_popCount_ext,2))

r_popCount_ext_mean <-  mean(subset(r_popCount,30:32), na.rm=T)


#### Create masks ----

# Create a pop mask to mask out any areas without population
popMask <- r_popCount_ext_mean
# mask out zero values (i.e. areas with zero pop)
popMask[popMask == 0] <- NA

# All areas with pop to 1
popMask[popMask > 0] <- 1

# land mask
landMask <- r_popCount_ext_mean
# mask out zero values (i.e. areas with zero pop)

# All areas with land to 1
landMask[!is.na(landMask)] <- 1

#### Create pop weighted bins ----

# function to create inequality-income bins
create_bins <- function(xRast = r_gini_mean_rev, 
                        yRast = r_gnic_mean_filled, 
                        pop_count = r_popCount_ext_mean, 
                        usedMask = landMask, 
                        N = 4) {
  
  
  
  # mask out non-populated areas from the data
  xRast[is.na(usedMask)] <- NA
  yRast[is.na(usedMask)] <- NA
  
  # assign NA to all pop_cells where no data for s-e data
  pop_count[is.na(xRast)] <- NA
  pop_count[is.na(yRast)] <- NA
  
  # create mask raster from cells where population is not Na
  tot_mask <- !is.na(pop_count) # create for which any data is nan
  
  # assign NA to all inequalirt and income cells where mask is 0 (i.e. no population)
  yRast[tot_mask == 0] <- NA
  xRast[tot_mask == 0] <- NA
  
  
  
  
  # raster to vector, drop na also
  gini_vec <- na.omit(c(as.matrix(xRast)))
  income_vec <- na.omit(c(as.matrix(yRast)))
  pop_vec <- na.omit(c(as.matrix(pop_count)))
  
  # create r_gini bins, based on N quantiles 
  # the lower the gni, the lower the inequality
  #gini_wtd_quantiles <- reldist::wtd.quantile (gini_vec, q = seq(0, 1, length.out = N+1), weight=pop_vec) #try also N+1
  gini_wtd_quantiles <- reldist::wtd.quantile (gini_vec, q = seq(0, 1, length.out = N+1)) #try also N+1
  
  #gini_wtd_quantiles <- c(-1, -0.05,0,0.05, 1)
  gini_wtd_quantiles[1] <- -Inf
  gini_wtd_quantiles[length(gini_wtd_quantiles)] <- Inf
  gini_wtd_quantiles[3] <- 0
  
  # let's use the larger of the ABS(25th) and ABS(75th) for both
  gini_wtd_quantiles[4] <- max(abs(gini_wtd_quantiles[2]),abs(gini_wtd_quantiles[4]))
  gini_wtd_quantiles[2] <- -gini_wtd_quantiles[4]
  
  
  # # Create r_gini bins based on a threshold 0.05
  # # <= -0.05 strong decrease
  # # -0.05 < gini <= 0 :decrease
  # # 0 < gini < 0.05: increase
  # # >= 0.05: strong increase
  # gini_wtd_quantiles <- c(-1, -0.05,0,0.05, 1)
  # gini_wtd_quantiles[1] <- -Inf
  # gini_wtd_quantiles[length(gini_wtd_quantiles)] <- Inf
  # #T_quantiles <- quantile (T_vec, p = seq(0, 1, length.out = N+1))
  
  # income_wtd_quantiles <- reldist::wtd.quantile (income_vec, q = seq(0, 1, length.out = N+1), weight=pop_vec) #try also N+1
  income_wtd_quantiles <- reldist::wtd.quantile (income_vec, q = seq(0, 1, length.out = N+1)) #try also N+1
  income_wtd_quantiles[1] <- -Inf
  income_wtd_quantiles[length(income_wtd_quantiles)] <- Inf
  income_wtd_quantiles[3] <- 0
  
  # let's use the larger of the ABS(25th) and ABS(75th) for both
  income_wtd_quantiles[4] <- max(abs(income_wtd_quantiles[2]),abs(income_wtd_quantiles[4]))
  income_wtd_quantiles[2] <- -income_wtd_quantiles[4]
  
  comb <- as.matrix(c(gini_wtd_quantiles,income_wtd_quantiles)) %>% 
    as_tibble()
  
  write_csv(comb, 'results/gini_gnic_slope_thresholds.csv')
  
  # # Income bins based on a threshold 10%
  # # <= -0.1 strong decrease
  # # -0.1 < income <= 0 :decrease
  # # 0 < income < 0.1: increase
  # # >= 0.1: strong increase
  # 
  # income_wtd_quantiles <- c(-1, -0.1, 0, 0.1, 1)
  # income_wtd_quantiles[1] <- -Inf
  # income_wtd_quantiles[length(income_wtd_quantiles)] <- Inf
  
  
  # Categorize
  gini_cut <- cut(gini_vec, gini_wtd_quantiles, include.lowest=TRUE)
  gini_bins <- (as.numeric(gini_cut)-1) + 1 
  #unique(gini_bins)
  
  income_cut <- cut(income_vec, income_wtd_quantiles, include.lowest=TRUE)
  income_bins <- (as.numeric(income_cut)-1) + 1 
  #sort(unique(income_bins))
  
  
  # comb_bins
  comb_bins = gini_bins * 10 + income_bins
  
  # GINI in tens
  # 10: strong decrease
  # 20: decrease
  # 30: increase
  # 40: strong increase
  
  # INCOME in ones
  # 1: strong decrease
  # 2: decrease
  # 3: increase
  # 4: strong increase
  
  gini_income_bins_raster <- rast(tot_mask)
  gini_income_bins_raster[tot_mask] <- comb_bins
  
  #plot(gini_income_bins_raster)
  
  #write_csv('results/')
  
  return(gini_income_bins_raster)
  
}

#  country boarders 
sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.2, keep_shapes = T) %>%
  st_as_sf() %>% 
  filter(!iso_a3 == 'ATA')

#### Raster map function ----

myFun_create_rasterMap <- function(r_in, colorpal,
                                   titleLabel, N = 16){
  
  # minValue <- quantile( values(r_in), .01, na.rm=T)
  # maxValue <- quantile( values(r_in), .99, na.rm=T) 
  # 
  pop_count = r_popCount_ext_mean
  # mask out non-populated areas from the data
  r_in[is.na(landMask)] <- NA
  
  
  # assign NA to all pop_cells where no data for s-e data
  pop_count[is.na(r_in)] <- NA
  
  
  # create mask raster from cells where population is not Na
  tot_mask <- !is.na(pop_count) # create for which any data is nan
  
  # assign NA to all hdi and aridity cells where mask is 0 (i.e. no population)
  r_in[tot_mask == 0] <- NA
  
  
  # raster to vector, drop na also
  r_in_vec <- na.omit(c(as.matrix(r_in)))
  
  pop_vec <- na.omit(c(as.matrix(pop_count)))
  
  #mybreaks <- plyr::round_any( reldist::wtd.quantile (r_in_vec, q = c(.01,seq(.1,.9,.1),.99), na.rm=T, weight=pop_vec), .01)
  mybreaks <- rep(1:4, each = 4)*10 + c(1,2,3,4) #plyr::round_any( reldist::wtd.quantile (r_in_vec, q = seq(0,1,1/N), na.rm=T, weight=pop_vec), .01)
  
  #
  # mybreaks <- seq( plyr::round_any( minValue,accuracy=0.1,f=floor ), 
  #                  plyr::round_any( maxValue,accuracy=0.1,f=ceiling ) ,
  #                  by= (maxValue-minValue)/10) 
  # 
  r_projected = terra::project(x = r_in, y = "+proj=robin", method = "near")
  
  map =  
    
    tm_shape(r_projected, raster.warp = FALSE) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks =  mybreaks,
              palette = colorpal,
              labels = as.character(mybreaks),
              showNA = F,
              colorNA = NULL,
              title = titleLabel, 
              legend.is.portrait = FALSE)+
    tm_shape(sf_adm0) + 
    tm_borders(col = "grey", lwd = 0.1) +
    tm_layout(legend.outside.position = "bottom",
              legend.outside.size = 0.15,
              legend.width = .75,
              legend.height = .15,
              legend.bg.color = F,
              legend.outside = TRUE,
              legend.position = c(0.35, 0.75),
              main.title.position = "centre",
              frame = FALSE)
  map
  
  #tmap_save(map, paste0('figures/inMap_', titleLabel, '.pdf'))
  
  return(map)
  
  
}


##### Fig 3c: Inequality and income bins ----


# what size bin matrix to create (N x N)
N = 4

# Use slope for inequality and %-change for income
r_ginislope <- rast('results/rast_slope_gini_disp_1990_2021.tif')*32 # multiply by 32 to get slope over the whole study period
r_incomeperc <- rast('results/rast_slope_adm1_perc_gnic.tif')

# define variables
xRast <-  r_ginislope
yRast <-  r_incomeperc
pop_count <-  r_popCount_ext_mean
usedMask <-  landMask
N = 4

# use the function to create bins
inequality_income_bins_raster <- create_bins(r_ginislope, r_incomeperc, r_popCount_ext_mean, landMask,  N)

# plot(gini_income_bins_raster)
# save bins
writeRaster(inequality_income_bins_raster, filename=paste0('results/inequality_income_bins_raster_',N,'_', Sys.Date(),'.tif'), overwrite = T)

# rMap_gini_gni <- myFun_create_rasterMap(r_in =inequality_income_bins_raster, 
#                                         colorpal= scico::scico(9,palette = 'lajolla', begin = 0.2, end = .8, direction = -1),
#                                         titleLabel="Change in inequality and income")
# 
# tmap_save(map, paste0('figures/inMap_', titleLabel, '.pdf'))

