

library(terra)
library(sf)

library(reldist)
library(stringr)

library(tmap)
library(boot)
library(tidyverse)
library(dplyr) 

library(tidyterra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


### 1 load data -----

r_gini_adm1 <- rast('../subnatGini/results/rast_gini_disp_1990_2021.tif')

r_gini_adm0 <- rast('../subnatGini/results/rast_adm0_gini_disp_1990_2021.tif')

v_gini_adm1 <- vect('../subnatGini/results/vect_gini_disp_1990_2021.gpkg') %>% 
  filter(!iso3 == 'ATA')
v_gini_adm0 <- vect('../subnatGini/results/vect_adm0_gini_disp_1990_2021.gpkg') %>% 
  filter(!iso3 == 'ATA')

r_urb <- rast('../subnatGini/data_gis/urbanisationCntryWise_GHS2023a.tif')

## population


r_ref_wgs84_5arcmin <- rast(ncol=360*12, nrow=180*12)

r_popCount <- rast('../subnatGini/data_gis/r_pop_GHS_1990_2022_5arcmin.tif')

# pop for year 2021
r_popCount_ext <- extend(subset(r_popCount, nlyr(r_popCount)-1 ),r_ref_wgs84_5arcmin)

r_popCount_ext_1990 <- extend(subset(r_popCount, 1 ),r_ref_wgs84_5arcmin)


### 2. populaion weighted urb ----

sf_pop <- terra::extract(r_popCount_ext,v_gini_adm1,fun='sum', na.rm=T)
sf_pop1990 <- terra::extract(r_popCount_ext_1990,v_gini_adm1,fun='sum', na.rm=T)

sf_pop_x_urb <- terra::extract(r_popCount_ext*subset(r_urb,nlyr(r_urb)),v_gini_adm1,fun='sum', na.rm=T)
sf_pop_x_urb1990 <- terra::extract(r_popCount_ext_1990*subset(r_urb,1),v_gini_adm1,fun='sum', na.rm=T)

sf_urb <- sf_pop_x_urb / sf_pop %>% 
  as_tibble() %>% 
  rename(urb = pop2021) 

sf_urb1990 <- sf_pop_x_urb1990 / sf_pop1990 %>% 
  as_tibble() %>% 
  rename(urb1990 = pop1990) 

#head(sf_urb1990)

### 3 ratio adm1 vs adm0 -----

df_ratio <- v_gini_adm1 %>% 
  as_tibble() %>% 
  select(admID, iso3, '1990','2021') %>% 
  rename(adm11990 = '1990') %>% 
  rename(adm12021 = '2021') %>% 
  bind_cols(sf_urb) %>% 
  rename(urb = pop2021) %>% 
  mutate(urb = ifelse(is.na(urb),1,urb)) %>% 
  bind_cols(sf_urb1990) %>% 
  rename(urb1990 = pop1990) %>% 
  mutate(urb1990 = ifelse(is.na(urb),1,urb)) %>% 
  # select only adm1 areas
  filter(admID > 999) %>% 
  left_join(v_gini_adm0 %>% as_data_frame() %>% select(iso3,'1990', '2021') %>%
              rename(adm01990 = '1990') %>% rename(adm02021 = '2021')) %>% 
  mutate(ratio1990 =  adm11990 / adm01990) %>% 
  mutate(ratio2021 =  adm12021 / adm02021) 
  
# 
# rsq_function <- function(formula, data, indices) {
#   d <- data[indices,] #allows boot to select sample
#   fit <- lm(formula, data=d)
#   return(summary(fit)$r.square)
# }
# 
# reps <- boot(data=mtcars, statistic=rsq_function, R=3000, formula=mpg~disp)
# reps$t0
# 
# df_ratio_boot <- df_ratio %>% 
#   group_split(iso3) %>% 
#   purrr::map_dfr(
#     function(x){
#       #wtd.avg <- weighted.mean(x$counts, x$weights)
#       basic <- boot(x, rsq_function, R = 100, formula=urb~ratio)
#      
#       data.frame(basic$t0)
#     }
#   )

df_ratio_corr <- df_ratio %>% 
  group_by(iso3) %>% 
  summarise(corr1990 = cor(urb1990, ratio1990), 
            corr2021 = cor(urb, ratio2021))

v_gini_adm0_urb_corr <- v_gini_adm0 %>% 
  tidyterra::select(admID,iso3) %>% 
  tidyterra::left_join(df_ratio_corr) %>% 
  st_as_sf()


df_ratio_corr_count <- df_ratio_corr %>% 
  summarise(positive1990 = sum(corr1990 > 0), 
            negative1990 = sum(corr1990 < 0),
            positive2021 = sum(corr2021 > 0), 
            negative2021 = sum(corr2021 < 0))

### 3.2 plot -----

source('functions/f_Plot_sfNetChng.R')

sf_adm0 <- read_sf("../subnatGini/data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')

minGini <- quantile( df_ratio_corr$corr2021, .05, na.rm=T)
maxGini <- quantile( df_ratio_corr$corr2021, .95, na.rm=T) 

giniRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
                  by= 0.1) 

palGini <-  scico::scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "broc")

p_gini_urb_corr_1990 <- f_Plot_sfNetChng(v_gini_adm0_urb_corr,'corr1990',giniRange, pal = palGini)
p_gini_urb_corr_2021 <- f_Plot_sfNetChng(v_gini_adm0_urb_corr,'corr2021',giniRange, pal = palGini)
p_gini_urb_corr_1990
p_gini_urb_corr_2021


tmap_save(p_gini_urb_corr_2021,filename = 'figures/fig_gini_ratio_urb_correlation.pdf',width = 180, height=120, units='mm')



### 5. test also slope in rural vs urban areas ----


df_ratio_slope <- v_gini_adm1 %>% 
  as_tibble() %>% 
  select(admID, iso3, slope) %>% 
  rename(slopeAdm1 = slope) %>% 
  bind_cols(sf_urb) %>% 
  rename(urb = pop2021) %>% 
  mutate(urb = ifelse(is.na(urb),1,urb)) %>% 
  # select only adm1 areas
  filter(admID > 999) %>% 
  left_join(v_gini_adm0 %>% as_data_frame() %>% select(iso3, slope) %>% rename(slopeAdm0 = slope)) %>% 
  mutate(ratio =  slopeAdm1) 

df_ratio_slope_corr <- df_ratio_slope %>% 
  group_by(iso3) %>% 
  summarise(correlation = cor(urb, ratio))

v_gini_adm0_urb_corr_slope <- v_gini_adm0 %>% 
  tidyterra::select(admID,iso3) %>% 
  tidyterra::left_join(df_ratio_slope_corr) %>% 
  st_as_sf() 


df_ratio_slope_corr_count <- df_ratio_slope_corr %>% 
  summarise(positive = sum(correlation > 0, na.rm=T), 
            negative = sum(correlation < 0, na.rm=T))


# plot

sf_adm0 <- read_sf("../subnatGini/data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')

minGini <- quantile( df_ratio_slope_corr$correlation, .05, na.rm=T)
maxGini <- quantile( df_ratio_slope_corr$correlation, .95, na.rm=T) 

giniRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
                  by= 0.1) 

palGini <-  scico::scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "broc")

p_gini_urb_corr_slope <- f_Plot_sfNetChng(v_gini_adm0_urb_corr_slope,'correlation',giniRange, pal = palGini)
p_gini_urb_corr_slope

p_gini_urb_coll <- tmap_arrange(p_gini_urb_corr_1990,
                             p_gini_urb_corr_2021, 
                             p_gini_urb_corr_slope,
                             ncol = 1)




if (dir.exists('figures/figUrb/')) {
  
} else {
  dir.create('figures/figUrb/')  
}

layers <- list(p_gini_urb_corr_1990, p_gini_urb_corr_2021, 
               p_gini_urb_corr_slope)

nameLayers <- c('p_gini_urb_corr_1990', 'p_gini_urb_corr_2021', 
                'p_gini_urb_corr_slope')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/figUrb/fig_',nameLayers[i],'.png'),width = 110, units='mm', dpi = 450)
  
}


tmap_save(p_gini_urb_coll,filename = 'figures/fig_gini_slope_urb_correlation.pdf',width = 180, height=240, units='mm')




##### 6. plot urban -----


source('functions/f_Plot_sfAbs.R')

sf_adm0 <- read_sf("../subnatGini/data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')

minUrb <- quantile( df_ratio$urb, .025, na.rm=T)
maxUrb <- quantile( df_ratio$urb, .975, na.rm=T) 

UrbRange <- seq( plyr::round_any( minUrb,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxUrb,accuracy=0.05,f=ceiling ) ,
                  by= 0.05) 

palUrb <-  scico::scico(9, begin = 0, end = 1,direction = -1, palette = "bam")

v_gini_adm0_urb <- v_gini_adm1 %>% 
  tidyterra::select(admID,iso3) %>% 
  tidyterra::left_join(df_ratio %>% select(admID, urb)) %>% 
  st_as_sf() 


p_gini_urb <- f_Plot_sfNetChng(v_gini_adm0_urb,'urb',UrbRange, pal = palUrb, midpnt =  1)
p_gini_urb

p_gini_urb_noLegend <- p_gini_urb + 
  tm_layout(legend.show=FALSE)

tmap_save(p_gini_urb_noLegend,filename = paste0('figures/figUrb/fig_','p_gini_urb_noLegend','.png'),width = 110, units='mm', dpi = 450)

tmap_save(p_gini_urb,filename = 'figures/figUrb/fig_urb_giniAdm1.pdf',width = 180, height=240, units='mm')






