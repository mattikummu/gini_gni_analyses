# This file contains the script for data preparation and plotting Figures 2 and 3a,b,d 
# (Scripts for Fig3c are in .R-files 2_GINI_GNI_bins and 3_plot_GINI_GNI_bins)
# Scripts for clustering and plotting Fig4 are in 4_fuzzy_clustering.R


rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## Open packages
library(terra)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)
library(tibble)
library(raster)
library(tmap)
library(rmapshaper)
# library(pals)
# library(e1071)
library(openxlsx)
library(rgeoda)
library(pals)
library(ggplot2)
library(biscale)



#### Read data ------
# Income (GNI per capita)
income <- rast('../hdi_subnat/results/rast_gnic_1990_2021.tif')
years <- as.character(seq(1990,2021,1,))
# Change layer names for income
names(income) <- c(rep('income', 32)) %>% str_c(., years)

# Inequality (GINI)
gini <- rast('results/rast_gini_disp_1990_2021.tif')
names(gini) <- c(rep('gini', 32)) %>% str_c(., years)

# Change in gini (slope used here)
gini_slope <- rast('results/rast_slope_gini_disp_1990_2021.tif')*32 # multiply by 32 to get total slope
# Change in GNI (percentual change used here)
income_perc <- rast('results/rast_slope_perc_gnic.tif')

# Population count
pop <- terra::rast("data_gis/r_pop_GHS_1990_2022_5arcmin.tif")
names(pop) <- paste0('pop', c(1990:2022))
pop <- pop[[-33]]
names(pop)
pop_mean <- mean(pop, na.rm =T)

# admin borders as sf for plotting
sf_gadm1 <- terra::as.polygons(rast('data_gis/gini_comb_Adm0Adm1_5arcmin.tif')) %>% 
  sf::st_as_sf() %>% # to sf
  rmapshaper::ms_simplify(.,keep=0.1,keep_shapes = T) # simplify


sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.2, keep_shapes = T) %>%
  st_as_sf() %>% 
  filter(!iso_a3 == 'ATA')


#### Aggregate income and inequality to adm1 level ----

adm1 <- rast('data_gis/gini_comb_Adm0Adm1_5arcmin.tif')

# Aggregate population as zonal sum
pop_adm <- terra::zonal(pop, adm1, fun=sum, na.rm=T) %>% 
  as.tibble()

# Aggregate income as zonal mean
income_adm <- terra::zonal(income, adm1, fun=mean, na.rm=T, w=pop_mean) %>% 
  as.tibble()

# Aggregate inequality as zonal mean
gini_adm <- terra::zonal(gini, adm1, fun=mean, na.rm=T) %>% 
  as.tibble()


#### Figure 2 ----
#### Prep Fig 2a: Categorize areas with combinations of GINI and GNI: WB and inequality grouping ----
# Combine into a matrix determining the number of people living under each category

# Based on literature
# Relative equality 0-0.3: 1 're'
# Adequate equality 0.3-0.4: 2 'ae'
# Big income gap 0.4-0.5: 3 'big'
# Severe inequality 0.5-1.0: 4 'se'

mydata_gini <- gini_adm %>% #select(nmbr, gini1990, gini2015, gini2021) %>% 
  rename(nmbr = layer) %>% 
  pivot_longer(!nmbr, names_to = 'year', values_to = 'gini') %>% 
  mutate(year = str_remove(year, 'gini')) %>% 
  group_by(nmbr) %>% 
  mutate(cat_gini = case_when(gini >= 0 & gini < 0.3 ~ 'relative',
                              gini >= 0.3 & gini < 0.4 ~ 'adequate',
                              gini >= 0.4 & gini < 0.5 ~ 'big',
                              gini >= 0.5 & gini <=1.0 ~ 'severe')) %>% 
  ungroup()


levels_gini <- rev(c('severe','big','adequate','relative'))

mydata_gini <- mydata_gini %>% mutate(cat_gini = factor(cat_gini, levels_gini))

# Check that groups ok
# tm_shape(left_join(sf_gadm1,mydata_gini) %>% filter(year==2015), projection = "+proj=robin") +
#   tm_polygons(col = "cat_gini",
#               #palette = mypal,
#               lwd = NA,
#               textNA = "no data",
#               colorNA = "#C2D7C6",
#               legend.show=F) +
#   tm_layout(frame = FALSE)

# World Bank classification

# low income: $1,135 or less in 2022 (LI; 1)
# lower middle-income economies are those with a GNI per capita between $1,136 and $4,465 (LMI, 2); 
# upper middle-income economies are those with a GNI per capita between $4,466 and $13,845 (UMI, 3); 
# high-income economies are those with a GNI per capita of $13,846 (HI, 4)

mydata_income <- income_adm %>% 
  rename(nmbr = layer) %>% 
  tidyr::pivot_longer(!nmbr, names_to = 'year', values_to = 'income') %>% 
  mutate(year = str_remove(year, 'income')) %>% 
  group_by(nmbr) %>% 
  mutate(cat_gni = case_when(income < 1006 ~ 'low',
                             income >= 1006 & income < 3955 ~ 'lowmid',
                             income >= 3955 & income < 12235 ~ 'highmid',
                             income >= 12235 ~ 'high'))%>% 
  ungroup()

levels_gni <- c('low','lowmid','highmid','high')

mydata_income <- mydata_income %>% mutate(cat_gni = factor(cat_gni, levels_gni))

# tm_shape(left_join(sf_gadm1,mydata_income) %>% filter(year==2015), projection = "+proj=robin") +
#   tm_polygons(col = "cat_gni",
#               #palette = mypal,
#               lwd = NA,
#               textNA = "no data",
#               colorNA = "#C2D7C6",
#               legend.show=F) +
#   tm_layout(frame = FALSE)

### Combine groups
# Assign ID for each observation (adm unit) based on the combination of gini and gni categories
data_groups <- mydata_income %>%
  left_join(., mydata_gini) %>% 
  mutate(category = str_c(cat_gini,'_', cat_gni)) %>% 
  # transform(., category = as.numeric(interaction(cat_gini, cat_gni))) %>%
  # mutate(category = as.character(category)) %>% 
  as_tibble() %>% 
  mutate(year = as.numeric(year))

data_groups

# remove rows where categories = NA
data_groups <- data_groups %>% filter(!is.na(category))

cat_levels <- data_groups %>% 
  expand(cat_gini, cat_gni) %>% 
  na.omit() %>% 
  mutate(category=str_c(cat_gini,'_',cat_gni)) %>% 
  pull(category)

data_groups <- data_groups %>% mutate(category = factor(category, cat_levels))

### Plot Fig 2a: GINI+GNI combinations-----

# Create palette
# First view palette
biscale::bi_pal(pal = 'BlueOr', dim = 4, rotate_pal = F, flip_axes = T)
mypal <- biscale::bi_pal(pal = 'BlueOr', dim = 4, rotate_pal = F, preview=F,flip_axes = T)

# Join geometry to data
mydata <- left_join(sf_gadm1 %>%  rename(nmbr = layer), data_groups)

mydata

# Plot categories on a map

# Plot categories on a map
map1 <- tm_shape(mydata %>% filter(year %in% c(1990, 2021)), projection = "+proj=robin") +
  tm_polygons(col = "category",
              palette = mypal,
              lwd = NA,
              textNA = "no data",
              colorNA = "#C2D7C6",
              legend.show=F) +
  tm_facets(by="year")+
  tm_shape(sf_adm0) + 
  tm_borders(col = "grey", lwd = 0.1) +
  tm_layout(frame = FALSE)

map1990 <- tm_shape(mydata %>% filter(year %in% c(1990)), projection = "+proj=robin") +
  tm_fill(col = "category",
              palette = mypal,
              lwd = NA,
              textNA = "no data",
              colorNA = "#C2D7C6",
              legend.show=F) +
  tm_facets(by="year")+
  tm_shape(sf_adm0) + 
  tm_borders(col = "grey", lwd = 0.1) +
  tm_layout(frame = FALSE)

map2021 <- tm_shape(mydata %>% filter(year %in% c(2021)), projection = "+proj=robin") +
  tm_fill(col = "category",
              palette = mypal,
              lwd = NA,
              textNA = "no data",
              colorNA = "#C2D7C6",
              legend.show=F) +
  tm_facets(by="year")+
  tm_shape(sf_adm0) + 
  tm_borders(col = "grey", lwd = 0.1) +
  tm_layout(frame = FALSE)


# Make legend
bivariate_color_scale <- data_groups %>% 
  filter(year == 2021) %>% 
  expand(cat_gini, cat_gni) %>% 
  #  na.omit() %>% 
  mutate(fill = mypal) 

# Plot
legend <- ggplot() +
  geom_tile(data = bivariate_color_scale,
            mapping = aes(x = cat_gini,
                          y = cat_gni,
                          fill = fill
            )) +
  scale_fill_identity() +
  labs(x = "Inequality group",
       y = "Income group") +
  cowplot::theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(hjust = 1, size = 6)) +
  # quadratic tiles
  coord_fixed()

# Plot as grid
# Convert tmap to grob


gini_gni_map <- cowplot::ggdraw() +
  cowplot::draw_plot(map1, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, 0, 0.0020, 0.25, 0.25)
gini_gni_map

tmap_save(map1990, 'figures/fig2a_map1990.png', units = 'mm', width = 110, height = 55, dpi = 450)
tmap_save(map2021, 'figures/fig2a_map2021.png', units = 'mm', width = 110, height = 55, dpi = 450)

# Save
cowplot::save_plot(str_c('figures/fig2a_',Sys.Date(),'.pdf'), gini_gni_map)

#### Prep Fig 2b and 2c: Number of people and pop change in each category in 1990 and 2021  ----

# Population data only for 1990-2020
# Population in each adm unit
my_pop <- pop_adm %>% 
  rename(nmbr = layer) %>% 
  pivot_longer(!nmbr, names_to = 'year', values_to = 'pop') %>% 
  mutate(year = str_remove(year, 'pop')) %>% 
  mutate(year = as.numeric(year)) 

# Extract global pop
globalPop <- my_pop %>% filter(year %in% c(1990,2021)) %>% 
  group_by(year) %>% summarise(popSum = sum(pop, na.rm = T)) %>% 
  pivot_wider(names_from = year, names_prefix = 'pop', values_from = popSum)

# Calculate population count in each of the 16 groups
pop_groups <- data_groups %>% filter(year %in% c(1990,2021)) %>% 
  left_join(., my_pop) %>% 
  group_by(cat_gni,cat_gini, year, category) %>% 
  # Population count in each category
  summarise(popSum = sum(pop, na.rm =T)) %>%
  pivot_wider(names_from = year, values_from = popSum, names_prefix = 'pop') %>% 
  # Calculate relative population (as percentage of global pop)
  mutate(popRel1990 = pop1990/globalPop$pop1990,
         popRel2021 = pop2021/globalPop$pop2021,
         # Change in percentage units
         change = popRel2021 - popRel1990) %>%
  # Round
  mutate(popRel1990 = signif(popRel1990*100,2),
         popRel2021 = signif(popRel2021*100,2),
         change = signif(change*100,2))

pop_groups

# Population in GNI categories
gni_pop <- pop_groups %>% group_by(cat_gni) %>% 
  summarise(pop1990 = sum(pop1990, na.rm=T), pop2021 = sum(pop2021, na.rm=T)) %>% 
  mutate(relPop1990 = pop1990 / sum(pop1990, na.rm=T),
         relPop2021 = pop2021 / sum(pop2021, na.rm=T)) %>% 
  mutate(pop1990 = round(pop1990/1000000),
         pop2021 = round(pop2021/1000000),
         relPop1990 = round(relPop1990*100),
         relPop2021 = round(relPop2021*100)) %>% 
  dplyr::select(cat_gni, pop1990, relPop1990, pop2021,relPop2021)

# Population in GINI categories
gini_pop <- pop_groups %>% group_by(cat_gini) %>% 
  summarise(pop1990 = sum(pop1990, na.rm=T), pop2021 = sum(pop2021, na.rm=T)) %>% 
  mutate(relPop1990 = pop1990 / sum(pop1990, na.rm=T),
         relPop2021 = pop2021 / sum(pop2021, na.rm=T)) %>% 
  mutate(pop1990 = round(pop1990/1000000),
         pop2021 = round(pop2021/1000000),
         relPop1990 = round(relPop1990*100),
         relPop2021 = round(relPop2021*100)) %>% 
  dplyr::select(cat_gini, pop1990, relPop1990, pop2021,relPop2021)

write.xlsx(gini_pop, paste0('results/gini_pop_',Sys.Date(),'.xlsx'))
write.xlsx(gni_pop, paste0('results/gni_pop_',Sys.Date(),'.xlsx'))

# Save to results
openxlsx::write.xlsx(pop_groups, paste0('results/fig2_gni_gini_groups_pop_',Sys.Date(),'.xlsx'))


### Plot Fig 2b: population in 1990 and 2021 ----
# Define levels
pop_groups <- pop_groups %>% mutate(cat_gini = factor(cat_gini, levels = rev(c('severe','big','adequate','relative'))),
                                    cat_gni = factor(cat_gni, levels = c('low','lowmid','highmid','high')))

plot_pop_rel <- pop_groups %>% 
  dplyr::select(cat_gini, cat_gni, popRel1990, popRel2021) %>% 
  pivot_longer(!c(cat_gini, cat_gni),names_to = 'year', values_to = 'popRel') %>% 
  mutate(year = str_remove(year, 'popRel')) %>% 
  ggplot(., aes(x=cat_gini, y=cat_gni)) +                           
  geom_tile(aes(fill = popRel)) +
  scico::scale_fill_scico(palette = 'davos', direction = -1,
                          limits = c(0,30), breaks = c(0,10,20,30))+
  #scale_fill_distiller(palette = "RdPu",direction = 1, limits = c(0,30), breaks = c(0,10,20,30)) +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.75, "lines"),
    legend.key.height = unit(10, "lines"))))+
  #scale_fill_viridis_c() + #option='magma'
  labs(fill = "Relative population (% of global pop)")+
  facet_wrap('year')+
  xlab('Inequality (GINI)')+
  ylab('Income (GNI)')+
  theme_minimal()+
  coord_fixed()

plot_pop_rel

# save
ggsave(paste0('figures/fig2b_relative_pop_gni_gini_groups_',Sys.Date(),'.pdf'), plot_pop_rel)

### Plot Fig 2c: Change in population in each group ----
plot_pop_change <- pop_groups %>% #filter(year%in% c(2000,2020)) %>% mutate(popSum = popSum/1000000) %>%  #mutate(popSum = log10(popSum)) %>% 
  dplyr::select(cat_gini, cat_gni, change) %>% 
  ggplot(., aes(x=cat_gini, y=cat_gni)) +                           
  geom_tile(aes(fill = change)) +
  scale_fill_gradient2(mid='#F5F5F5', low="#890620", high = "#002642", midpoint=0,  
                       limits = c(-11,25), breaks = c(-10,-5,0, 5, 10, 15, 20)) + #palette = "RdPu",direction = 1,
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.75, "lines"),
    legend.key.height = unit(10, "lines"))))+
  #scale_fill_viridis_c() + #option='magma'
  labs(fill = "Change in relative population (in %-units)")+
  #facet_wrap('year')+
  xlab('Inequality (GINI)')+
  ylab('Income (GNI)')+
  theme_minimal()+
  coord_fixed()

plot_pop_change

ggsave(paste0('figures/Fig2c_change_relative_pop_gni_gini_groups_',Sys.Date(),'.pdf'), plot_pop_change)


#### Fig 3: Increasing and decreasing equality and income -----
#### Prep Fig 3a: CHANGE IN INEQUALITY ----
# GINI
# Change in inequality is STRONG if decrease/increase (i.e. slope) </> 0.05 -> categorise STRONG INCREASE 2, STRONG DECREASE -2
# Change in inequality is MODERATE if decrease/increase is smaller than this -> INCREASE 1, DECREASE -1


# define thresholds


## read thresholds

thresholds <- round(read.csv('results/gini_gnic_slope_thresholds.csv'),2)



# Classify according to threshold
gini_change_classified <- ifel((gini_slope <= thresholds[2,1]), -2, 
                               ifel((gini_slope > thresholds[2,1] & gini_slope < 0), -1, 
                                    ifel((gini_slope > 0 & gini_slope < thresholds[4,1]), 1, 
                                         ifel((gini_slope >= thresholds[4,1]), 2, 
                                              0))))
plot(gini_change_classified)


### Plot Fig 3a ----

mypal <- c("#196a99","#75aac4","#d39a78","#b95d2c") #,"#EAEAEA"=0

# Project data
mydata = terra::project(x = gini_change_classified, y = "+proj=robin", method = "near") ##left_join(sf_gadm1, gini_change)

gini_change_map <- tm_shape(mydata, raster.warp = FALSE) +
  tm_raster(style = "cat", # draw gradient instead of classified
            #breaks =  mybreaks,
            palette = mypal,
            labels = as.character(c('Strong decrease', 'Decrease','No change','Increase','Strong Increase')),
            showNA = F,
            colorNA = NULL,
            title = "Change in inequality over 1990-2021", 
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
gini_change_map

tmap::tmap_save(gini_change_map, str_c('figures/Fig3a_gini_change_',Sys.Date(),'.pdf'))

p_fig = gini_change_map +
  tm_layout(legend.show=FALSE)

tmap_save(p_fig,filename = paste0('figures/fig_','gini_trend_classes','.png'),width = 110, units='mm', dpi = 450)

#### Prep Fig 3b: CHANGE IN INCOME ----
# Change in income STRONG if decrease/increase </> 0.15 -> strong increase 2, strong decrease -2
# Change in income MODERATE if decrease/increase smaller than this -> increase 1, decrease -1 

income_change_classified <- ifel((income_perc <= thresholds[7,1]), -2, 
                                 ifel((income_perc > thresholds[7,1] & income_perc < 0), -1, 
                                      ifel((income_perc == 0), 0, 
                                           ifel((income_perc > 0 & income_perc < thresholds[9,1]), 1, 
                                                ifel((income_perc >= thresholds[9,1]), 2, 
                                                     0)))))
plot(income_change_classified)

### Plot Fig 3b ----
mydata = terra::project(x = income_change_classified, y = "+proj=robin", method = "near") ##left_join(sf_gadm1, gini_change)
#mypal <- rev(c("#196a99","#75aac4","#EEEEEE","#d39a78","#b95d2c"))
mypal <- rev(c("#196a99","#75aac4","#d39a78","#b95d2c"))

gni_change_map <- tm_shape(mydata, raster.warp = FALSE) +
  tm_raster(style = "cat", # draw gradient instead of classified
            #breaks =  mybreaks,
            palette = mypal,
            labels = as.character(c('Strong decrease', 'Decrease','Increase','Strong Increase')),
            showNA = F,
            colorNA = NULL,
            title = "Change in income over 1990-2021", 
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
gni_change_map

tmap::tmap_save(gni_change_map, str_c('figures/Fig3b_gni_change',Sys.Date(),'.pdf'))

p_fig = gni_change_map +
  tm_layout(legend.show=FALSE)

tmap_save(p_fig,filename = paste0('figures/fig_','gni_trend_classes','.png'),width = 80, units='mm', dpi = 450)


# Fig 3c in scripts 2_GINI_GNI_bins.R and 3_plot_GINI_GNI_bins.R
#### Prep Fig 3d: share of global pop living where inequality changed ----
# Population where inequality changed

pop_gini <- terra::zonal(pop, gini_change_classified, fun = sum, na.rm=T) %>% 
  as_tibble() %>% 
  rename(layer = gini_disp_slope_1990_2021) %>% 
  dplyr::select(layer, pop1990, pop2021)

# Calculate relative population (of global pop)
pop_gini <- pop_gini %>% 
  mutate(relPop1990 = pop1990/sum(pop_gini$pop1990), #in 1990
         relPop2021 = pop2021/sum(pop_gini$pop2021)) %>% #in 2021
  # Add categories
  mutate(category = c('Strong decrease', 'Decrease','No change','Increase','Strong Increase')) %>% 
  dplyr::select(category, pop1990, relPop1990, pop2021, relPop2021) %>% 
  mutate(pop1990 = round(pop1990/1000000),
         pop2021 = round(pop2021/1000000)) %>% 
  mutate(relPop1990 = round(relPop1990,2)*100,
         relPop2021 = round(relPop2021,2)*100) %>% 
  rename('Population in 1990 (M)' = 'pop1990') %>% 
  rename('Population in 2021 (M)' = 'pop2021')

write.xlsx(pop_gini, paste0('results/Fig3d_pop_gini_change_', Sys.Date(),'.xlsx'))

#### Prep Fig 3d: share of global pop living where income changed ----
pop_gni <- terra::zonal(pop, income_change_classified, fun = sum, na.rm=T) %>% 
  as_tibble() %>% 
  dplyr::select(gnic_slope_1990_2021, pop1990, pop2021)

# Calculate relative population (of global pop)
pop_gni <- pop_gni %>% mutate(relPop1990 = pop1990/sum(pop_gni$pop1990), #in 2000
                              relPop2021 = pop2021/sum(pop_gni$pop2021)) %>% #in 2020
  # Calculate the change in population
  mutate(changePop = (pop2021-pop1990) / pop1990) %>% 
  # Add categories
  mutate(category = c('Strong decrease', 'Decrease','Increase','Strong Increase')) %>% 
  dplyr::select(category, pop1990, relPop1990, pop2021, relPop2021) %>% 
  mutate(pop1990 = round(pop1990/1000000),
         pop2021 = round(pop2021/1000000)) %>% 
  mutate(relPop1990 = round(relPop1990,2)*100,
         relPop2021 = round(relPop2021,2)*100) %>% 
  rename('Population in 1990 (M)' = 'pop1990') %>% 
  rename('Population in 2021 (M)' = 'pop2021')
pop_gni

write.xlsx(pop_gni, paste0('results/Fig3d_pop_gni_change_', Sys.Date(),'.xlsx'))


#### Prep Fig 3d: People living in inequality-income -bins ----

# Combination bins defined in another script (inequality-income bins)
gini_gni_bins <- terra::rast("results/inequality_income_bins_raster_4_2024-08-08.tif")
names(gini_gni_bins) <- 'change'
plot(gini_gni_bins)

# Calculate zonal stats for each group

pop_bins <- left_join(terra::zonal(pop$pop1990, gini_gni_bins, fun= sum, na.rm=T) %>% as_tibble() %>% rename('popSum1990' = 'pop1990'),
                      terra::zonal(pop$pop2021, gini_gni_bins, fun= sum, na.rm=T) %>% as_tibble() %>% rename('popSum2021' = 'pop2021'),
                      by = 'change') %>%
  left_join(., terra::zonal(pop$pop1990, gini_gni_bins, fun= mean, na.rm=T) %>% as_tibble() %>% rename('popMean1990' = 'pop1990'),
            by = 'change') %>% 
  left_join(., terra::zonal(pop$pop2021, gini_gni_bins, fun= mean, na.rm=T) %>% as_tibble() %>% rename('popMean2021' = 'pop2021'),
            by = 'change')

# Add categories 
# Gini 10: strong decrease, 20: decrease, 30: increase, 40: strong increase
# Income 1: strong decrease, 2: decrease, 3: increase, 4: strong increase
bins <- tibble(unique(gini_gni_bins)) %>% 
  arrange(change) %>% 
  mutate(change_gini = c(rep('strong decrease', 4), rep('decrease',4), rep('increase',4), rep('strong increase', 4))) %>% 
  mutate(change_gni = rep(c("strong decrease" ,"decrease", "increase",'strong increase'), 4)) %>% 
  mutate(category = str_c(change_gini,'_', change_gni)) 

# Calculate relative change in population in each category
pop_temp <- left_join(bins, pop_bins) 
pop1990 <- sum(pop_temp$popSum1990, na.rm=T)
pop2021 <- sum(pop_temp$popSum2021, na.rm=T)

pop_temp <- pop_temp %>% group_by(category) %>% 
  mutate(popChange = (popSum2021-popSum1990)/popSum1990) %>% 
  mutate(relPop1990 = popSum1990 / pop1990,
         relPop2021 = popSum2021 / pop2021) %>% 
  mutate(change_gini = factor(change_gini, c('strong decrease', 'decrease','increase','strong increase')),
         change_gni = factor(change_gni, c('strong decrease', 'decrease','increase','strong increase')))

pop_temp %>% group_by(change_gni) %>% summarise(sum2021 = sum(relPop2021, na.rm=T))

### Plot Fig 3d: Share of global population in each inequality-income bin -----
plot3 <- pop_temp %>% #mutate(change_pop_perc = if_else(change_pop_perc > 1, 1, change_pop_perc)) %>% 
  ggplot(., aes(change_gini, change_gni)) +                           
  geom_tile(aes(fill = relPop2021*100)) +
  scico::scale_fill_scico(palette = 'davos', direction = -1)+
  #scale_fill_distiller(palette = "RdPu",direction = 1) +
  #scale_fill_viridis_c() + #option='magma'
  labs(fill = "Relative population in 2021 (% of global pop)")+
  #facet_wrap('year')+
  xlab('Inequality (GINI)')+
  ylab('Income (GNI)')+
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(0.75, "lines"),
    legend.key.height = unit(10, "lines")))) +
  cowplot::theme_map() +
  theme(title = element_text(size = 9),
        legend.text = element_text(size=9),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9,hjust = 1)) +
  #theme_minimal() +
  coord_fixed()
plot3

ggsave(paste0('figures/Fig3d_relpop2021_gini_gni_change_', Sys.Date(),'.pdf'), plot3)



