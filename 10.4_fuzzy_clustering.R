rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



## Open packages
library(terra)

library(stringr)
library(tidyr)
library(sf)
library(tibble)
library(tmap)
library(rmapshaper)
library(pals)
library(e1071)
library(openxlsx)
library(factoextra)
library(clusterSim)
library(ggplot2)
library(viridis)

library(dplyr)
library(tidyverse)

#### Prepare data ####
# Use:
# 1) Slope of gini and gni
# 2) Long term average of gini and gni/ Latest year
# All four variables are aggregated to admin 1 level (provincial)
# by taking a pop weighted zonal mean. Gini and gni are also averaged temporally.
# For clustering, data is standardised so that mean = 0 and sd=1. 

# Load data
# Trends:
# Inequality (slope)
gini_slope <- rast('results/rast_slope_gini_disp_1990_2021.tif')*32 # multiply by 32 to get total slope
# Income (percentual change)
income_perc <- rast('results/rast_slope_perc_gnic.tif')

# Income (GNI per capita)
income <- rast('../hdi_subnat/results/rast_gnic_1990_2021.tif') %>% 
  `names<-`(c(seq(1990,2021,1))) #%>% terra::subset(as.character(seq(2000,2020,1)))
#names(income) <- paste0('income_', names(income))
# Inequality (GINI)
gini <- rast('results/rast_gini_disp_1990_2021.tif') %>% 
  `names<-`(c(seq(1990,2021,1))) #%>% terra::subset(as.character(seq(2000,2020,1)))
#names(gini) <- paste0('gini_', names(gini))

# Population count
pop <- terra::rast("data_gis/r_pop_GHS_1990_2022_5arcmin.tif")
names(pop) <- paste0('pop', c(1990:2022))
pop <- pop[[-33]]
names(pop)
pop_mean <- mean(pop, na.rm =T)



# Admin rasters


adm1 <- rast('data_gis/gini_comb_Adm0Adm1_5arcmin.tif') 

adm1[adm1 == 10] = NA # drop antarctica
# plot(adm1)
# Aggregate population as zonal sum
pop_adm <- terra::zonal(pop, adm1, fun=sum, na.rm=T) %>% 
  as.tibble()

# Aggregate income as zonal mean
income_adm <- terra::zonal(income, adm1, fun=mean, na.rm=T, w=pop_mean) %>% 
  as.tibble()

# Aggregate inequality as zonal mean
gini_adm <- terra::zonal(gini, adm1, fun=mean, na.rm=T) %>% 
  as.tibble()



# 
# adm1 <- rast('data/gadm_lev1_5arcmin.tif')
# names(adm1) <- 'nmbr'
# pop <- terra::rast("data/GlobPOP_5arcmin_1990_2022.tif")
# names(pop) <- paste0('pop_', c(1990:2022))
# pop <- pop[[-33]]
# pop_mean <- mean(pop, na.rm =T)
# 
# # Aggregate to adm1 level
# 
# gini_adm <- terra::zonal(gini, adm1, fun=mean, na.rm = T, w=pop_mean) %>% as.tibble() %>% 
#   pivot_longer(!nmbr, values_to = 'gini', names_to = 'year') %>% 
#   group_by(nmbr) %>% 
#   # take long term mean
#   #summarise(mean_gini = mean(gini, na.rm=T))
#   # select latest year
#   filter(year == 2021) %>% 
#   dplyr::select(-year)
# 
# 
gini_slope_adm <- zonal(gini_slope, adm1, fun=mean, na.rm = T) %>%
  as.tibble() %>%
  rename('slope_gini'='layer')
# 
# income_adm <- terra::zonal(income, adm1, fun=mean, na.rm = T, w=pop_mean) %>% as.tibble() %>% 
#   pivot_longer(!nmbr, values_to = 'gni', names_to = 'year') %>% 
#   group_by(nmbr) %>% 
#   # take long term mean
#   #summarise(mean_gni = mean(gni, na.rm=T))
#   # subset latest timestep
#   filter(year == 2021) %>% 
#   dplyr::select(-year)
# 
income_slope_adm <- terra::zonal(income_perc, adm1, fun=mean, na.rm=T, w=pop_mean) %>%
  as.tibble() %>%
  rename('slope_gni'='gnic_slope_1990_2021')

# Country borders for plotting
# admin borders as sf for plotting
sf_gadm1 <- terra::as.polygons(rast('data_gis/gini_comb_Adm0Adm1_5arcmin.tif')) %>% 
  sf::st_as_sf() %>% # to sf
  rmapshaper::ms_simplify(.,keep=0.25,keep_shapes = T) %>% # simplify
  filter(! layer == 10) # drop antarctica


sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.25, keep_shapes = T) %>%
  st_as_sf() %>% 
  filter(!iso_a3 == 'ATA')

## combine into one df
data <- gini_adm %>%
  rename(gini = '2021') %>% 
  select(layer, gini) %>% 
  left_join(gini_slope_adm %>% rename(layer = slope_gini)) %>% 
  left_join(income_adm %>% select(layer, '2021') %>% rename(gnic = '2021')) %>% 
  left_join(income_slope_adm) %>% 
  rename(nmbr = layer)
readr::write_csv(data, 'results/in_data_clusters.csv')

# standardize values so that mean = 0 and sd=1 
data_stand <- data %>%
  dplyr::mutate(across(-nmbr, ~ scale(.x))) 

#remove any NAs, NaNs or Infinite values
data_stand <- data_stand %>% na.omit() %>% filter_all(all_vars(!is.infinite(.)))
#readr::write_csv(data, 'data/in_standard_data_clusters.csv')

data_mat <- data_stand %>%
  dplyr::select(-nmbr) %>%
  as.matrix()

#### Test optimal cluster number #### 
set.seed(123)
factoextra::fviz_nbclust(data_mat, kmeans, method='wss')
factoextra::fviz_nbclust(data_mat, kmeans, method='silhouette')
factoextra::fviz_nbclust(data_mat, kmeans, nstart=25, method='gap_stat', nboot=50)

# Clustering Comparison for Determining Optimal Clusters
# "Cluster Separation Score or Gap Index is used calculate the optimal number of clusters in our dataset. 
# Higher gap index suggests better defined clusters.
# It measures the gap   between the observed clustering quality and the expected clustering quality" (https://www.geeksforgeeks.org/fuzzy-clustering-in-r/)
# Computing PAM clustering with k = 5
cl1 <- cluster::pam(data_mat, 6) 
# Computing PAM clustering with k = 6
cl2 <- cluster::pam(data_mat, 7)  
# Combine the clustering results
cl_all <- cbind(cl1$clustering, cl2$clustering)

# Calculate the Gap index for the dataset
gap <- clusterSim::index.Gap(data_mat, cl_all, reference.distribution = "unif", 
                             B = 10, method = "pam") #pam=partition around medoids

print(gap)
#gap index(gap) : represents distinct and quality clusters. 
# High gap index suggests better defined clusters.
# difference values(diffu) : represents the uncertainty in the estimated Gap statistic. 
# A negative value here indicates that the clustering solution has a lower standard deviation which shows 
# that the clusters are distinct in comparison to random distribution.


#### Clustering ----
# Initialize

ncenters = 7
minconf = 0.2
set.seed(42)

fcm <- cmeans(data_mat, centers = ncenters, iter.max = 100)

fcm_maxmemb <- apply(fcm$membership, MARGIN = 1, FUN = max)

data_stand$cluster <- fcm$cluster
data_stand$cluster_conf <- fcm_maxmemb

data_stand <- data_stand %>%
  mutate(cluster = replace(.$cluster, .$cluster_conf < minconf, -1)) #-1 when confidence level smaller than 0.2

ret <- list(data, fcm$centers, fcm$withinerror) %>%
  setNames(c("data", "fcm_centers", "withinerror"))

#number of units in each cluster
count(data_stand, cluster, name="n_units")


# save cluster data
openxlsx::write.xlsx(data_stand, 'results/data_cluster.xlsx', overwrite = T)

#### Plots ---
#### Supplementary Figure: Boxplots to show distribution of clustering variables ####

pal <- rev(c("#74a0bd", "#81CBC1", "#8564ad", "#d2d289", "#c3a37a", "#b8828f", "#FED4E7")) #%>% as_tibble() %>% 
# rename('hex'='value') %>% 
# mutate(cluster = c(-1,1:7))
xord <- rev(c(1,2,3,4, 5,6,7))

# # Order palette
# pal <- pal %>% mutate(cluster = factor(cluster, levels = c(-1,xord)))

library(forcats)
library(ggplot2)

# Mean gni 'outliers' ids 3368 (washington dc) and 2742 (singapore); consider removing for plotting?

plt <- data %>%
  left_join(dplyr::select(data_stand, nmbr, cluster)) %>% 
  filter(!nmbr %in% c(3368,2742)) %>% 
  filter(cluster != -1) %>% 
  #select(-cluster_conf) %>% 
  pivot_longer(!c(nmbr, cluster)) %>% 
  mutate(cluster = as.factor(cluster)) %>%
  rename('variable' = 'name') %>% 
  mutate(cluster = fct_relevel(cluster, as.character(xord))) %>%
  #filter(season == s & vtype == v) %>%
  ggplot() +
  geom_boxplot(aes(x = cluster, y = value, color = cluster),
               fill = "grey95", outlier.shape = NA) +
  scale_color_manual(breaks = as.factor(xord), values = pal) + #pal$hex[-1]
  facet_wrap(~ variable, scales="free")

# plt <- plt +
#   scale_y_continuous(limits = c(-1,1), sec.axis = sec_axis(~ (.x)))

# for trickery with boxplot alignment
#lbs <- c("0.00", "0.25", "0.50", "0.75", "1.00")
# plt <- plt +
#   scale_y_continuous(limits = c(-0.04,0.04),
#                      labels = lbs,
#                      sec.axis = sec_axis(~ (.x), labels = lbs))
# }

plt <- plt +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave(paste0('figures/FigS1_cluster_boxplots_', ncenters, '_', minconf,'.pdf'), plt, units = 'mm', 
       width = 150, height = 150)



#### Fig 4a: Clusters ----
#### Plot Fig 4a: Clusters on map  ----
# FIrst put clusters in raster format
class_matrix <- data_stand %>% dplyr::select(nmbr, cluster) %>% as.matrix()


r_clusters <- terra::classify(adm1,
                              class_matrix) #variable to plot

r_clusters[r_clusters > 7] = NA
plot(r_clusters)
# #project to another crs
tocrs = "+proj=robin +over"
r_clusters_rob <- project(r_clusters,method = 'near', tocrs, mask = TRUE)

#pal <- pal %>% add_row(hex = "grey95", cluster = factor(-1)) %>% mutate(cluster = factor(cluster, levels = c(-1, xord)))

# create tmap object
p_cluster_map <- tm_shape(r_clusters_rob, raster.warp = F) + #
  tm_raster(style = "fixed", # draw gradient instead of classified
            breaks =  seq(-0.5,7.5,1),
            #palette = c('grey95',"grey50", "red", "blue", "yellow", "green", "purple", "orange"),
            palette = c('grey95',"#74a0bd", "#81CBC1", "#8564ad", "#d2d289", "#c3a37a", "#b8828f","#FED4E7"),
            #palette = pal$hex,
            showNA = F,
            colorNA = 'white', 
            title = 'Clusters',
            labels = as.character(c(-1,1,2,3,4,5,6,7)),
            drop.levels	=T)+
  #tm_facets(free.scales = F) + 
  tm_shape(sf_adm0) + 
  tm_borders(col='grey',
             lwd = 0.1) +
  tm_layout(main.title.position = "left",
            legend.bg.color = TRUE,
            legend.outside = TRUE,
            frame = FALSE,
            legend.show = T)

tmap_save(p_cluster_map, paste0('figures/Fig4a_clusters_countries_', ncenters, '_', minconf, '.pdf'),  width = 200, height = 140, units = "mm")

p_fig <- p_cluster_map + 
  tm_layout(legend.show=FALSE)

tmap_save(p_fig,filename = paste0('figures/fig_','cluster_map','.png'),width = 110, units='mm', dpi = 450)


# save as grid
map_grob <- tmap_grob(cluster_map)
pdf(paste0("plots/cmeans_clusters_",ncenters, '_', minconf,'_', Sys.Date(),".pdf"), height=10, width=20)
cowplot::plot_grid(map_grob, plt)
dev.off()

#### Prep legend for Fig 3a ----

in_data <- readr::read_csv('results/in_data_clusters.csv') %>% as_tibble()
clusters_data <- openxlsx::read.xlsx('results/data_cluster.xlsx') %>% as_tibble() %>% 
  dplyr::select(nmbr, cluster, cluster_conf) %>% 
  filter(!cluster==-1) %>% 
  dplyr::select(-cluster_conf)


mydata <- left_join(clusters_data, in_data)

# hist(mydata %>% filter(cluster == 3) %>% pull(gni))
# hist(mydata %>% filter(cluster == 6) %>% pull(gni))
# hist(mydata %>% filter(cluster == 1) %>% pull(gni))

# Check cluster medians
cluster_medians_means <- mydata %>%
  #left_join(dplyr::select(data_stand, nmbr, cluster)) %>% 
  filter(!nmbr %in% c(3368,2742)) %>% 
  filter(cluster != -1) %>% 
  #select(-cluster_conf) %>% 
  pivot_longer(!c(nmbr, cluster)) %>% 
  #mutate(cluster = as.factor(cluster)) %>%
  rename('variable' = 'name') %>% group_by(cluster, variable) %>% 
  summarise(median = median(value, na.rm = T),
            mean = mean(value, na.rm =T)) %>% 
  ungroup() #%>% 
#pivot_wider(id_cols = cluster, names_from = variable, values_from = median) %>%
#mutate(cluster = c('B','F','C','D','G','A','E')) %>% 
#arrange(gini)

# Cluster numbers to letters
# c(6,1,3,4,7,2,5) <- c('A','B','C','D','E','F','G')


readr::write_csv(cluster_medians_means, paste0('results/cluster_medians_', ncenters, '_', minconf, '.csv'))

#### Plot Fig 4a legend ----
clusters_data <- openxlsx::read.xlsx('results/data_cluster.xlsx') %>% as_tibble()
cluster_medians <- readr::read_csv('results/cluster_medians_7_0.2.csv') %>% 
  dplyr::select(cluster, variable, median) %>% 
  rename(parameter = variable, value = median)
#arrange(desc(median_gini)) %>% pivot_longer(!cluster, names_to = 'parameter', values_to = 'value')

mydata <- cluster_medians 

library(scales)

p_cluster_median_gini <- ggplot2::ggplot(mydata %>% filter(parameter == 'gini'), aes(x=parameter, y=cluster)) +
  geom_point(aes(fill=value), size=7, shape=22)+
  
  scale_fill_gradientn(colors = viridis::viridis(10,option="E" ,direction=-1), space ="Lab" , na.value = "lightyellow2",
                       limits = c(0.3,0.5), oob=squish) +
  ggthemes::theme_tufte()+
  # theme(axis.title=element_blank(),
  #       axis.ticks.x=element_blank(),
  #       legend.position = "none",
  #       text = element_text(size=13))+
  scale_x_discrete(position = "top") +
  scale_size_identity()

p_cluster_median_gni <- ggplot2::ggplot(mydata %>% filter(parameter == 'gnic'), aes(x=parameter, y=cluster)) +
  geom_point(aes(fill=value), size=7, shape=22)+
  
  scale_fill_gradientn(colors = viridis::viridis(10,option="E" ,direction=-1), space ="Lab" , na.value = "lightyellow2",
                       limits = c(3700,30000), breaks=c(8000, 20000, 30000),
                       oob=squish) +
  ggthemes::theme_tufte()+
  theme(axis.title=element_blank(),
        axis.ticks.x=element_blank(),
        #legend.position = "none",
        text = element_text(size=13))+
  scale_x_discrete(position = "top") +
  scale_size_identity()

# ggplot(t_cluster_centres_res, aes(parameter, cluster_id, label = round(value,digits=2), colour = value)) +
# geom_text()+
# scale_colour_scico(begin=0.2, end = 0.8, direction = -1, palette ="berlin")+

mydata_round <- cluster_medians %>% filter(parameter %in% c('gini_disp_slope_1990_2021', 'slope_gni')) %>% 
  mutate(value = round(value, 2)) %>% 
  mutate(bool = case_when(value < 0 ~ -1, 
                          value > 0 ~ 1, 
                          TRUE ~ 0)) %>% 
  mutate(bool = factor(bool))


p_cluster_change_gini <- ggplot(mydata_round %>% filter(parameter == 'gini_disp_slope_1990_2021'), 
                                aes(x=parameter, y=cluster, group=bool)) +
  geom_point(aes(shape=bool, fill=value), size=4.5)+
  scale_shape_manual(values=c(25, 24))+
  scale_fill_gradient2(low = muted("blue"), mid = "white",
                       high = muted("red"),limits=c(-0.07,0.04),breaks=c(-0.06, -0.04, -0.02, 0, 0.02, 0.04), oob=squish)+
  ggthemes::theme_tufte()+
  theme(axis.title=element_blank(),
        axis.ticks.x=element_blank(),
        #legend.position = "none",
        text = element_text(size=13))+
  scale_x_discrete(position = "top") +
  scale_size_identity()

#ggsave('figures/clusters_gini_change.pdf', p_cluster_change_gini)

p_cluster_change_gni <- ggplot(mydata_round %>% filter(parameter == 'slope_gni'), 
                               aes(x=parameter, y=cluster, group=bool)) +
  geom_point(aes(shape=bool, fill=value), size=4.5)+
  scale_shape_manual(values=c(24))+
  scale_fill_gradient(low = "white", high = muted("blue"),limits=c(0,0.2),oob=squish)+
  # scale_fill_gradient2(low = muted("red"), mid = "white",
  #                      high = muted("blue"),limits=c(0,0.2),oob=squish)+
  ggthemes::theme_tufte()+
  theme(axis.title=element_blank(),
        axis.ticks.x=element_blank(),
        #legend.position = "none",
        text = element_text(size=13))+
  scale_x_discrete(position = "top") +
  scale_size_identity()

p_legend <- gridExtra::grid.arrange(p_cluster_median_gini, p_cluster_median_gni,
                                    p_cluster_change_gini, p_cluster_change_gni,
                                    ncol= 2)

ggsave(paste0('figures/Fig4a_cluster_legend_',Sys.Date(),'.pdf'), p_legend)

#### Fig 4b: Heterogeneity of countries ----

#### Prep Fig 4b:# How many clusters in each country

# First read in data
data_clusters <- openxlsx::read.xlsx('results/data_cluster.xlsx') %>% as_tibble() %>% 
  dplyr::select(nmbr, cluster, cluster_conf) 

# Read in country data
# prov_id <- st_read('data/gadm36_levels.gpkg', layer = 'level1') %>% 
#   as_tibble() %>% 
#   #rmapshaper::ms_simplify(.,keep=0.1,keep_shapes = T) %>% 
#   #dplyr::select(-geom) %>% 
#   tibble::rowid_to_column(., var='nmbr') %>% 
#   dplyr::select(nmbr, GID_0, NAME_0)

# countries_id <- st_read('data/gadm36_levels.gpkg', layer = 'level0') %>%
#   as_tibble() %>% 
#   #rmapshaper::ms_simplify(.,keep=0.1,keep_shapes = T) %>% 
#   dplyr::select(GID_0, NAME_0) %>%
#   distinct()
#tibble::rowid_to_column(., var="cntry_code")

prov_id <-  sf_gadm1 %>% 
  rename(nmbr = layer)

prov_countries <- terra::extract(r_clusters, vect(prov_id), fun = 'modal', na.rm=T) %>% 
  as_tibble() %>% 
  bind_cols(prov_id) 

adm1_adm0 <- terra::extract(rast('data_gis/gini_Adm0_5arcmin.tif'), vect(prov_id), fun = 'modal', na.rm=T) %>% 
  as_tibble() %>% 
  rename(adm0 = layer)

prov_countries_adm0 <- prov_countries %>% 
  select(-ID) %>% 
  bind_cols(adm1_adm0 %>% select(-ID))


# Join cluster data
prov_countries_adm0_clust <- left_join(prov_countries_adm0, data_clusters)

# Group and calculate the number of clusters in each country
n_clusters <- prov_countries_adm0_clust %>% 
  # filter units that don't belong to any group
  filter(!cluster == -1) %>% 
  filter(!is.na(cluster)) %>% 
  group_by(adm0) %>% 
  summarise(n_clusters = n_distinct(cluster)) %>% 
  mutate(n_clusters = as.numeric(n_clusters))

# Plot number of clusters on a map
# Join geometry information to n_clusters
sf_countries <- as.polygons(rast('data_gis/gini_Adm0_5arcmin.tif')) %>% 
  st_as_sf() %>% 
  rename(adm0 = layer)
  

mydata <- left_join(sf_countries, 
                    n_clusters) %>% 
  filter(!adm0 == 10)


# clust_data <- left_join(st_read('data/gadm36_levels.gpkg', layer = 'level1') %>% 
#                           as_tibble() %>% 
#                           #rmapshaper::ms_simplify(.,keep=0.1,keep_shapes = T) %>% 
#                           #dplyr::select(-geom) %>% 
#                           tibble::rowid_to_column(., var='nmbr') %>%
#                           dplyr::select(nmbr, GID_0, NAME_0, GID_1, NAME_1), dplyr::select(data_stand, nmbr, cluster))
# 
# sf_provinces <- st_read('data/gadm36_levels.gpkg', layer = 'level1') %>% as_tibble() %>%  dplyr::select(GID_1,geom)
# 
# clust_data <- left_join(sf_provinces, clust_data, by = c('GID_1' = 'GID_1')) 

# Write to gpkg
st_write(mydata, 'results/n_clusters_gadm0.gpkg', delete_dsn = T)
#st_write(clust_data, 'data/clusters_gadm1.gpkg', layer = 'clusters_gadm1.gpkg', driver ='GPKG',append=FALSE)

plot_nclusters <- tm_shape(mydata, projection = "+proj=robin") +
  tm_fill(col = "n_clusters",
              style = "cat",
              palette = rev(colorspace::sequential_hcl(palette = "Purple-Yellow", n=6)),
              lwd = NA,
              textNA = "no data",
              colorNA = "#656765",
              legend.show=T,
              legend.is.portrait = FALSE) +
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
plot_nclusters


p_fig = plot_nclusters +
  tm_layout(legend.show=FALSE)

tmap_save(p_fig,filename = paste0('figures/fig_','map_nclusters','.png'),width = 110, units='mm', dpi = 450)

tmap::tmap_save(plot_nclusters, 'figures/Fig4b_n_clusters.pdf')

# Final figure in 4b plotted in QGIS using the gpkg created above

