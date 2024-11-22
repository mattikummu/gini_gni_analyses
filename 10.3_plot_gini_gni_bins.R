#### This script plots inequality-income bins 


#install.packages(c('paletteer','rasterVis'))


library(terra)

library(tidyverse)
library(dplyr) 

library(paletteer)
library(tmap)
library(rasterVis)
library(ggplot2)
library(sf)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# create outline
r_popCount <- subset(terra::rast("../GIS_data_common/GlobPOP/GlobPOP_5arcmin_1990_2022_v2.tif"),32)

shoreline = r_popCount
shoreline[!is.na(shoreline)] <- 1

shoreline_rob <- terra::project(x = shoreline, y = "+proj=robin", method = "near")

sf_shoreline <- terra::as.polygons(shoreline_rob) |> st_as_sf()

# # breaks:
# nBreaks = 8

# increase the number of max.categories in tmap
tmap_options(max.categories = 100)


sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.2, keep_shapes = T) %>%
  st_as_sf() %>% 
  filter(!iso_a3 == 'ATA')

# 
# seed.in = c(seq(11,18), seq(21,28), seq(31,38), seq(41,48), 
#             seq(51,58), seq(61,68), seq(71,78), seq(81,88))



#### set colour palette ----

colmat <- function(nbreaks = 4, breakstyle = 'quantile', #"fixed",
                   upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = FALSE) {
  # TODO - replace any tidyr, dplyr etc. functions with data.table #
  library(tidyverse)
  require(ggplot2)
  require(classInt)
  if (breakstyle == "sd") {
    warning("SD breaks style cannot be used.\nWill not always return the correct number of breaks.\nSee classInt::classIntervals() for details.\nResetting to quantile",
            call. = FALSE, immediate. = FALSE)
    breakstyle <- "quantile"}
  # The colours can be changed by changing the HEX codes for:
  # upperleft, upperright, bottomleft, bottomright
  # From http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  # upperleft = "#64ACBE", upperright = "#574249", bottomleft = "#E8E8E8", bottomright = "#C85A5A",
  # upperleft = "#BE64AC", upperright = "#3B4994", bottomleft = "#E8E8E8", bottomright = "#5AC8C8",
  # upperleft = "#73AE80", upperright = "#2A5A5B", bottomleft = "#E8E8E8", bottomright = "#6C83B5",
  # upperleft = "#9972AF", upperright = "#804D36", bottomleft = "#E8E8E8", bottomright = "#C8B35A",
  # upperleft = "#DA8DC8", upperright = "#697AA2", bottomleft = "#E8E8E8", bottomright = "#73BCA0",
  # Similar to Teuling, Stockli, Seneviratnea (2011) [https://doi.org/10.1002/joc.2153]
  # upperleft = "#F7900A", upperright = "#993A65", bottomleft = "#44B360", bottomright = "#3A88B5",
  # Viridis style
  # upperleft = "#FEF287", upperright = "#21908D", bottomleft = "#E8F4F3", bottomright = "#9874A1",
  # Similar to Fjeldsa, Bowie, Rahbek 2012
  # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
  # Default from original source
  # upperleft = "#0096EB", upperright = "#820050", bottomleft= "#BEBEBE", bottomright = "#FFE60F",
  
  # # For 4x4 matrix
  # mybreaks <- rep(1:4, each = 4)*10 + c(1,2,3,4)
  # 
  
  my.data <- seq(0, 1, .01) #seq(11, 44, 11)#
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(my.data,
                                       n = nbreaks,
                                       style = breakstyle,
  )
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- classInt::findColours(my.class, my.col)
  }
  
  ## need to convert this to data.table at some stage.
  # col.matrix.plot <- col.matrix %>%
  #   as.data.frame(.) %>% 
  #   mutate("Y" = row_number()) %>%
  #   mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
  #   pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>% 
  #   mutate("X" = as.integer(sub("V", "", .$X))) %>%
  #   distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
  #   mutate(Y = rev(.$Y)) %>% 
  #   dplyr::select(-c(4)) %>%
  #   mutate("Y" = rep(seq(from = 1, to = nbreaks, by = 1), each = nbreaks),
  #          "X" = rep(seq(from = 1, to = nbreaks, by = 1), times = nbreaks)) %>%
  #   mutate("UID" = row_number())
  
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>%
    mutate("GNI" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>%
    pivot_longer(data = ., cols = -GNI, names_to = "GINI", values_to = "HEXCode") %>%
    mutate("GINI" = as.integer(sub("V", "", .$GINI))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(GNI = rev(.$GNI)) %>%
    dplyr::select(-c(4)) %>%
    mutate("GNI" = rep(seq(from = 1, to = nbreaks, by = 1), each = nbreaks),
           "GINI" = rep(seq(from = 1, to = nbreaks, by = 1), times = nbreaks)) %>%
    mutate("UID" = row_number())
  
  # Use plotLeg if you want a preview of the legend
  if (plotLeg) {
    p <- ggplot(col.matrix.plot, aes(GINI, GNI, fill = HEXCode)) +
      geom_tile() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 12, colour = "black",hjust = 0.5, 
                                      vjust = 1),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
    print(p)
    assign(
      x = "BivLegend",
      value = p,
      pos = .GlobalEnv
    )
  }
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    ggsave(filename = "bivLegend.pdf", plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in",
           dpi = 300)
  }
  seqs <- seq(0, 100, (100 / nbreaks)) #seq(11,44, (44/4)) 
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  attr(col.matrix, "breakstyle") <- breakstyle
  attr(col.matrix, "nbreaks") <- nbreaks
  return(col.matrix)
}


#### plot bins function ----

f_plotBins <- function(rBins = bins_social,
                       colPalette = col.matrix_unempl_gnic,
                       xName = xNameSet,
                       yName = yNameSet,
                       nBreaks = 8){
  
  
  # seed at top of each raster to ensure each ID is in plot
  # seed.in = seq(11,14) #seq(11,44,11) 
  # for (i in 2:nBreaks) {
  #   addSeed = seq((i*10+1),(i*10+nBreaks))
  #   seed.in = c(seed.in, addSeed)
  #   
  # }
  # 
  # # rBins = bins_unempl_gnic
  # # colPalette = col.matrix_unempl_gnic
  # # v <- c('a', 'b')
  # # rep(v, each=10, 20)
  # 
  # # to ensure that all values are in the map 
  # rBins[1:3,1:(3*length(seed.in))] = rep(seed.in, each = 3, 3)
  rBins_projected = terra::project(x = rBins, y = "+proj=robin", method = "near")
  # rBins_projected[1:20,1:64] = rep(seed.in, each = 20)
  #rBins_projected[1200,1101:1164] = seed.in
  #rBins_projected[1000:1020,1101:1164] = seed.in
  
  # col pal to right format
  bivir_pal = as.vector(colPalette[2:(nBreaks+1),2:(nBreaks+1)])
  
  # Check colours
  #c("#D3D3D3" "#C69BB5" "#B96397" "#AC2C7A" "#ABBF9F" "#9E8B87" "#92576F" "#852458" "#83AC6B" "#777B59" "#6B4B47" "#5F1C36" "#5C9938" "#506C2C""#444020" "#391415")
  
  map =  
    tm_shape(sf_shoreline) +
    tm_fill(col = "white", lwd = 0, border.col = NA) +
    tm_shape(rBins_projected) +
    tm_raster(palette = bivir_pal,
              style = "cat",
              colorNA = NULL) +
    tm_shape(sf_shoreline) + 
    tm_borders(col = "black", lwd = 0.1) +
    tm_layout(legend.show = F, legend.frame = F, frame = F)+
    tm_shape(sf_adm0) + 
    tm_borders(col = "grey", lwd = 0.1)
  map
  
  
  p_fig = map +
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/fig_','gini_gni_bins','.png'),width = 80, units='mm', dpi = 450)
  
  
  ## plot legend
  
  # col.matrix.plot <- colPalette %>%
  #   as.data.frame(.) %>% 
  #   mutate("Y" = row_number()) %>%
  #   mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
  #   pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>% 
  #   mutate("X" = as.integer(sub("V", "", .$X))) %>%
  #   distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
  #   mutate(Y = rev(.$Y)) %>% 
  #   dplyr::select(-c(4)) %>%
  #   mutate("Y" = rep(seq(from = 1, to = nBreaks, by = 1), each = nBreaks),
  #          "X" = rep(seq(from = 1, to = nBreaks, by = 1), times = nBreaks)) %>%
  #   mutate("UID" = row_number())
  
  col.matrix.plot <- colPalette %>%
    as.data.frame(.) %>%
    mutate("GNI" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>%
    pivot_longer(data = ., cols = -GNI, names_to = "GINI", values_to = "HEXCode") %>%
    mutate("GINI" = as.integer(sub("V", "", .$GINI))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(GNI = rev(.$GNI)) %>%
    dplyr::select(-c(4)) %>%
    mutate("GNI" = rep(seq(from = 1, to = nBreaks, by = 1), each = nBreaks),
           "GINI" = rep(seq(from = 1, to = nBreaks, by = 1), times = nBreaks)) %>%
    mutate("UID" = row_number())
  xlab = xName
  ylab = yName
  
  p <- ggplot(col.matrix.plot, aes(GINI, GNI, fill = HEXCode)) +
    geom_tile() +
    scale_fill_identity() +
    coord_equal(expand = FALSE) +
    theme_void() +
    theme(aspect.ratio = 1,
          axis.title = element_text(size = 12, colour = "black",hjust = 0.5, 
                                    vjust = 1),
          axis.title.y = element_text(angle = 90, hjust = 0.5)) +
    xlab(bquote(.(xlab) ~  symbol("\256"))) +
    ylab(bquote(.(ylab) ~  symbol("\256")))
  
  # to get the map to the plot_grid
  map_tg <- tmap_grob(map)
  
  p_comb <- cowplot::plot_grid(map_tg, p, rel_widths = c(6,1))
  
  #
  
  return(p_comb)
  
}


#### Fig 3c: inequality - income bins ----

# read bins
gini_gni_bins <- terra::rast('results/inequality_income_bins_raster_4_2024-08-08.tif')

nBrks = 4
# names
xNameSet = "Inequality (GINI) increases"
yNameSet = "Income (GNI) increases"

# Create the colour matrix
col.matrix_unempl_gnic <- colmat(nbreaks = nBrks, breakstyle = "quantile",
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



cowplot::save_plot(plot=p_incEqua, filename = paste0('figures/Fig3c_bins_N',nBrks,'_','GINI','_','GNI_',Sys.Date(),'.pdf')) #base_height = 6, base_width = 24


