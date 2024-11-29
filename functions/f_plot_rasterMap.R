f_plot_rasterMap <- function(r_in, colorpal,
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