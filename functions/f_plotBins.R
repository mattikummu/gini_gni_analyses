
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