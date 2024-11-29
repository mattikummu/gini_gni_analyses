
# function to create inequality-income bins
f_create_bins <- function(xRast = r_gini_mean_rev, 
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