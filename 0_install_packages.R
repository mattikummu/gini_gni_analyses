## install all required packages

# code for  gini - gni per capita analyses
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

required_packages <- c(
  "terra", "sf",
  "reldist", "stringr",
  "tmap", "boot", "tidyverse", "dplyr",
  "tidyterra", "tidyr", "tibble",
  "rmapshaper", "pals", "e1071", "openxlsx",
  "factoextra", "clusterSim", "forcats", "ggplot2",
  "viridis", "scales", "rnaturalearth", "paletteer",
  "rasterVis", "raster", "rgeoda", "biscale"
)

# Function to check if a package is installed, and if not, install it
install_if_missing <- function(pack) {
  if (!require(pack, character.only = TRUE, quietly = TRUE)) {
    install.packages(pack, dependencies = TRUE)
    library(pack, character.only = TRUE)
  }
}

# Apply the function to each package
lapply(required_packages, install_if_missing)

# Now that all packages are installed, load them
lapply(required_packages, library, character.only = TRUE)
