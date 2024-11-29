# Gini - GNI per capita analyses

These codes were used to analyse the interplay of subnational Gini coefficient and GNI per capita at the global scale. It is part of the following publication.  Please do cite this paper when using the codes.

Chrisendo, Venla, Hoffman, Masoumzadeh Sayyar, Rocha, Sandström, Solt, Kummu. 2024. Income inequality has increased for over two-thirds of the global population. Preprint. 

To run the code, you first need to run the following codes:
Gini coefficient data creation: https://github.com/mattikummu/subnatGini
GNI per capita data creation: https://github.com/mattikummu/subnatGNI


The code is numbered with the order it should be run. Below each code is briefly explained. We used R (version 4.3.2) to develop the code.

**0_install_packages.R**: install the needed packages

**1_gini_gni_bins.R**: puts together admin 0 level (national) data. 

**2_gini_gni_groups**: Interpolates and extrapolates the missing values.

**3_plot_gini_gni_bins.R**: puts together admin 1 level (sbunational) data, interpolates and extrapolates the missing values

**4_fuzzy_clustering.R**: calculates the ratio between admin 1 and admin 0 level

**5_urban_gini_analysis.R**:  combines the admin 0 and admin 1 level to a global grid and gpkg file; puts data to raster and polygon files


**functions**: the functions used in the scripts above are in this folder


For more information, please contact Matti Kummu (matti.kummu@aalto.fi)