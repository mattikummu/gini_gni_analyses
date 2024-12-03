# Gini - GNI per capita analyses

These codes were used to analyse the interplay of subnational Gini coefficient and GNI per capita at the global scale. It is part of the following publication.  Please do cite this paper when using the codes.

Chrisendo D, Niva V, Hoffman R, Sayyar SM, Rocha J, Sandström V, Solt F, Kummu M. 2024. Income inequality has increased for over two-thirds of the global population. Preprint. doi: https://doi.org/10.21203/rs.3.rs-5548291/v1

To run the code, you first need to run the following codes:

Gini coefficient data creation: https://github.com/mattikummu/subnatGini

GNI per capita data creation: https://github.com/mattikummu/subnatGNI


The code is numbered with the order it should be run. Below each code is briefly explained. We used R (version 4.3.2) to develop the code.

**0_install_packages.R**: install the needed packages

**1_gini_gni_bins.R**: creates the Gini - GNI bins

**2_gini_gni_groups**: creates and plots the Gini - GNI combination groups

**3_plot_gini_gni_bins.R**: plots the bins

**4_fuzzy_clustering.R**: performs the fuzzy clustering

**5_urban_gini_analysis.R**:  code for the urban gini analysis


**functions**: the functions used in the scripts above are in this folder


For more information, please contact Matti Kummu (matti.kummu@aalto.fi)