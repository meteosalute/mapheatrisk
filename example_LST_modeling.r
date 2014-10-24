###########################################################################################################################################
# Urban hazard risk analyses for identifying urban heat-related human ederly health risk areas in main the most populous Italian cities.

# Marco Morabito  Alfonso Crisci  Beniamino Gioli  Giovanni Gualtieri  Piero Toscano, Simone Orlandini, Gian Franco Gensini
# Institute of Biometeorology, National Research Council - Via Giovanni Caproni 8, 50145 Florence, Italy.
# Interdepartmental Centre of Bioclimatology, University of Florence - Piazzale delle Cascine 18, 50144 Florence, Italy.
# Department of Agrifood Production and Environmental Sciences, University of Florence - Piazzale delle Cascine 18, 50144 Florence, Italy.
# Clinica Medica e Cardiologia, University of Florence - Viale Morgagni 85, 50134 Florence, Italy.
#############################################################################################################################################

#############################################################################################################################################
# Load R libraries

library(raster)
library(rgdal)
library(mgcv)

#############################################################################################################à

ndvi=raster("ndvi_city_layers/bari_ndvi_ago_n.tif")
cover=raster("land_cover_layers/r_globcover_bari.tif")
lst_day_bari=raster("lst_city_layers/r_bari_lst_day_ago.tif")
name_city="bari"

lstmodeling(ndvi,lst_day_bari,cover,names_city,dirdest="example")

#############################################################################################################à
