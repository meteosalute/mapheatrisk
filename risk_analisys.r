###########################################################################################################################################
# Urban hazard risk analyses for identifying urban heat-related human ederly health risk areas in main the most populous Italian cities.

# Marco Morabito  Alfonso Crisci  Beniamino Gioli  Giovanni Gualtieri  Piero Toscano, Simone Orlandini, Gian Franco Gensini
# Institute of Biometeorology, National Research Council - Via Giovanni Caproni 8, 50145 Florence, Italy.
# Interdepartmental Centre of Bioclimatology, University of Florence - Piazzale delle Cascine 18, 50144 Florence, Italy.
# Department of Agrifood Production and Environmental Sciences, University of Florence - Piazzale delle Cascine 18, 50144 Florence, Italy.
# Clinica Medica e Cardiologia, University of Florence - Viale Morgagni 85, 50134 Florence, Italy.
#############################################################################################################################################




###########################################################################################################################################
# Load libraries

library(raster)
library(rgdal)
library(classInt)
library(rasterVis)
library(maptools)
require(reshape)
require(hash)
require(doBy)

###########################################################################################################################################
# retrieve list name of city

citta=readRDS("gis_city_resources/city.rds")

####################################################################################################‡‡
# retrieve population list of density layers
 
filepopover=sapply(Sys.glob(file.path("pop_city_layers","popover*.tif")),raster)
filepop=sapply(Sys.glob(file.path("pop_city_layers","pop*.tif")),raster)

# retrieve poligonal boundary of areas 
 
list_area=readRDS("gis_city_resources/list_pol_area.rds")


#######################################################################################################
# Area surface calculation

res_area_pol=list()

for ( i in 1:length(list_area)){

    res_area_pol[[i]]=data.frame(area=unlist(lapply(slot(list_area[[i]], "polygons"),function(x) x@area))[1],citta=citta[i])
                             
}

aree_pols_df=reshape::merge_all(res_area_pol)

saveRDS(aree_pols_df,file="final_data/aree_pols_df.rds")

################################################################################################################################
# Retrieve data from GIS Administrative Level 2 ISTAT and ADM2 level

shn2comune=readRDS("final_data/shn2comune.rds")
list_comuni=readRDS("gis_city_resources/list_comuni.rds")

####################################################################################
# Calculation of areas of ADM2 units

res_area_com=list()

for ( i in 1:length(list_comuni)){

    res_area_com[[i]]=data.frame(area=unlist(lapply(slot(list_comuni[[i]], "polygons"),function(x) x@area)),cittashn=as.character(list_comuni[[i]]@data$SHN),comune=NA)
    for ( j in 1:length(res_area_com[[i]]$cittashn)){ res_area_com[[i]]$comune[j]=shn2comune[[as.character(res_area_com[[i]]$cittashn[j])]]
                         

}
}

aree_comuni_df=reshape::merge_all(res_area_com)

saveRDS(aree_comuni_df,file="final_data/aree_comuni_df.rds")

#####################################################################################################################################
# For each city and each class calculate areas in the city - Day 

list_area_day=readRDS("final_data/list_day_class.rds")

res_area_day=list()

for ( i in 1:length(list_area_day)){

    res_area_day[[i]]=data.frame(area=unlist(lapply(slot(list_area_day[[i]], "polygons"),function(x) x@area)),classe=list_area_day[[1]]@data$z,citta=citta[i])
                          
}
res_area_day_df=reshape::merge_all(res_area_day)

saveRDS(res_area_day_df,file="final_data/res_area_day_df.rds")

#####################################################################################################################################
# For each city and each class calculate areas in the city - Night 

list_area_nig=readRDS("final_data/list_nig_class.rds")

res_area_nig=list()

for ( i in 1:length(list_area_nig)){

    res_area_nig[[i]]=data.frame(area=unlist(lapply(slot(list_area_nig[[i]], "polygons"),function(x) x@area))[1:5],classe=list_area_nig[[1]]@data$z,citta=citta[i])
                          
}

res_area_nig_df=reshape::merge_all(res_area_nig)

saveRDS(res_area_nig_df,file="final_data/res_area_nig_df.rds")

#####################################################################################################################################
# Preparing blank layers 


raster_com=list()

for ( i in 1:length(filepopover)) {
      list_comuni[[i]]@data$ID=as.numeric(row.names(list_comuni[[i]]@data))
      r=filepop[[i]]*0
	  
	  raster_com[[i]] <- rasterize(list_comuni[[i]], r, 'ID',fun='last')

}

saveRDS(raster_com,file="final_data/raster_com.rds")

#####################################################################################################################################
# Naming commune fron shn code

ras_citta_hash=list()

for ( i in 1:length(res_area_com)) {
      
	  ras_citta_hash[[i]]=hash(as.character(as.numeric(row.names(list_comuni[[i]]@data))),as.character(res_area_com[[i]]$comune))
      
}

saveRDS(ras_citta_hash,file="final_data/ras_citta_hash.rds")
saveRDS(res_area_com,file="final_data/res_area_com.rds")

#####################################################################################################################################
# Preparing data stacks for risk calculations

ras_citta_df=list()

for ( i in 1:length(citta)) {

list_comuni[[i]]@data$ID=as.numeric(row.names(list_comuni[[i]]@data))

pop_citta=raster(Sys.glob(file.path("pop_city_layers",paste0("pop_",citta[i],"*.tif"))))
popover_citta=raster(Sys.glob(file.path("pop_city_layers",paste0("popover*",citta[i],"*.tif"))))
raster_citta=rasterize(list_comuni[[i]], pop_citta*0, 'ID')
lst_day_citta=raster(Sys.glob(file.path("summer_LST_layer_100m",paste0(citta[i],"*day*.tif"))))
lst_nig_citta=raster(Sys.glob(file.path("summer_LST_layer_100m",paste0(citta[i],"*nig*.tif"))))



data_citta=data.frame(pop_citta=getValues(pop_citta),
                      popover_citta=getValues(popover_citta),
                      raster_citta=getValues(raster_citta),
                      lst_day_citta=getValues(lst_day_citta),
                      lst_nig_citta=getValues(lst_nig_citta))
                       


ras_citta_df[[i]]=data_citta

print(i)

}

saveRDS(ras_citta_df,file="final_data/ras_citta_df.rds")


############################################################################################################################################‡
# Risk procedure calculation

ras_citta_df=readRDS("final_data/ras_citta_df.rds")

for ( i in 1:length(ras_citta_df)) {


ind_na_pop=which(!is.na(ras_citta_df[[i]]$pop_citta))
ind_na_popover=which(!is.na(ras_citta_df[[i]]$popover_citta))

ind_na_lst_day=which(!is.na(ras_citta_df[[i]]$lst_day_citta))
ind_na_lst_nig=which(!is.na(ras_citta_df[[i]]$lst_nig_citta))

ras_citta_df[[i]]$Npop_citta=NA
ras_citta_df[[i]]$Npopover_citta=NA
ras_citta_df[[i]]$Nlst_day=NA
ras_citta_df[[i]]$Nlst_nig=NA
ras_citta_df[[i]]$Wpop=NA
ras_citta_df[[i]]$Wlst_nig=NA
ras_citta_df[[i]]$Wlst_day=NA
ras_citta_df[[i]]$risk_day=NA
ras_citta_df[[i]]$risk_nig=NA
ras_citta_df[[i]]$Crisk_day=NA
ras_citta_df[[i]]$Crisk_nig=NA

################################################################################################################‡
# Normalize layers

ras_citta_df[[i]]$Npop_citta[ind_na_pop]=scale(as.vector(ras_citta_df[[i]]$pop_citta[ind_na_pop]),center=min(as.vector(ras_citta_df[[i]]$pop_citta[ind_na_pop])),scale=diff(range(as.vector(ras_citta_df[[i]]$pop_citta[ind_na_pop]))))

ras_citta_df[[i]]$Npopover_citta[ind_na_popover]=scale(as.vector(ras_citta_df[[i]]$popover_citta[ind_na_popover]),center=min(as.vector(ras_citta_df[[i]]$popover_citta[ind_na_popover])),scale=diff(range(as.vector(ras_citta_df[[i]]$popover_citta[ind_na_popover]))))

ras_citta_df[[i]]$Nlst_day[ind_na_lst_day]=scale(as.vector(ras_citta_df[[i]]$lst_day[ind_na_lst_day]),center=min(as.vector(ras_citta_df[[i]]$lst_day[ind_na_lst_day])),scale=diff(range(as.vector(ras_citta_df[[i]]$lst_day[ind_na_lst_day]))))

ras_citta_df[[i]]$Nlst_nig[ind_na_lst_nig]=scale(as.vector(ras_citta_df[[i]]$lst_nig[ind_na_lst_nig]),center=min(as.vector(ras_citta_df[[i]]$lst_nig[ind_na_lst_nig])),scale=diff(range(as.vector(ras_citta_df[[i]]$lst_nig[ind_na_lst_nig]))))


################################################################################################################‡
# Normalize layers

ras_citta_df[[i]]$Wpop[ind_na_pop]=(0.5*ras_citta_df[[i]]$Npop_citta[ind_na_pop]+0.5*ras_citta_df[[i]]$Npopover[ind_na_pop])*0.5


ind_na_Wpop=which(is.na(ras_citta_df[[i]]$Wpop))

ras_citta_df[[i]]$Wlst_nig=0.5*ras_citta_df[[i]]$Nlst_nig
ras_citta_df[[i]]$Wlst_day=0.5*ras_citta_df[[i]]$Nlst_day
ras_citta_df[[i]]$Wpop[ind_na_Wpop]=0

ras_citta_df[[i]]$risk_day=ras_citta_df[[i]]$Wpop+ras_citta_df[[i]]$Wlst_day
ras_citta_df[[i]]$risk_nig=ras_citta_df[[i]]$Wpop+ras_citta_df[[i]]$Wlst_nig


################################################################################################################‡
# Risk class calculation 

ind_na_Wday=which(!is.na(ras_citta_df[[i]]$risk_day))
ind_na_Wnig=which(!is.na(ras_citta_df[[i]]$risk_nig))

# break class 

brks.day = classIntervals(ras_citta_df[[i]]$risk_day[ind_na_Wday], n =5,style="fixed",fixedBreaks=c(0,0.20,0.40,0.60,0.80,1))
brks.nig = classIntervals(ras_citta_df[[i]]$risk_nig[ind_na_Wnig], n =5,style="fixed",fixedBreaks=c(0,0.20,0.40,0.60,0.80,1))

ras_citta_df[[i]]$Crisk_day=findInterval(ras_citta_df[[i]]$risk_day, brks.day$brks)
ras_citta_df[[i]]$Crisk_nig=findInterval(ras_citta_df[[i]]$risk_nig, brks.nig$brks)
}

saveRDS(ras_citta_df,file="final_data/ras_citta_full.rds)")


###################################################################################
