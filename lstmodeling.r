###########################################################################################################################################
# Urban hazard risk analyses for identifying urban heat-related human ederly health risk areas in main the most populous Italian cities.

# Marco Morabito  Alfonso Crisci  Beniamino Gioli  Giovanni Gualtieri  Piero Toscano, Simone Orlandini, Gian Franco Gensini
# Institute of Biometeorology, National Research Council - Via Giovanni Caproni 8, 50145 Florence, Italy.
# Interdepartmental Centre of Bioclimatology, University of Florence - Piazzale delle Cascine 18, 50144 Florence, Italy.
# Department of Agrifood Production and Environmental Sciences, University of Florence - Piazzale delle Cascine 18, 50144 Florence, Italy.
# Clinica Medica e Cardiologia, University of Florence - Viale Morgagni 85, 50134 Florence, Italy.
#############################################################################################################################################

##################################################################################################################################
# Function to estimate MODIS LST 250 m from MODIS NDVI 250 m by using ESA Globcover as model driver. 
# code: lstmodeling.r
# sogliaq is the value of minimal summer LST treshsold. 
# npixel is the minimal numerosity of pixel's image to allow model LST-NDVI fitting for each globcover class.
##################################################################################################################################

lstmodeling <- function(ndvi,lst,cover,names_city,sogliaq=10,npixel=150,dirdest="example"){
	  
require(raster)
require(rgdal)
require(mgcv)
name=name_city


#####################################################################
# Threshold for ndvi values

sogliad <- function(x) { x[x<sogliaq] <- NA; return(x) }

#####################################################################
# Filter raster of lower lst values

lst_n<- calc(lst,sogliad)

#####################################################################
# Build a working matrix and exclude missing values

mat_work=data.frame(ndvi=as.vector(values(ndvi)),
                    lst_val=as.vector(values(lst_n)),
					cover=as.vector(values(cover)),
					indexrow=c(1:length(cover)))
					
mat_workn=na.exclude(mat_work)

#####################################################################
# Check and list cover class in a vector

level_cover=as.numeric(levels(as.factor(mat_workn$cover)))

#####################################################################
# Instance raster layers

res_gam_ras=ndvi
res_gam_e_ras=ndvi

res_gam=rep(NA,length(as.vector(values(ndvi))))
res_gam_e=rep(NA,length(as.vector(values(ndvi))))

level_cover_l=list()
model_gam_l=list()
tempdata_e_l=list()
full_data_l=list()

#####################################################################
# Loop to fit gam model for each cover class
 
j=0

for ( i in 1:length(level_cover)) {
                         
						 tempdata=na.exclude(mat_workn[mat_workn$cover==level_cover[i],])
						 if (nrow(tempdata) <=npixel) {next}
						 if (level_cover[i] >=200) {next}
                         j=j+1
                        
						 level_cover_l[[j]]=level_cover[i]
						 
						 tempdata_e_l[[j]]=tempdata
						 
                         full_data_l[[j]]=mat_workn[mat_workn$cover==level_cover[i],]

						 options(show.error.messages = FALSE)
						 model_day_g <- try(gam(lst_val ~ s(ndvi),data=tempdata))
						 resp=.Last.value[1]
						 
						 options(show.error.messages = TRUE)
						 
						 if (resp == "try-error") {model_gam_l[[j]]=lm(lst_val ~ ndvi,data=tempdata)}
						 else { 
                                model_gam_l[[j]]=gam(lst_val ~ s(ndvi),data=tempdata)       
                          } 
						
						 
                        }

#####################################################################
# Prepare list of data-object for merging predictions of different models
                        						
fun_list=list(level_cover_l,model_gam_l,tempdata_e_l)

#####################################################################
# Assessing

for ( i in 1:length(tempdata_e_l)) { 
                                    res_gam[full_data_l[[i]]$indexrow]=as.vector(predict(model_gam_l[[i]],data.frame(ndvi=full_data_l[[i]]$ndvi)))
                                    res_gam_e[tempdata_e_l[[i]]$indexrow]=model_gam_l[[i]]$residuals
                                    }

#####################################################################									
# fill results
						
values(res_gam_ras)<-as.vector(res_gam)
values(res_gam_e_ras)<-as.vector(res_gam_e)

#####################################################################
# export results in GeoTIFF and KML format by using R raster library.

writeRaster(res_gam_ras,filename=paste(dirdest,"/",name,"_LST_gam.tif",sep=""),format="GTiff",overwrite=T)
KML(res_gam_ras,file=paste(dirdest,"/",name,"_LST_gam.kmz",sep=""),col=rev(heat.colors(255)),overwrite=T)
writeRaster(res_gam_e_ras,filename=paste(dirdest,"/",name,"_LST_gam_res.tif",sep=""),format="GTiff",overwrite=T)
                        
cat(paste0("Work done for ",name,"\n"))

}
