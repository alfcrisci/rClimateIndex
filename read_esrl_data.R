#########################################################################################################
# Authors : Alfonso Crisci 
# IBIMET CNR Institute of Biometeorology Firenze via Caproni 8,50145,Italia
# mail: a.crisci@ibimet.cnr.it
# file: .r
# github: https://github.com/alfcrisci
# Data NCEP/DOE 2 Reanalysis data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA,Web site at http://www.esrl.noaa.gov/psd/ 

#########################################################################################################

# install.packages('climates',,'http://www.rforge.net/')
# devtools::install_github("camposfa/plhdbR")

library(XML)
library(ncdf4)
library(rts)
library(sp)
library(ncdf)
library(raster)
library(rgdal)
library(xts)

#########################################################################################################
# http://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/catalog.html

setwd("/home/alf/Scrivania/lav_betti_RNCEP")

dataset_catalog_data=htmlParse("http://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/catalog.html")

#########################################################################################################


years_rea_2=c(1979:(as.numeric(format(Sys.Date(),"%Y"))-1))


retrieve_NCEP_pressure=function(dataset="ncep.reanalysis2.dailyavgs",var="air",level=1000,year=1979,minLon,maxLon,minLat,maxLat) {
                               require(raster)
                               require(ncdf)
                               e <- extent(minLon,maxLon,minLat,maxLat)
                               levels_NCEP_pressure=as.list(c(1:17))
                               names(levels_NCEP_pressure)=as.character(c(1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10))
                               nlevel=as.numeric(levels_NCEP_pressure[as.character(level)])
                               dataset = brick(paste0("http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/",dataset,"/pressure/",var,".",as.character(year),".nc"),level=nlevel)
                               proj4string(dataset) <- CRS("+init=epsg:4326")
                               return(crop(dataset,e))
                               }



################################################################################################################

index_WY_list=list()

for ( i in 1:length(years_rea_2)) {   uwnd_850=retrieve_NCEP_pressure(var="uwnd",year=years_rea_2[i],level=850,minLon=40,maxLon=110,minLat=0,maxLat=20)
                                       uwnd_200=retrieve_NCEP_pressure(var="uwnd",year=years_rea_2[i],level=200,minLon=40,maxLon=110,minLat=0,maxLat=20)
                                       area_index_WY=uwnd_850-uwnd_200
                                       WY_daily <- data.frame(WY_index_daily=apply(as.array(area_index_WY),3, mean),dates=as.Date(uwnd_850@z$time))
                                       index_WY_list[[i]]=WY_daily                   
                                       }
                              
saveRDS(index_WY_list,file="index_WY_list.rds")

index_WY=do.call("rbind",index_WY_list)


# Webster-Yang monsoon index (U850-U200 averaged over 0-20N, 40E-110E)
# Webster, P.J., and S.Yang, 1992: Monsoon and ENSO: Selectively interactive
# systems. Quart. J. Roy. Meteor. Soc., 118, 877-926.


################################################################################################################


index_AUMi_list=list()

for ( i in 1:length(years_rea_2)) { uwnd_850=retrieve_NCEP_pressure(var="uwnd",year=years_rea_2[i],level=850,minLon=110,maxLon=150,minLat=-15,maxLat=-2.5)
                                    AUMi_daily <- data.frame(AUMi_index_daily=apply(as.array(uwnd_850),3, mean),dates=as.Date(uwnd_850@z$time))
                                    index_AUMi_list[[i]]=AUMi_daily                   
}

saveRDS(index_AUMi_list,file="index_AUMi_list.rds")

# Australian monsoon index (U850 averaged over 2.5S-15S, 110E-150E)
# Hung, C.-W, and M. Yanai, 2004: Factors contributing to the onset of the
# Australian summer monsoon. Quart. J. Roy. Meteor. Soc., 130, 739-758.

################################################################################################################

index_SAMi_list=list()

for ( i in 1:length(years_rea_2)) {    vwnd_850=retrieve_NCEP_pressure(var="vwnd",year=years_rea_2[i],level=850,minLon=70,maxLon=110,minLat=10,maxLat=30)
                                       vwnd_200=retrieve_NCEP_pressure(var="vwnd",year=years_rea_2[i],level=200,minLon=70,maxLon=110,minLat=10,maxLat=30)
                                       area_index_SAMi=vwnd_850-vwnd_200
                                       SAMi_daily <- data.frame(SAMi_index_daily=apply(as.array(area_index_SAMi),3, mean),dates=as.Date(vwnd_850@z$time))
                                       index_SAMi_list[[i]]=SAMi_daily                   
}

saveRDS(index_SAMi_list,file="index_SAMi_list.rds")

# South Asian monsoon index (V850-V200 averaged over 10N-30N, 70E-110E)
# Goswami, B. N., B. Krishnamurthy, and H. Annamalai, 1999: A broad-scale
# Circulation index for interannual variability of the Indian summer monsoon.
# Quart. J. Roy. Meteor. Soc., 125, 611-633.

################################################################################################################


index_DInd_list=list()


for ( i in 1:length(years_rea_2)) {   uwnd_850=retrieve_NCEP_pressure(var="uwnd",year=years_rea_2[i],level=850,minLon=40,maxLon=80,minLat=5,maxLat=15)
                                       uwnd_850b=retrieve_NCEP_pressure(var="uwnd",year=years_rea_2[i],level=850,minLon=70,maxLon=90,minLat=20,maxLat=30)
                                       DInd_daily <- data.frame(DInd_index_daily=apply(as.array(uwnd_850),3, mean)-apply(as.array(uwnd_850b),3, mean),dates=as.Date(uwnd_850@z$time))
                                       index_DInd_list[[i]]=DInd_daily                   
}

saveRDS(index_DInd_list,file="index_DInd_list.rds")




# Dynamic Indian monsoon index (U850 (5N-15N, 40E-80E) - (U850 20N-30N, 70E-90E))
# Wang, B., and Z. Fan, 1999: Choice of south Asian summer monsoon indices.
# Bull. Amer. Meteor. Soc., 80, 629-638.




################################################################################################################
################################################################################################################



index_EAWP_list=list()
for ( i in 1:length(years_rea_2)) {   uwnd_850=retrieve_NCEP_pressure(var="uwnd",year=years_rea_2[i],level=850,minLon=90,maxLon=130,minLat=5,maxLat=15)
                                      uwnd_850b=retrieve_NCEP_pressure(var="uwnd",year=years_rea_2[i],level=200,minLon=110,maxLon=140,minLat=20,maxLat=30)
area_index_EAWP=uwnd_850-uwnd_200
EAWP_daily <- data.frame(EAWP_index_daily=apply(as.array(uwnd_850),3, mean)-apply(as.array(uwnd_850b),3, mean),dates=as.Date(uwnd_850@z$time))
index_EAWP_list[[i]]=EAWP_daily
}

saveRDS(index_EAWP_list,file="index_EAWP_list.rds")



# East Asian - Western North Pacific monsoon index (U850 (5N-15N, 90E-130E) - U850 (20N-30N, 110E-140E))

################################################################################################################
index_WY_list=readRDS(file="index_WY_list.rds")
index_AUMi_list=readRDS(file="index_AUMi_list.rds")
index_DInd_list=readRDS(file="index_DInd_list.rds")
index_EAWP_list=readRDS(file="index_EAWP_list.rds")
index_SAMi_list=readRDS(file="index_SAMi_list.rds")


index_WY=do.call("rbind",index_WY_list)
index_AUMi=do.call("rbind",index_AUMi_list)
index_DInd=do.call("rbind",index_DInd_list)
index_EAWP=do.call("rbind",index_EAWP_list)
index_SAMi=do.call("rbind",index_SAMi_list)

index_WY_xts=xts(zoo(index_WY$WY_index_daily,index_WY$dates))
index_AUMi_xts=xts(zoo(index_AUMi$AUMi_index_daily,index_WY$dates))
index_DInd_xts=xts(zoo(index_DInd$DInd_index_daily,index_WY$dates))
index_EAWP_xts=xts(zoo(index_EAWP$EAWP_index_daily,index_WY$dates))
index_SAMi_xts=xts(zoo(index_SAMi$SAMi_index_daily,index_WY$dates))

saveRDS(index_WY_xts,"index_WY_xts.rds")
saveRDS(index_AUMi_xts,"index_AUMi_xts.rds")
saveRDS(index_DInd_xts,"index_DInd_xts.rds")
saveRDS(index_EAWP_xts,"index_EAWP_xts.rds")
saveRDS(index_SAMi_xts,"index_SAMi_xts.rds")



################################################################################
# temperatura della toscana

# Create raster stack time series

years_rea_2=c(1979:(as.numeric(format(Sys.Date(),"%Y"))-1))

index_t_850_tusc_list=list()

for ( i in 1:length(years_rea_2)) {     t_850_tusc=retrieve_NCEP_pressure(var="air",year=years_rea_2[i],level=850,minLon=9,maxLon=13,minLat=41,maxLat=45)
                                        t_850_tusc_daily<- data.frame(t_850_tusc_index_daily=apply(as.array(t_850_tusc),3, mean),dates=as.Date(t_850_tusc@z$time))
                                        index_t_850_tusc_list[[i]]= t_850_tusc_daily                
}



saveRDS(index_t_850_tusc_list,file="index_t_850_tusc_list.rds")

index_t_850_tusc_list=do.call("rbind",index_t_850_tusc_list)



#########################################################################################################################################################################à
# http://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/noaa.oisst.v2.highres/catalog.html


retrieve_NCEP_sst=function(dataset="noaa.oisst.v2.highres",var="sst.day.anom",year=1981,minLon,maxLon,minLat,maxLat) {
  require(raster)
  require(ncdf)
  e <- extent(minLon,maxLon,minLat,maxLat)
  dataset = brick(paste0("http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/",dataset,"/",var,".",as.character(year),".v2.nc"),lvar=1)
  proj4string(dataset) <- CRS("+init=epsg:4326")
  return(crop(dataset,e))
}


# The IOD is commonly measured by an index that is the difference between sea surface temperature (SST) anomalies 
# in the western (50°E to 70°E and 10°S to 10°N) and eastern (90°E to 110°E and 10°S to 0°S) equatorial Indian Ocean. 
# The index is called the Dipole Mode Index (DMI). The map below shows the east and west poles of the IOD for November 1997; 


years_sst=c(1981:(as.numeric(format(Sys.Date(),"%Y"))))

IOD_sst_list=list()
betti_dipole_list=list()
med_occ_sst_list=list()
med_ori_sst_list=list()

for ( i in 1:length(years_sst)) {    IODa=retrieve_NCEP_sst(var="sst.day.anom",year=years_sst[i],minLon=50,maxLon=70,minLat=-10,maxLat=10)
                                      IODb=retrieve_NCEP_sst(var="sst.day.anom",year=years_sst[i],minLon=90,maxLon=110,minLat=-10,maxLat=10)
                                      IOD_daily <- data.frame(IOD_index_daily=apply(as.array(IODa),3, mean,na.rm=T)-apply(as.array(IODb),3, mean,na.rm=T),dates=as.Date(IODa@z$time))
                                      IOD_sst_list[[i]]= IOD_daily                
                                  }






for ( i in 1:length(years_sst)) {     medw=retrieve_NCEP_sst(var="sst.day.anom",year=years_sst[i],minLon=-6,maxLon=15,minLat=31,maxLat=46)
                                      medo=retrieve_NCEP_sst(var="sst.day.anom",year=years_sst[i],minLon=16,maxLon=35,minLat=31,maxLat=45)
                                      sst_medw_daily=data.frame(medw_index_daily=apply(as.array(medw),3, mean,na.rm=T),dates=as.Date(medw@z$time))
                                      sst_medo_daily=data.frame(medo_index_daily=apply(as.array(medo),3, mean,na.rm=T),dates=as.Date(medo@z$time))
                                      
                                      betti_dipole_daily <- data.frame(betti_dipole_index_daily=apply(as.array(medw),3, mean,na.rm=T)-apply(as.array(medo),3, mean,na.rm=T),dates=as.Date(medw@z$time))
                                      med_occ_sst_list[[i]]=sst_medw_daily
                                      med_ori_sst_list[[i]]=sst_medo_daily
                                      betti_dipole_list[[i]]= betti_dipole_daily                
}


saveRDS(IOD_sst_list,file="IOD_sst_list.rds")
saveRDS(betti_dipole_list,file="betti_dipole_list.rds")
saveRDS(med_occ_sst_list,file="med_occ_sst_list.rds")
saveRDS(med_ori_sst_list,file="med_ori_sst_list.rds")


index_IOD=do.call("rbind",IOD_sst_list)
index_betti_dipole=do.call("rbind",betti_dipole_list)
index_sst_medw=do.call("rbind",med_occ_sst_list)
index_sst_medo=do.call("rbind",med_ori_sst_list)

index_IOD_xts=xts(zoo(index_IOD$IOD_index_daily,index_IOD$dates))
index_betti_dipole_xts=xts(zoo(index_betti_dipole$betti_dipole_index_daily,index_betti_dipole$dates))
index_sst_medw_xts=xts(zoo(index_sst_medw$sst_medw_index_daily,index_sst_medw$dates))
index_sst_medo_xts=xts(zoo(index_sst_medo$sst_medo_index_daily,index_sst_medo$dates))

saveRDS(index_IOD_xts,"index_IOD_xts.rds")
saveRDS(index_betti_dipole_xts,"index_betti_dipole_xts.rds")
saveRDS(index_sst_medw_xts,"index_sst_medw_xts.rds")
saveRDS(index_sst_medo_xts,"index_sst_medo_xts.rds")

########################################################################################################################################################
IOD_sst_list=readRDS(file="IOD_sst_list.rds")
saveRDS(index_betti_dipole_xts,"index_betti_dipole_xts.rds")
saveRDS(index_sst_medw_xts,"index_sst_medw_xts.rds")
saveRDS(index_sst_medo_xts,"index_sst_medo_xts.rds")

# https://biologyforfun.wordpress.com/2014/05/05/importing-100-years-of-climate-change-into-r/
# ncep.reanalysis2.dailyavgs=xmlParse("http://www.esrl.noaa.gov/psd/thredds/wcs/Datasets/ncep.reanalysis2.dailyavgs/pressure/air.1979.nc?service=WCS&version=1.0.0&request=GetCapabilities")######

  
getOpenDapURLAsSpatialGrid = function(opendapURL,variableName,bboxInDegrees)

{  
  require("sp")
  require("ncdf")
  
  print(paste("Loading opendapURL",opendapURL)); 
   
  # Open the dataset  
   
   dataset = open.ncdf(opendapURL)    
   
   bbox=bboxInDegrees;  
  
   # Get lon and lat variables, which are the dimensions of depth. For this specific dataset they have the names lon and lat  
   
   G.x=get.var.ncdf(dataset,"lon")  
   
   G.y=get.var.ncdf(dataset,"lat")   
   
   # Make a selection of indices which fall in our subsetting window  
   # E.g. translate degrees to indices of arrays.  
   
   xindicesinwindow=which(G.x>bbox[1]&G.x<bbox[3]);  
   xmin=min(xindicesinwindow)  
   xmax=max(xindicesinwindow)  
   xcount=(xmax-xmin)+1; 
   
   # needs to be at least 1 
   
   yindicesinwindow=which(G.y>bbox[2]&G.y<bbox[4]);  
   ymin=min(yindicesinwindow)  
   ymax=max(yindicesinwindow)  
   ycount=(ymax-ymin)+1;
   
   # needs to be at least 1   
   
   print(paste("Indices:",xmin,ymin,xmax,ymax));
   
   # <== print bbox in indices   
   # Get the variable depth  
   
   G.z=get.var.ncdf(dataset, variableName,start=c(xmin,ymin), count=c(xcount,ycount));   
   
   # Transpose this dataset, sometimes X and Y are swapped  
   
   #G.z=t(G.z)   
   
   # At the beginning we loaded the complete lat and lon variables  
   # in order to find which indices belong in our subset window  
   # In order to create a spatialdatagrid frame, we need to make the lat and lon variables  
   # the same size as the requested matrix. E.g. The lat and lon (or y and x) needs to be subsetted: 
   
   G.sx = G.x[xmin:xmax]  
   
   G.sy = G.y[ymin:ymax]   
   
   # Optionally create dims with equal cellsizes  
   # This is sometimes needed because there can be small errors in the values of the x and y variables. 
   
   makeCellsizesEqual=TRUE  
   
   if(makeCellsizesEqual){   
                          # Make cellsizes equal for X dimension    
     
                          cellsizex=(G.sx[length(G.sx)]-G.sx[1])/(length(G.sx)-1)    
     
                          tempX=(((1:length(G.sx))-1))*cellsizex+G.sx[1]    
     
                          G.sx=tempX     
     
                          #   Make cellsizes equal for Y dimension    
     
                          cellsizey=(G.sy[length(G.sy)]-G.sy[1])/(length(G.sy)-1)    
     
                          tempY=(((1:length(G.sy))-1))*cellsizey+G.sy[1]   
     
                          G.sy=tempY  
   }   
   
   # We have now x, y, and z complete. 
   # In order to create a SpatialGridDataFrame  
   # We need to make the shape of all variables the same  
   # Create a matrix of X values  
   
   G.mx=rep(G.sx,dim(G.z)[2])  
   
   # Create a matrix field of Y values  
   
   G.my=(as.vector(t(matrix(rep(G.sy,dim(G.z)[1]),nrow=dim(G.z)[2],ncol=dim(G.z)[1]))))   
   
   # Make a dataframe of the X, Y and Z values  
   
   myspatialgrid=data.frame(topo=as.vector(G.z),lon=G.mx,lat=G.my)  
   
   # We have now gathered all information required to create a SpatialGridDataFrame object  
   # Assign X and Y coordinates  coordinates(myspatialgrid)=~lon+lat   
   # Make a gridded dataset, previousely the object was just a bunch of points with XY coodinates  
   
   gridded(myspatialgrid) = TRUE  
   
   fullgrid(myspatialgrid) = TRUE   
   
   # This can be converted to a SpatialGridDataFrame  
   
   myspatialgrid = as(myspatialgrid, "SpatialGridDataFrame")   
   
   # Optionally assign a projection string to this object  

   attributes(myspatialgrid)$proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs  <>")   
   
   myspatialgrid;

} 