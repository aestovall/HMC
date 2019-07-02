
library(maptools)
library(raster)
library(rgdal)
library(dplyr)
library(stringr)
library(viridis)
library(lidR)
library(rlas)

las_dir<-"las"
las_files<-list.files(las_dir, pattern = ".las", full.names = TRUE, recursive = TRUE)

epsg <- make_EPSG()
proj<-epsg %>% filter(code == 2251)

readLAS(las_files[i])

ctg <- catalog(las_files)
crs(ctg)<-as.character(proj$prj4)
plot(ctg, mapview = TRUE)

opt_output_files(ctg) <- paste(las_dir,"/normalized/{ORIGINALFILENAME}_NORM",sep = "")

library(parallel)
cores<-detectCores()
opt_cores(ctg)<-cores-2

myfun = function(cluster, ...)
{
  las = readLAS(cluster)
  if (is.empty(las)) return(NULL)
  
  DTM<-grid_terrain(las, res = 1, knnidw())
  las_norm <- lasnormalize(las, DTM)
  return(las_norm)
}

catalog_apply(ctg, myfun)