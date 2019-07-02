# library(RCurl) 
# 
# lasfiles<-read.table('las_files_hmc.txt', stringsAsFactors = FALSE)
# base<-"ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/Projects/USGS_LPC_MI_Marquette_2015_LAS_2018/las/tiled/"
# for(i in 1:length(lasfiles$V1)) download.file(lasfiles$V1[i], destfile = gsub(base,"",lasfiles$V1[i]), method = "wininet")

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

# myfun = function(cluster, ...)
# {
#   las = readLAS(cluster)
#   if (is.empty(las)) return(NULL)
#   
#   DTM<-grid_terrain(las, res = 5, knnidw())
#   las_norm <- lasnormalize(las, DTM)
#   chm <- grid_canopy(las_norm, 1, pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0,1)))
#   
#   return(chm)
# }

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

#All LiDAR point clouds normalized

#Clip plots from normalized point cloud

plot_dir<-"GIS/Woods Plot Locations"
plots<-read.csv(paste(plot_dir, "/HMC Plots lat-long.csv", sep = ""), 
                stringsAsFactors = FALSE)

las_dir<-"las/normalized"
ctg <- catalog(las_dir)
plot(ctg, mapview = TRUE)

library(rgdal)
d <- data.frame(lon=plots$longitude, lat=plots$latitude)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:4326") # WGS 84

d.proj <- spTransform(d, CRS(proj$prj4))
plot(d.proj, add = TRUE)
plots<-data.frame(plot=as.character(plots$name),
                  d.proj@coords, stringsAsFactors = FALSE)

opt_output_files(ctg) <- paste(las_dir,"/plots/{ID}",sep = "")
opt_cores(ctg)<-cores-1

# plots<-data.frame(as.character(plots$name), d.proj@coords)

lasclipCircle(ctg, plots$lon, plots$lat, radius = 58.530184)

# lasclipRectangle(ctg, plots$easting+1, plots$northing+1,plots$easting+41,plots$northing+41)

las_dir<-"las/normalized/plots"
las_files<-list.files(las_dir, pattern = "las")

lad_ls<-list()
i=1
for (i in 1:length(las_files)){
  las<-read.las(list.files(las_dir, pattern = "las", full.names = TRUE)[i])
  lad_ls[[i]]<-data.frame(ID = plots$plot[as.numeric(gsub(".las", "", las_files[i]))], LAD(las$Z, dz = 3, k = 0.9, z0 = 1.5))
}

lad_all<-do.call(rbind,lad_ls)
library(ggplot2)
library(viridis)

# lad_all$plot_name<-as.character(plots$plots.name)[lad_all$ID]

ggplot(lad_all[lad_all$lad<1,], aes(y = lad, x = z*0.3048, group = as.factor(ID), color = as.factor(ID))) + theme_bw()+
  geom_path(size = 0.5, alpha = 0.7) + 
  coord_flip() +
  scale_color_discrete() + facet_wrap(~ID)+
  theme(text = element_text(size = 8.0, colour = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylab( expression(PAVD ~ (m^{2} ~ m^{-2}))) + xlab( expression(Height ~ (m))) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position= "none")

ggsave("output/figures/PAVD_all_ALS.pdf", width = 3.75*1.5, height = 3.75*1, units = "in")

plots$plot[1:28]

profile_ls<-list()
i=1
for (i in 1:length(las_files)){
  las<-read.las(list.files(las_dir, pattern = "las", full.names = TRUE)[i])
  
  profile_ls[[i]]<-data.frame(ID = plots$plot[as.numeric(gsub(".las", "", las_files[i]))], 
                              X=las$X,Z=las$Z,Intensity=las$Intensity)
}

profile_all<-do.call(rbind,profile_ls)

ggplot(profile_all, aes(y = Z, x = X, color = Intensity)) + theme_bw()+
  geom_point() + 
  # coord_flip() +
  facet_wrap(~ID, scales = "free_x")+
  scale_color_viridis(option = "inferno")+
  theme(text = element_text(size = 8.0, colour = "black")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ylab( expression()) + xlab( expression(Height ~ (m))) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  theme(axis.title.y = element_text(vjust=1.5)) +
  theme(plot.title = element_text(vjust=2.5)) +
  theme(legend.position= "none")

ggsave("output/figures/all_ALS.pdf", width = 3.75*1.5*2, height = 3.75*1*5, units = "in")


# ggplot(lad_all[lad_all$lad<1&
#                  lad_all$plot_name=="7094_11"|
#                  lad_all$plot_name=="7091"|
#                  lad_all$plot_name=="7092"|
#                  lad_all$plot_name=="7093"|
#                  lad_all$plot_name=="7094_11"|
#                  lad_all$plot_name=="7094_11"|,], aes(y = lad, x = z*0.3048, group = as.factor(plot_name), color = as.factor(plot_name))) + theme_bw()+
#   geom_path(size = 0.5, alpha = 0.7) + 
#   coord_flip() +
#   scale_color_discrete() + facet_wrap(~plot_name)+
#   theme(text = element_text(size = 8.0, colour = "black")) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   ylab( expression(PAVD ~ (m^{2} ~ m^{-2}))) + xlab( expression(Height ~ (m))) +
#   theme(axis.title.x = element_text(vjust=-0.5)) +
#   theme(axis.title.y = element_text(vjust=1.5)) +
#   theme(plot.title = element_text(vjust=2.5)) +
#   theme(legend.position= "none")

# ggsave("output/figures/PAVD_all_ALS.pdf", width = 3.75*1.5, height = 3.75*1, units = "in")

las_dir<-"las/normalized/plots"
las_files<-list.files(las_dir, pattern = "las")

proj<-epsg %>% filter(code == 2251)
proj.new<-epsg %>% filter(code == 26916)

ctg<-catalog(las_dir)
crs(ctg)<-proj$prj4

opt_cores(ctg)<-cores-1

metrics_ls<-list()
i=1
for (i in 1:length(las_files)){
  las<-readLAS(list.files(las_dir, pattern = "las", full.names = TRUE)[i])
  crs(las)<-as.character(proj$prj4)
  las<-lastransform(las,epsg = 26916)
  las@data$Z<-las@data$Z*0.3048
  metrics_ls[[i]]<-data.frame(ID = gsub(".las", "", las_files[i]), 
                              lasmetrics(las, stdmetrics(X,Y,Z,Intensity,ReturnNumber,Classification)))
}

metrics_all<-do.call(rbind,metrics_ls)
# plot(metrics_all)

metrics_all<-metrics_all[,-7]
str(metrics_all)

metrics_all$ID<-as.character(plots$plots.name)[metrics_all$ID]

write.csv(metrics_all, "output/data/metrics.csv", row.names = FALSE)

M<-cor(metrics_all[,-1])
library(corrplot)
corrplot::corrplot(M, method="circle")

plot(metrics_all$zq95, metrics_all$zsd)


as.character(plots$plots.name)


