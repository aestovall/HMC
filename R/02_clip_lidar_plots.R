#Clip out the plots

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

lasclipCircle(ctg, plots$lon, plots$lat, radius = 58.530184)