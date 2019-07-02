# Get plot-level LiDAR metrics

las_dir<-"las/normalized/plots"
las_files<-list.files(las_dir, pattern = "las")

proj<-epsg %>% filter(code == 2251)
proj.new<-epsg %>% filter(code == 26916)

ctg<-catalog(las_dir)
crs(ctg)<-proj$prj4

opt_cores(ctg)<-cores-1

metrics_ls<-list()

for (i in 1:length(las_files)){
  las<-readLAS(list.files(las_dir, pattern = "las", full.names = TRUE)[i])
  crs(las)<-as.character(proj$prj4)
  las<-lastransform(las,epsg = 26916)
  las@data$Z<-las@data$Z*0.3048
  metrics_ls[[i]]<-data.frame(ID = gsub(".las", "", las_files[i]), 
                              lasmetrics(las, stdmetrics(X,Y,Z,Intensity,ReturnNumber,Classification)))
}

metrics_all<-do.call(rbind,metrics_ls)
metrics_all<-metrics_all[,-7]
metrics_all$ID<-as.character(plots$plots.name)[metrics_all$ID]

write.csv(metrics_all, "output/data/metrics.csv", row.names = FALSE)

M<-cor(metrics_all[,-1])

library(corrplot)
corrplot::corrplot(M, method="circle")

plot(metrics_all$zq95, metrics_all$zsd)