#Create PAVD Profiles

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
