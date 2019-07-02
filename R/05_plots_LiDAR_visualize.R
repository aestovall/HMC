#Visualize plot-level LiDAR data

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
