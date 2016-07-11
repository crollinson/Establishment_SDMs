# Figures
large.axes2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines")) + theme(legend.position=c(0.2,0.8), legend.text=element_text(size=20), legend.title=element_text(size=20), legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + theme(strip.text=element_text(size=rel(1.25), face="bold"))


png("../Establishment Manuscript/GEB_Submission1/Figure1_Establishment_Time.png", res=180, units="in", width=11, height=6)
ggplot(data=estab.site) + 
  geom_histogram(aes(x=Year, weight=p.Major*.3), binwidth=1, fill="gray75") + 	
  geom_line(aes(x=Year, y=p.smooth, color=Spp.Group), size=1) + 	
  facet_grid(Site.NS ~ .) + scale_color_manual(values=as.vector(group.col2$Color), name="Species") + 
  scale_y_continuous(name="Percent Establishment", breaks=c(0,0.2,0.4)) + 
  scale_x_continuous(name="Establishment Date", expand=c(0,0)) + 
  large.axes2 + 
  theme(legend.position=c(0.93,0.245), legend.text=element_text(size=10), legend.title=element_text(size=12))
dev.off()