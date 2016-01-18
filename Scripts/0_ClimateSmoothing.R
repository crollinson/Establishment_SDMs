setwd("~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Establishment_Modeling")

library(reshape2)
library(ggplot2)
library(grid)
library(raster)

###########################
# 1 km climate - Reformatting
###########################
climate.1km <- read.csv("Data/raw_inputs/CARCA_Plots_Climate_1km_Buffer.csv") # Generated 
climate.1km$Year <- as.factor(climate.1km$Year)
summary(climate.1km)

temp.melt <- melt(climate.1km[,c("PlotID", "Year", "Month", "Tavg")])
summary(temp.melt)

precip.melt <- melt(climate.1km[,c("PlotID", "Year", "Month", "Precip.PRISM")])
summary(precip.melt)

# Separating out Monthly Temperatures
temp.1km <- dcast(temp.melt, PlotID + Year ~ Month)
names(temp.1km) <- c("PlotID", "Year", paste("Tmean", names(temp.1km[,3:ncol(temp.1km)]), sep="."))
summary(temp.1km)

# Separating out Monthly Precip
precip.1km <- dcast(precip.melt, PlotID + Year ~ Month)
names(precip.1km) <- c("PlotID", "Year", paste("Precip", names(precip.1km[,3:ncol(precip.1km)]), sep="."))
summary(precip.1km)

# Making 1 "wide" formate climate
climate.1km.2 <- merge(temp.1km, precip.1km, all.x=T, all.y=T)
climate.1km.2$Tmean.yr <- rowMeans(climate.1km.2[,substr(names(climate.1km.2),1,5)=="Tmean"], na.rm=F)
climate.1km.2$Precip.yr <- rowMeans(climate.1km.2[,substr(names(climate.1km.2),1,6)=="Precip"], na.rm=F)
climate.1km.2$Tmean.JJA <- rowMeans(climate.1km.2[,c("Tmean.X06", "Tmean.X07", "Tmean.X08")], na.rm=F)
climate.1km.2$Tmean.MAM <- rowMeans(climate.1km.2[,c("Tmean.X03", "Tmean.X04", "Tmean.X05")], na.rm=F)
climate.1km.2$Precip.JJA <- rowMeans(climate.1km.2[,c("Precip.X06", "Precip.X07", "Precip.X08")], na.rm=F)
climate.1km.2$Precip.MAM <- rowMeans(climate.1km.2[,c("Precip.X03", "Precip.X04", "Precip.X05")], na.rm=F)
climate.1km.2$Tmean.JJA <- rowMeans(climate.1km.2[,c("Tmean.X06", "Tmean.X07", "Tmean.X08")], na.rm=F)
climate.1km.2$Tmean.M_S <- rowMeans(climate.1km.2[,c("Tmean.X05", "Tmean.X06", "Tmean.X07", "Tmean.X08", "Tmean.X09")], na.rm=F)
climate.1km.2$Precip.M_S <- rowMeans(climate.1km.2[,c("Precip.X05", "Precip.X06", "Precip.X07", "Precip.X08", "Precip.X09")], na.rm=F)
summary(climate.1km.2)

write.csv(climate.1km.2, "Data/processed_inputs/CARCA_Plots_Climate_1km_Buffer_Wide.csv", row.names=F)


################################################################################################
# Smoothing
climate.1km.2 <- read.csv("Data/processed_inputs/CARCA_Plots_Climate_1km_Buffer_Wide.csv")
# climate.1km.2$Year <- as.numeric(paste(climate.1km.2$Year))

climate.1km.2 <- climate.1km.2[complete.cases(climate.1km.2),]
summary(climate.1km.2)

climate.smooth <- climate.1km.2[climate.1km.2$Year>=(min(climate.1km.2$Year)+5) & climate.1km.2$Year<=(max(climate.1km.2$Year)-5),c("PlotID", "Year")]
dim(climate.smooth)
dim(climate.1km.2)

y <- 2005
p <- "BLDA1"
for(p in unique(climate.smooth$PlotID)){
	data.plot <- climate.1km.2[climate.1km.2$PlotID==p,]
	for(y in min(climate.smooth$Year):max(climate.smooth$Year)){
	# Temperature
	climate.smooth[climate.smooth$PlotID==p & climate.smooth$Year==y,"Tmean.yr.smooth"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Tmean.yr"], na.rm=F)
	climate.smooth[climate.smooth$PlotID==p & climate.smooth$Year==y,"Tmean.MAM.smooth"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Tmean.MAM"], na.rm=F)
	climate.smooth[climate.smooth$PlotID==p & climate.smooth$Year==y,"Tmean.JJA.smooth"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Tmean.JJA"], na.rm=F)
	climate.smooth[climate.smooth$PlotID==p & climate.smooth$Year==y,"Tmean.M_S.smooth"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Tmean.M_S"], na.rm=F)



	# Precip
	climate.smooth[climate.smooth$PlotID==p & climate.smooth$Year==y,"Precip.yr.smooth"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Precip.yr"], na.rm=F)
	climate.smooth[climate.smooth$PlotID==p & climate.smooth$Year==y,"Precip.MAM.smooth"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Precip.MAM"], na.rm=F)
	climate.smooth[climate.smooth$PlotID==p & climate.smooth$Year==y,"Precip.JJA.smooth"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Precip.JJA"], na.rm=F)
	climate.smooth[climate.smooth$PlotID==p & climate.smooth$Year==y,"Precip.M_S.smooth"] <- mean(data.plot[data.plot$Year>=(y-5) & data.plot$Year<=(y+5),"Precip.M_S"], na.rm=F)
}
}
summary(climate.smooth)
write.csv(climate.smooth, "Data/processed_inputs/CARCA_Plots_Climate_1km_Smooth.csv", row.names=F)


climate.smooth[climate.smooth$Year==2005,]


################################################################################################
# Plotting climate space for establishment time frames
################################################################################################
large.axes2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines")) + theme(legend.position=c(0.2,0.8), legend.text=element_text(size=20), legend.title=element_text(size=20), legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + theme(strip.text=element_text(size=rel(1.25), face="bold"))


climate.smooth <- read.csv("Data/processed_inputs/CARCA_Plots_Climate_1km_Smooth.csv")
summary(climate.smooth)

climate.smooth$Tmean.yr <- round(climate.smooth$Tmean.yr.smooth, digits=1)
climate.smooth$Precip.yr <- round(climate.smooth$Precip.yr.smooth, digits=-0.5)
summary(climate.smooth)

climate2 <- aggregate(climate.smooth[,c("Tmean.yr")], by=list(climate.smooth$Tmean.yr, climate.smooth$Precip.yr), FUN=length)
names(climate2) <- c("Tmean.yr", "Precip.yr", "Frequency")
summary(climate2)

pdf("Figures/Background/ClimateSpace_Establishment.pdf")
ggplot(data=climate2) + large.axes2 + 
	geom_tile(aes(x=Precip.yr, y=Tmean.yr, fill=Frequency)) +
	scale_x_continuous(name="Mean Annual Precip (mm)") +
	scale_y_continuous(name="Mean Annual Temperature (C)") +
	scale_fill_gradientn(colours=bpy.colors(200)) +
	theme(legend.position=c(0.9,0.85), legend.text=element_text(size=rel(1.5)), legend.title=element_text(size=rel(1.75))) + guides(fill=guide_colorbar(barwidth=2, barheight=5))
dev.off()

