# clear memory
rm(list=ls())

# importing libraries
library(dplR)
library(lattice)

# Getting Libraries
library(reshape)
library(car)
library(mgcv)
library(nlme)
#library(lme4)
library(splines)
library(MASS)
library(MuMIn)
library(ggplot2)
library(grid)

se <- function(x){
	sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


group.col <- read.csv("GroupColors.csv")
group.col <- group.col[order(group.col$Spp),]
summary(group.col)


q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))	

large.axes <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines")) 


large.axes2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines")) + theme(legend.position=c(0.2,0.8), legend.text=element_text(size=20), legend.title=element_text(size=20), legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + theme(strip.text=element_text(size=rel(1.25), face="bold"))



#################################################################################################
# estab <- read.csv("Establishment_SpeciesGroup_Plot_Climate_1km.csv")
#################################################################################################
#################################################################################################
estab.smooth <- read.csv("Establishment_SpeciesGroup_Climate_1km_smooth_1900-2013_RF.csv")
summary(estab.smooth)



group.col2 <- group.col[group.col$Spp.Group %in% unique(estab.smooth$Spp.Group),]
group.col2
colors2 <- group.col2$Color

# Looking at plot-level Release & Establishment
ggplot(data=estab.smooth[estab.smooth$Site=="BLD",]) + 
	geom_histogram(aes(x=Year, weight=p.Major/8), binwidth=1, fill="gray75") + 	
	geom_line(aes(x=Year, y=p.smooth, color=Spp.Group), size=1) + 	
	facet_grid(PlotID ~ .) + scale_color_manual(values=as.vector(group.col2$Color)) + 
	scale_y_continuous(name="Percent Establishment", breaks=c(0,0.5)) + 
	scale_x_continuous(name="Establishment Date") + 
	large.axes2 +
	guides(color=F) # + 
	# theme(legend.position=c(0.93,0.245), legend.text=element_text(size=14), legend.title=element_text(size=16))

# Minor Disturbance
pdf("Release_Major_smoothed_BlD.pdf")
ggplot(data=estab.smooth[estab.smooth$Site=="BLD",]) + 
	geom_histogram(aes(x=Year, weight=p.Major/8), binwidth=1, fill="gray75") + 	
	# geom_line(aes(x=Year, y=p.smooth, color=Spp.Group), size=1) + 	
	facet_grid(PlotID ~ .) + scale_color_manual(values=as.vector(group.col2$Color)) + 
	scale_y_continuous(name="Percent Trees Showing Release", breaks=c(0,0.5)) + 
	scale_x_continuous(name="Establishment Date") + 
	large.axes2 
dev.off()

# Major Disturbance
pdf("Release_Minor_smoothed_BLD.pdf")
ggplot(data=estab.smooth[estab.smooth$Site=="BLD",]) + 
	geom_histogram(aes(x=Year, weight=p.Minor/8), binwidth=1, fill="gray75") + 	
	# geom_line(aes(x=Year, y=p.smooth, color=Spp.Group), size=1) + 	
	facet_grid(PlotID ~ .) + scale_color_manual(values=as.vector(group.col2$Color)) + 
	scale_y_continuous(name="Percent Trees Showing Release", breaks=c(0,0.15)) + 
	scale_x_continuous(name="Establishment Date") + 
	large.axes2 
dev.off()

############################
names(estab.smooth)
estab.site <- aggregate(estab.smooth[,5:ncol(estab.smooth)], by=list(estab.smooth$Site, estab.smooth$Spp.Group, estab.smooth$Year), FUN=mean, na.rm=T)
names(estab.site) <- c("Site", "Spp.Group", "Year", names(estab.site[,4:ncol(estab.site)]))
estab.site$Site.NS <- recode(estab.site$Site, "'HIK'='1'; 'HNK'='2';'BLD'='3'; 'MOR'='4'; 'FLT'='5'; 'IRN'='6'")
levels(estab.site$Site.NS) <- c("HIK", "HNK", "BLD", "MOR", "FLT", "IRN")
summary(estab.site)
levels(estab.site$Site.NS)

############################
group.col2 <- group.col[group.col$Spp.Group %in% unique(estab.site$Spp.Group),]
group.col2
colors2 <- group.col2$Color


############################
# Plotting mean establishment per site with Time
############################
pdf("Establishment_1900-2013.pdf", width=11, height=6)
ggplot(data=estab.site) + 
	# geom_histogram(aes(x=Year, weight=p.Major*.3), binwidth=1, fill="gray75") + 	
	geom_line(aes(x=Year, y=p.smooth, color=Spp.Group), size=1) + 	
	facet_grid(Site.NS ~ .) + scale_color_manual(values=as.vector(group.col2$Color), name="Species") + 
	scale_y_continuous(name="Percent Establishment", breaks=c(0,0.2,0.4)) + 
	scale_x_continuous(name="Year", breaks=c(1920, 1950, 1980, 2010)) + 
	large.axes2 + 
	theme(legend.position=c(0.93,0.245), legend.text=element_text(size=14), legend.title=element_text(size=16))
dev.off()



pdf("Establishment_Release_Major_1900-2013.pdf", width=11, height=6)
ggplot(data=estab.site) + 
	geom_histogram(aes(x=Year, weight=p.Major*.3), binwidth=1, fill="gray75") + 	
	geom_line(aes(x=Year, y=p.smooth, color=Spp.Group), size=1) + 	
	facet_grid(Site.NS ~ .) + scale_color_manual(values=as.vector(group.col2$Color), name="Species") + 
	scale_y_continuous(name="Percent Establishment", breaks=c(0,0.2,0.4)) + 
	scale_x_continuous(name="Establishment Date") + 
	large.axes2 + 
	theme(legend.position=c(0.93,0.245), legend.text=element_text(size=10), legend.title=element_text(size=12))
dev.off()

pdf("Establishment_Release_Minor_1900-2013.pdf", width=11, height=6)
ggplot(data=estab.site) + 
	geom_histogram(aes(x=Year, weight=p.Minor*.3), binwidth=1, fill="gray75") + 	
	geom_line(aes(x=Year, y=p.smooth, color=Spp.Group), size=1) + 	
	facet_grid(Site.NS ~ .) + scale_color_manual(values=as.vector(group.col2$Color), name="Species") + 
	scale_y_continuous(name="Percent Establishment", breaks=c(0,0.2,0.4)) + 
	scale_x_continuous(name="Establishment Date") + 
	large.axes2 + 
	theme(legend.position=c(0.93,0.8), legend.text=element_text(size=12), legend.title=element_text(size=14))
dev.off()



summary(estab.site)


############################
# Establishment vs. Climate
############################

focal.species <- c("ACRU", "BELE", "NYSY", "QUPR", "QURU")
focal.colors <- c("purple", "blue", "green3", "orange", "red")

ggplot(data=estab.site[estab.site$Spp.Group %in% focal.species,]) +
	geom_point(aes(x=Tmean.yr.smooth, y=p.smooth)) +
	facet_grid(Spp.Group ~ .) +
	scale_y_continuous(name="Percent Establishment", breaks=c(0,0.2,0.4)) +
	scale_x_continuous(name="Tmean Year (C)") +
	large.axes2


###################
# Histograms
pdf("Establishment_Species_Temp.pdf")
ggplot(data=estab.site[estab.site$Spp.Group %in% focal.species,]) +
	geom_histogram(aes(x=Tmean.yr.smooth, weight=p.smooth, fill=Spp.Group), binwidth=0.1) +
	facet_grid(Spp.Group ~ .) +
	scale_y_continuous(name="Percent Establishment", breaks=c(0,2,4)) +
	scale_x_continuous(name="Tmean Year (C)") +
	scale_fill_manual(values=focal.colors) + 
 	large.axes2 + guides(fill=F)
dev.off()	


pdf("Establishment_Species_Precip.pdf")
ggplot(data=estab.site[estab.site$Spp.Group %in% focal.species,]) +
	geom_histogram(aes(x=Precip.yr.smooth, weight=p.smooth, fill=Spp.Group), binwidth=2) +
	facet_grid(Spp.Group ~ .) +
	scale_y_continuous(name="Percent Establishment", breaks=c(0,2,4)) +
	scale_x_continuous(name="Precip Year (mm)") +
	scale_fill_manual(values=focal.colors) + 
 	large.axes2 + guides(fill=F)
dev.off()	



###################
# Scatter plots

# All Together
pdf("Establishment_Species_Temp_Precip.pdf")
ggplot(data=estab.site[estab.site$Spp.Group %in% focal.species,]) +
	geom_jitter(aes(x=Tmean.yr.smooth, y=Precip.yr.smooth, size=p.smooth, color=Spp.Group), position=position_jitter(width=0.1, height=2), alpha=0.7) +
	# facet_grid(Spp.Group ~ .) +
	scale_y_continuous(name="Precip Year (mm)") +
	scale_x_continuous(name="Tmean Year (C)") +
	scale_color_manual(values=focal.colors) + 
 	large.axes2 + guides(size=F) + 
 	theme(legend.position=c(0.8,0.8))
dev.off()

ggplot(data=estab.site[estab.site$Spp.Group %in% focal.species,]) +
	geom_jitter(aes(x=Tmean.yr.smooth, y=Precip.yr.smooth, size=p.smooth, color=Spp.Group), position=position_jitter(width=0.1, height=2), alpha=0.7) +
	facet_grid(. ~ Site.NS) +
	scale_y_continuous(name="Precip Year (mm)", breaks=c(75,100)) +
	scale_x_continuous(name="Tmean Year (C)", breaks=c(8,10)) +
	scale_color_manual(values=focal.colors) + 
 	large.axes2 + guides(size=F) + 
 	theme(legend.position=c(0.2,0.8))

pdf("Establishment_Species_Temp_Precip_Facet_Site.pdf")
ggplot(data=estab.site[estab.site$Spp.Group %in% focal.species,]) +
	geom_jitter(aes(y=Tmean.yr.smooth, x=Precip.yr.smooth, size=p.smooth, color=Spp.Group), position=position_jitter(width=0.1, height=2), alpha=0.7) +
	facet_grid(Site.NS ~ .) +
	scale_x_continuous(name="Precip Year (mm)") +
	scale_y_continuous(name="Tmean Year (C)", limit=c(5,13), breaks=c(6,9,12)) +
	scale_color_manual(values=focal.colors) + 
 	large.axes2 + 
 	theme(legend.position=c(0.8,0.7))
dev.off()

# Broken into facets by Species
pdf("Establishment_Species_Temp_Precip_Facet_Species.pdf")
ggplot(data=estab.site[estab.site$Spp.Group %in% focal.species,]) +
	geom_point(aes(x=Tmean.yr.smooth, y=Precip.yr.smooth, size=p.smooth, color=Spp.Group)) +
	facet_grid(Spp.Group ~ .) +
	scale_y_continuous(name="Precip Year (mm)", limits=c(60,140), breaks=c(60,90,120)) +
	scale_x_continuous(name="Tmean Year (C)") +
	scale_color_manual(values=focal.colors) + 
 	large.axes2 + guides(color=F, size=F)
dev.off()


############################
# Site Disturbances (Ugly formatting)
############################

pdf("Release_Major_HIK.pdf")
ggplot(data=release.smooth[release.smooth$Site=="HIK",]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(PlotID ~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(name="Percentage Trees Showing Release")
dev.off()

pdf("Release_Major_HNK.pdf")
ggplot(data=release.smooth[release.smooth$Site=="HNK",]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(PlotID ~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(name="Percentage Trees Showing Release")
dev.off()

pdf("Release_Major_BLD.pdf")
ggplot(data=release.smooth[release.smooth$Site=="BLD",]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(PlotID ~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(name="Percentage Trees Showing Release")
dev.off()

pdf("Release_Major_MOR.pdf")
ggplot(data=release.smooth[release.smooth$Site=="MOR",]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(PlotID ~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(name="Percentage Trees Showing Release")
dev.off()

pdf("Release_Major_FLT.pdf")
ggplot(data=release.smooth[release.smooth$Site=="FLT",]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(PlotID ~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(name="Percentage Trees Showing Release")
dev.off()

pdf("Release_Major_IRN.pdf")
ggplot(data=release.smooth[release.smooth$Site=="IRN",]) + geom_histogram(aes(x=Year, weight=p.Major), binwidth=1) + facet_grid(PlotID ~ .) + large.axes2 + scale_x_continuous(breaks=c(1800,1850,1900,1950,2000)) +  scale_y_continuous(name="Percentage Trees Showing Release")
dev.off()







#################################################################################################
# Looking at climate of establishments
#################################################################################################
estab.smooth <- read.csv("Establishment_SpeciesGroup_Climate_1km_smooth_1900-2013_RF.csv")
summary(estab.smooth)

plot.env <- read.csv("Plot_EnvironmentCharacteristics_IVweights.csv")
summary(plot.env)

########################################
# Establishment vs. all time
########################################
pdf("Establishment_Climate_QURU.pdf")
ggplot() +
	geom_point(data=estab.smooth, aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="gray50", size=4, shape=19) +
	geom_point(data=estab.smooth[estab.smooth$Spp=="QURU" & estab.smooth$p.smooth>0,], aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="black", size=2, shape=19) +
	scale_x_continuous(name="Growing Season Precip (mm)") +
	scale_y_continuous(name="Growing Season Temp (C)") +
	large.axes
dev.off()

pdf("Establishment_Climate_QUPR.pdf")
ggplot() +
	geom_point(data=estab.smooth, aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="gray50", size=4, shape=19) +
	geom_point(data=estab.smooth[estab.smooth$Spp=="QUPR" & estab.smooth$p.smooth>0,], aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="black", size=2, shape=19) +
	scale_x_continuous(name="Growing Season Precip (mm)") +
	scale_y_continuous(name="Growing Season Temp (C)") +
	large.axes
dev.off()

pdf("Establishment_Climate_NYSY.pdf")
ggplot() +
	geom_point(data=estab.smooth, aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="gray50", size=4, shape=19) +
	geom_point(data=estab.smooth[estab.smooth$Spp=="NYSY" & estab.smooth$p.smooth>0,], aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="black", size=2, shape=19) +
	scale_x_continuous(name="Growing Season Precip (mm)") +
	scale_y_continuous(name="Growing Season Temp (C)") +
	large.axes
dev.off()


pdf("Establishment_Climate_BELE.pdf")
ggplot() +
	geom_point(data=estab.smooth, aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="gray50", size=4, shape=19) +
	geom_point(data=estab.smooth[estab.smooth$Spp=="BELE" & estab.smooth$p.smooth>0,], aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="black", size=2, shape=19) +
	scale_x_continuous(name="Growing Season Precip (mm)") +
	scale_y_continuous(name="Growing Season Temp (C)") +
	large.axes
dev.off()


pdf("Establishment_Climate_ACRU.pdf")
ggplot() +
	geom_point(data=estab.smooth, aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="gray50", size=4, shape=19) +
	geom_point(data=estab.smooth[estab.smooth$Spp=="ACRU" & estab.smooth$p.smooth>0,], aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="black", size=2, shape=19) +
	scale_x_continuous(name="Growing Season Precip (mm)") +
	scale_y_continuous(name="Growing Season Temp (C)") +
	large.axes
dev.off()




########################################
# Establishment vs. 30yr Norms
########################################
pdf("Establishment_Climate_QURU.csv")
ggplot() +
	geom_point(data=plot.env, aes(x=Precip.M_S.norm, y=Tmean.M_S.norm), color="gray50", size=4, shape=19) +
	geom_point(data=estab.smooth[estab.smooth$Spp=="QURU" & estab.smooth$p.smooth>0,], aes(x=Precip.M_S.smooth, y=Tmean.M_S.smooth), color="black", size=2, shape=19) +
	scale_x_continuous(name="Growing Season Precip (mm)") +
	scale_y_continuous(name="Growing Season Temp (C)") +
	large.axes
dev.off()

quru.t.norm <- t.test(plot.env$Tmean.M_S.norm, estab.smooth[estab.smooth$Spp=="QURU" & estab.smooth$p.smooth>0, "Tmean.M_S.smooth"])
quru.t.norm

quru.t.yrs <- t.test(estab.smooth$Tmean.M_S.smooth, estab.smooth[estab.smooth$Spp=="QURU" & estab.smooth$p.smooth>0, "Tmean.M_S.smooth"])
quru.t.yrs