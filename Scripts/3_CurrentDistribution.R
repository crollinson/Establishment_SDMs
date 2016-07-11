setwd("~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Establishment_Modeling")

#################################################################
# Setting up parameterizations of current forest condition to be compared with establishment models
#
#################################################################
rm(list=ls())

# Libraries
library(reshape2)
library(ggplot2)
library(grid)


large.axes2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines")) + theme(legend.position=c(0.2,0.8), legend.text=element_text(size=20), legend.title=element_text(size=20), legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + theme(strip.text=element_text(size=rel(1.25), face="bold"))


#######################################
# Data Sets
#######################################
spp.list <- read.csv("Data/raw_inputs/SppList.csv", na.strings="")
summary(spp.list)

tree.data <- read.csv("Data/raw_inputs/TreeData.csv", na.strings=c("", "*", "#DIV/0!"))
summary(tree.data)

estab.data <- read.csv("Data/raw_inputs/Establishment_AllCores.csv")
summary(estab.data)

estab.smooth <- read.csv("Data/model_inputs/Establishment_SpeciesGroup_Climate_1km_smooth_1900-2013_RF.csv")
summary(estab.smooth)

climate.1km <- read.csv("Data/processed_inputs/CARCA_Plots_Climate_1km_Buffer_Wide.csv")
summary(climate.1km) 

soils <- read.csv("Data/raw_inputs/CARCA_Plots_SoilsInfo_Final.csv")
summary(soils)

topo <- read.csv("Data/raw_inputs/CARCA_Plots_TopoInputs_1km_Buffer.csv")
summary(topo)

plot.data <- read.csv("Data/raw_inputs/PlotData.csv")
names(plot.data) <- c("PlotID", "Site.Name", "Site", names(plot.data[,4:ncol(plot.data)]))
summary(plot.data) 

release.time <- read.csv("Data/processed_inputs/ReleaseEvents_Plots_Smooth_TimeElapsed.csv")
summary(release.time)

####################################################################################
# Aggregating to match current species IV
####################################################################################
summary(tree.data)
summary(spp.list)
tree.data2 <- merge(tree.data, spp.list[,c("Spp", "Spp.Group")])
tree.data2$Site <- as.factor(substr(tree.data2$PlotID,1,3))
tree.data2 <- tree.data2[!substr(tree.data2$PlotID,1,3)=="BLU" & !tree.data2$Trans=="D" & tree.data2$Live=="LIVE" & tree.data2$DBH>=5,]
summary(tree.data2)
length(unique(tree.data2$PlotID))


spp.data <- aggregate(tree.data2[,c("IV.avg", "BA.m2ha", "Density.ha")], by=list(tree.data2$PlotID, tree.data2$Spp), FUN=sum)
names(spp.data) <- c("PlotID", "Spp", names(spp.data[,3:ncol(spp.data)]))
summary(spp.data)

spp.melt <- melt(spp.data[,c("PlotID", "Spp", "IV.avg")])
summary(spp.melt)

spp.cast <- dcast(spp.melt, PlotID ~ Spp)
spp.cast <- spp.cast[order(spp.cast$PlotID),]
spp.cast[is.na(spp.cast)] <- 0
summary(spp.cast)
write.csv(spp.cast, "Data/model_inputs/SpeciesMatrix_Final.csv", row.names=F)

spp.data <- stack(spp.cast[,2:ncol(spp.cast)])
names(spp.data) <- c("IV.avg", "Spp")
spp.data$PlotID <- spp.cast$PlotID
summary(spp.data)



####################################################################################
# Aggregating to match current species IV
####################################################################################
#######################################
# Matching climate & disturb with individual tree establishment
#######################################
dim(estab.data); dim(estab.smooth)
names(estab.data)
names(estab.smooth)

# subsetting just the data I want
estab.data2 <- estab.data[,c("TreeID", "Spp", "pith.use")]
names(estab.data2) <- c("TreeID", "Spp", "Year")
summary(estab.data2)

# estab.data2

summary(spp.list)
# Adding in the Spp Group
estab.data2 <- merge(estab.data2, spp.list[,c("Spp", "Spp.Group")], all.x=T, all.y=F)
estab.data2$PlotID <- as.factor(substr(estab.data2$TreeID, 1, 5))
summary(estab.data2)
dim(estab.data2); dim(estab.data)


summary(tree.data); dim(tree.data)
# Adding in some Tree Info; mostly IV.avg, which will be used to weight regen info
estab.data3 <- merge(estab.data2, tree.data[,c("TreeID", "PlotID", "Canopy", "Live", "DBH", "IV.avg", "BA.m2ha", "Density.ha")], all.x=T, all.y=F)
summary(estab.data3)
dim(estab.data3); dim(estab.data2)

estab.data4 <- aggregate(estab.data3[,c("IV.avg","BA.m2ha", "Density.ha")], by=list(estab.data3$PlotID, estab.data3$Spp, estab.data3$Year), FUN=sum, na.rm=T)
names(estab.data4) <- c("PlotID", "Spp", "Year", names(estab.data4[,4:ncol(estab.data4)]))
summary(estab.data4)
dim(estab.data4); dim(estab.data3)



plot.smooth <- aggregate(estab.smooth[,c("Tmean.yr.smooth", "Tmean.JFM.smooth", "Tmean.MAM.smooth", "Tmean.JJA.smooth", "Tmean.SON.smooth", "Precip.yr.smooth", "Precip.JFM.smooth", "Precip.MAM.smooth", "Precip.JJA.smooth", "Precip.SON.smooth", "Release.Minor", "Release.Major", "p.Minor", "p.Major", "time.Minor", "peak.Minor.mag", "peak.Minor.ext", "time.Major", "peak.Major.mag", "peak.Major.ext")], by=list(estab.smooth$PlotID, estab.smooth$Year), FUN=mean, na.rm=T)
names(plot.smooth) <- c("PlotID", "Year", names(plot.smooth[,3:ncol(plot.smooth)]))
# plot.smooth$Year <- as.factor(plot.smooth$Year)
summary(plot.smooth)
length(unique(plot.smooth$PlotID))
length(unique(estab.data2$PlotID))

unique(estab.data4$PlotID)
unique(climate.1km$PlotID)

summary(estab.data4$PlotID)
summary(climate.1km$PlotID)


summary(climate.1km)
summary(estab.data4)

# Getting rid of Years for which there is no climate data 
estab.data5 <- estab.data4[estab.data4$Year %in% plot.smooth$Year,]
summary(estab.data5)
estab.data5[estab.data5$PlotID=="BLDC1" & estab.data5$Spp=="QURU","IV.avg"]

# Aggregating to get the total IV of trees that were dated
spp.IV.plot <- aggregate(estab.data5[,c("IV.avg","BA.m2ha", "Density.ha")], by=list(estab.data5$PlotID, estab.data5$Spp), FUN=sum)
names(spp.IV.plot) <- c("PlotID", "Spp", names(spp.IV.plot[,3:ncol(spp.IV.plot)]))
summary(spp.IV.plot)
dim(spp.IV.plot); length(unique(spp.IV.plot$PlotID))
spp.IV.plot[spp.IV.plot$PlotID=="BLDC1" & spp.IV.plot$Spp.Grou=="QURU",]


estab6 <- merge(estab.data5, plot.smooth, all.x=T, all.y=F)
summary(estab6)
dim(estab6); dim(estab.data5); dim(estab.data4)


#######################################
# Aggregate to match modern data -- weighted by IV
#######################################
summary(estab6)
summary(spp.IV.plot)

estab.plot <- spp.IV.plot[,c("PlotID", "Spp")]
summary(estab.plot)


for(p in unique(estab.plot$PlotID)){
for(s in unique(estab.plot$Spp)){
	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"IV.avg.check"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"IV.avg.dated"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"])

	############
	# Temperature
	############
	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Tmean.yr"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Tmean.yr.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Tmean.MS"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Tmean.MS.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])
	
  estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Tmean.JFM"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Tmean.JFM.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Tmean.MAM"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Tmean.MAM.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Tmean.JJA"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Tmean.JJA.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Tmean.SON"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Tmean.SON.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	############
	# Precipitation
	############
	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Precip.yr"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Precip.yr.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

  estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Precip.MS"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Precip.MS.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])
	
	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Precip.JFM"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Precip.JFM.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Precip.MAM"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Precip.MAM.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Precip.JJA"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Precip.JJA.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"Precip.SON"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "Precip.SON.smooth"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	############
	# Release
	############
	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"time.Minor"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "time.Minor"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"peak.Minor.mag"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "peak.Minor.mag"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"peak.Minor.ext"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "peak.Minor.ext"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"time.Major"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "time.Major"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"peak.Major.mag"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "peak.Major.mag"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])

	estab.plot[estab.plot$PlotID==p & estab.plot$Spp==s,"peak.Major.ext"] <- sum(estab6[estab6$PlotID==p & estab6$Spp==s, "peak.Major.ext"] * estab6[estab6$PlotID==p & estab6$Spp==s, "IV.avg"]/spp.IV.plot[spp.IV.plot$PlotID==p & spp.IV.plot$Spp==s,"IV.avg"])
	}	
	}

summary(estab.plot)

#######################################
# Merging in static plot info
#######################################
summary(estab.plot)
summary(spp.data)

estab.plot1 <- merge(estab.plot, spp.data, all.x=T, all.y=T)
summary(estab.plot1)
dim(estab.plot1);dim(estab.plot); dim(spp.data)


# Adding in soil
estab.plot2 <- merge(estab.plot1, soils, all.x=T)
summary(estab.plot2)
dim(estab.plot2); dim(estab.plot1)

# Adding in Topo
estab.plot3 <- merge(estab.plot2, topo[,c("PlotID", "elev", "flow", "TPI", "slope", "aspect", "sol.yr")], all.x=T)
summary(estab.plot3)
dim(estab.plot3); dim(estab.plot2)

# Adding in Plot Data
estab.plot4 <- merge(estab.plot3, plot.data[,c("PlotID", "live.ba", "dead.ba", "live.dens")], all.x=T)
summary(estab.plot4)
dim(estab.plot4); dim(estab.plot3)


write.csv(estab.plot4, "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_withNA.csv", row.names=F)

# Removing NA for RF analysis 
estab.plot4 <- estab.plot4[complete.cases(estab.plot4),]

write.csv(estab.plot4, "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF.csv", row.names=F)

write.csv(estab.plot4[estab.plot4$Spp=="QURU",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_QURU.csv", row.names=F)
write.csv(estab.plot4[estab.plot4$Spp=="QUPR",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_QUPR.csv", row.names=F)
write.csv(estab.plot4[estab.plot4$Spp=="NYSY",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NYSY.csv", row.names=F)
write.csv(estab.plot4[estab.plot4$Spp=="BELE",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_BELE.csv", row.names=F)
write.csv(estab.plot4[estab.plot4$Spp=="ACRU",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_ACRU.csv", row.names=F)




####################################################################################
# Dealing with only current trees & climate
####################################################################################

# Subsetting the climate time frame I want to use for nomrals
climate.1km.80.10 <- climate.1km[climate.1km$Year>=1981 & climate.1km$Year<=2010, ]
summary(climate.1km.80.10)
# climate.1km.80.10[climate.1km.80.10$Year==1985,]

# Producing climate normals
climate.norms <- aggregate(climate.1km.80.10[,c("Tmean.yr", "Tmean.MS", "Tmean.JFM", "Tmean.MAM", "Tmean.JJA", "Tmean.SON", "Precip.yr", "Precip.MS", "Precip.JFM", "Precip.MAM", "Precip.JJA", "Precip.SON")], by=list(climate.1km.80.10$PlotID), FUN=mean, na.rm=T)
names(climate.norms) <- c("PlotID", paste(names(climate.norms[,2:ncol(climate.norms)]), "norm", sep="."))
summary(climate.norms)
length(unique(climate.norms$PlotID))


# Merging current IV together with climate norms
plot.norms <- merge(spp.data, climate.norms, all.x=T, all.y=T)
summary(plot.norms)
dim(plot.norms); dim(spp.data); dim(climate.norms)

# Disturbances in 2013
release.smooth <- read.csv("Data/processed_inputs/ReleaseEvents_Plots_Smooth_TimeElapsed.csv")
summary(release.smooth)

release <- release.smooth[release.smooth$Year==2013,]
summary(release); dim(release)

plot.norms1 <- merge(plot.norms, release, all.x=T, all.y=T)
summary(plot.norms1)

# Adding in soil
plot.norms2 <- merge(plot.norms1, soils, all.x=T)
summary(plot.norms2)
dim(plot.norms2); dim(plot.norms1)

# Adding in Topo
plot.norms3 <- merge(plot.norms2, topo[,c("PlotID", "elev", "flow", "TPI", "slope", "aspect", "sol.yr")], all.x=T)
summary(plot.norms3)
dim(plot.norms3); dim(plot.norms2)

# Adding in Plot Data
plot.norms4 <- merge(plot.norms3, plot.data[,c("PlotID", "live.ba", "dead.ba", "live.dens")], all.x=T)
summary(plot.norms4)
dim(plot.norms4); dim(plot.norms3)

write.csv(plot.norms4, "Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF.csv", row.names=F)

write.csv(plot.norms4[plot.norms4$Spp=="QURU",], "Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF_QURU.csv", row.names=F)
write.csv(plot.norms4[plot.norms4$Spp=="QUPR",], "Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF_QUPR.csv", row.names=F)
write.csv(plot.norms4[plot.norms4$Spp=="NYSY",], "Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF_NYSY.csv", row.names=F)
write.csv(plot.norms4[plot.norms4$Spp=="BELE",], "Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF_BELE.csv", row.names=F)
write.csv(plot.norms4[plot.norms4$Spp=="ACRU",], "Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF_ACRU.csv", row.names=F)




####################################################################################
# Replacing plot-level NA
# Random Forests won't work with missing data & presence-only (no IV = 0) matrix 
#   were too small, so we need to put something in there.  
#
# NA Fill Method: Climate from mean time of establishment of that species hierarchicaly
#   through the site.  First fill from mean species establishment in the transect and 
#   then the site.
#   -- This assumes similar timing/behavior across the site & doesn't screw species like 
#      BELE up that might come in in a slight gap after everything else
#   ** If the species is missing at an entire site (BELE), fill with the MEDIAN establishment
#      for the PLOT
####################################################################################
# Load plot RF data
plot.estab <- read.csv("Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_withNA.csv")
summary(plot.estab)
summary(plot.estab[,1:18])

# Looking at some stats for what we're going to gapfill 
summary(plot.estab[plot.estab$Spp %in% c("ACRU", "BELE", "NYSY", "QUPR", "QURU") & plot.estab$IV.avg==0, c("Site", "PlotID", "Spp", "IV.avg", "IV.avg.dated")])
summary(plot.estab[plot.estab$Spp %in% c("BELE") & plot.estab$IV.avg==0, c("Site", "PlotID", "Spp", "IV.avg", "IV.avg.dated")])

# -------------------------------------
# Find mean year of stand establishment
# -------------------------------------
cores.estab <- read.csv("Data/raw_inputs/Establishment_AllCores.csv")
summary(cores.estab)

# Getting mean & 95% CI for each year
# by transect & species

estab.spp.trans <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "Trans", "Spp")]), FUN=median)
names(estab.spp.trans) <- c("Site", "Trans", "Spp", "med.estab")
estab.spp.trans$ci.lo <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "Trans", "Spp")]), FUN=quantile, 0.025)[,4]
estab.spp.trans$ci.hi <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "Trans", "Spp")]), FUN=quantile, 0.975)[,4]
estab.spp.trans$n <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "Trans", "Spp")]), FUN=length)[,4]
estab.spp.trans$Site.trans <- as.factor(paste0(estab.spp.trans$Site, estab.spp.trans$Trans))

summary(estab.spp.trans)
# estab.spp.trans[estab.spp.trans$Spp %in% c("ACRU", "BELE", "NYSY", "QUPR", "QURU"),]

# by site & species
estab.spp.site <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "Spp")]), FUN=median)
names(estab.spp.site) <- c("Site", "Spp", "med.estab")
estab.spp.site$ci.lo <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "Spp")]), FUN=quantile, 0.025)[,3]
estab.spp.site$ci.hi <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "Spp")]), FUN=quantile, 0.975)[,3]
estab.spp.site$n <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "Spp")]), FUN=length)[,3]

summary(estab.spp.site)
estab.spp.site[estab.spp.site$Spp %in% c("ACRU", "BELE", "NYSY", "QUPR", "QURU"),]


# by plot
estab.cores.plot <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "PlotID")]), FUN=median)
names(estab.cores.plot) <- c("Site", "PlotID", "med.estab")
estab.cores.plot$ci.lo <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "PlotID")]), FUN=quantile, 0.025)[,3]
estab.cores.plot$ci.hi <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "PlotID")]), FUN=quantile, 0.975)[,3]
estab.cores.plot$n <- aggregate(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),"pith.use"], by=c(cores.estab[cores.estab$pith.use>min(plot.smooth$Year),c("Site", "PlotID")]), FUN=length)[,3]

summary(estab.cores.plot)
estab.cores.plot[estab.cores.plot$Site %in% c("FLT"),]
# -------------------------------------

# # plot.norms <- read.csv("Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF.csv")
# # summary(plot.norms)
# summary(plot.smooth)
# 
# dim(plot.estab); dim(plot.norms)
# names(plot.estab); names(plot.norms)
# 
# 
# summary(plot.estab[,1:18])
# 
# climate.vars <- c("Tmean.yr", "Tmean.JFM", "Tmean.MAM", "Tmean.JJA", "Tmean.SON", "Precip.yr", "Precip.JFM", "Precip.MAM", "Precip.JJA", "Precip.SON")
# disturb.vars <- c("time.Minor", "peak.Minor.mag", "peak.Minor.ext", "time.Major", "peak.Major.mag", "peak.Major.ext")
# 
# for(p in unique(plot.estab$PlotID)){
# 	for(s in unique(plot.estab$Spp)){
# 		#########################
# 		# Replacing missing temp & precip with 30-yr norms (columns 5-12)
# 		#########################
# 		# Important: Only do if Tmean, etc is missing
# 		if(is.na(min(plot.estab[plot.estab$PlotID==p & plot.estab$Spp==s, c(climate.vars, disturb.vars)]))){
# 
# 		if(nrow(estab.spp.trans[estab.spp.trans$Site.trans==substr(p,1,4) & estab.spp.trans$Spp==s,])>0){
# 			# if that species exists elsewhere in the transect, use that median
# 			yr.use <- round(estab.spp.trans[estab.spp.trans$Site.trans==substr(p,1,4) & estab.spp.trans$Spp==s,"med.estab"], 0)
# 		} else if(nrow(estab.spp.site[estab.spp.site$Site==substr(p,1,3) & estab.spp.site$Spp==s,])>0){
# 			yr.use <- round(estab.spp.site[estab.spp.site$Site==substr(p,1,3) & estab.spp.site$Spp==s,"med.estab"], 0)		
# 		} else {
# 			yr.use <- round(estab.cores.plot[estab.cores.plot$PlotID==p,"med.estab"], 0)		
# 		}
# 		# Do the actual overwriting
# 		plot.estab[plot.estab$PlotID==p & plot.estab$Spp==s, c(climate.vars, disturb.vars)] <- plot.smooth	[plot.smooth$PlotID==p & plot.smooth$Year==yr.use,c(paste0(climate.vars, ".smooth"), disturb.vars)]
# 		}
# 	}
# }
# summary(plot.estab[,1:18])
# 
# # summary(plot.estab[is.na(),1:20])

write.csv(plot.estab, "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled.csv", row.names=F)

write.csv(plot.estab[plot.estab$Spp=="QURU",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled_QURU.csv", row.names=F)
write.csv(plot.estab[plot.estab$Spp=="QUPR",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled_QUPR.csv", row.names=F)
write.csv(plot.estab[plot.estab$Spp=="NYSY",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled_NYSY.csv", row.names=F)
write.csv(plot.estab[plot.estab$Spp=="BELE",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled_BELE.csv", row.names=F)
write.csv(plot.estab[plot.estab$Spp=="ACRU",], "Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled_ACRU.csv", row.names=F)









####################################################################################
# Environmental Matrix
####################################################################################
summary(spp.data)

###################
# Mean Stand Age
###################
summary(estab.data2)
summary(tree.data2)

estab.data3 <- merge(estab.data2, tree.data2[,c("TreeID", "IV.avg")], all.x=F, all.y=F)
estab.data3$Age <- 2014-estab.data3$Year
summary(estab.data3)

length(unique(spp.data$Spp))
length(unique(estab.data3$Spp))

for(p in unique(estab.data3$PlotID)){
	for(s in unique(estab.data3$Spp)){
		spp.IV <- spp.data[spp.data $PlotID==p & spp.data$Spp==s, "IV.avg"]
		for(y in unique(estab.data3$Year))
		estab.data3[estab.data3$PlotID==p & estab.data3$Spp==s & estab.data3$Year==y, "IV.weight"] <- estab.data3[estab.data3$PlotID==p & estab.data3$Spp==s & estab.data3$Year==y, "IV.avg"]/spp.IV
	}
}
summary(estab.data3)

estab.data3$Age.weight <- estab.data3$Age * estab.data3$IV.weight
summary(estab.data3)

plot.env1a <- aggregate(estab.data3$Age, by=list(estab.data3$PlotID), FUN=mean)
names(plot.env1a) <- c("PlotID", "Age")
summary(plot.env1a)

plot.env1b <- aggregate(estab.data3[c("Age.weight", "IV.weight")], by=list(estab.data3$PlotID), FUN=sum)
names(plot.env1b) <- c("PlotID", "Age.weight1", "IV.weight")
plot.env1b$Age.IVweight <- plot.env1b$Age.weight1/plot.env1b$IV.weight
summary(plot.env1b)

plot.env1 <- merge(plot.env1a, plot.env1b[,c("PlotID", "Age.IVweight")], all.x=T, all.y=T)
summary(plot.env1)

###################
# Adding in release, norms, soil, & topo
###################
plot.env2 <- merge(plot.env1, climate.norms, all.x=T, all.y=T)
summary(plot.env2)
dim(plot.env2)

plot.env2b <- merge(plot.env2, release, all.x=T, all.y=T)
summary(plot.env2b)

plot.env3 <- merge(plot.env2b, soils, all.x=T, all.y=T)
summary(plot.env3)
dim(plot.env3)

plot.env4 <- merge(plot.env3, topo[,c("PlotID", "elev", "flow", "TPI", "slope", "aspect", "sol.yr")], all.x=T, all.y=T)
summary(plot.env4); dim(plot.env4)

plot.env5 <- merge(plot.env4, plot.data[,c("PlotID", "live.ba", "dead.ba", "live.dens")], all.x=T, all.y=T)
summary(plot.env5)
dim(plot.env5)

write.csv(plot.env5, "Data/processed_inputs/Plot_EnvironmentCharacteristics.csv", row.names=F)




####################################################################################
# Environmental Matrix -- weighted climate
####################################################################################
plot.estab <- read.csv("Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled.csv")
summary(plot.estab)
names(plot.estab)

IV.total <- aggregate(plot.estab$IV.avg, by=list(plot.estab$PlotID), FUN=sum)
names(IV.total) <- c("PlotID", "IV.total")
summary(IV.total)

plot.env1 <- IV.total

for(p in unique(plot.env1$PlotID)){
	for(v in c(climate.vars, disturb.vars)){
	plot.env1[plot.env1$PlotID==p, v] <- sum(plot.estab[plot.estab$PlotID==p,v]*(plot.estab[plot.estab$PlotID==p, "IV.avg"]/plot.env1[plot.env1$PlotID==p, "IV.total"]))
	}
}
summary(plot.env1)	

plot.env2 <- merge(plot.env1, climate.norms, all.x=T, all.y=T)
summary(plot.env2)
dim(plot.env2)

plot.env3 <- merge(plot.env2, soils, all.x=T, all.y=T)
summary(plot.env3); dim(plot.env3)

plot.env4 <- merge(plot.env3, topo[,c("PlotID", "elev", "flow", "TPI", "slope")], all.x=T, all.y=T)
dim(plot.env4)

plot.env5 <- merge(plot.env4, plot.data[,c("PlotID", "live.ba", "dead.ba", "live.dens")], all.x=T, all.y=T)
summary(plot.env5)
dim(plot.env5)

write.csv(plot.env5, "Data/processed_inputs/Plot_EnvironmentCharacteristics_IVweights.csv", row.names=F)

