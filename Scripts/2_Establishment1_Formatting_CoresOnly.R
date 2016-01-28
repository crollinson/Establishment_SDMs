setwd("~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Establishment_Modeling")

# clear memory
rm(list=ls())

# importing libraries
library(reshape2)
library(car)
library(ggplot2)
library(grid)

se <- function(x){
	sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}

################################################################################################
# Reading in and merging data sets
plot.data <- read.csv("Data/raw_inputs/Plot_Data_AllYrs.csv")
summary(plot.data)
dim(plot.data)

tree.data <- read.csv("Data/raw_inputs/TreeData.csv")
summary(tree.data)

estab.cores <- read.csv("Data/raw_inputs/Establishment_AllCores.csv")
estab.cores$Plot <- as.factor(estab.cores$Plot) 
estab.cores$Tree <- as.factor(estab.cores$Tree) 
estab.cores$pith.use <- round(estab.cores$pith.use, digits=0)
estab.cores <- estab.cores[,c("TreeID", "PlotID", "Site", "Trans", "Plot", "Spp", "Genus", "Canopy", "DBH", "Pith.Yr", "pith.calc", "pith.modeled", "pith.use", "Inner", "Outer", "Bark")]
summary(estab.cores)

spp.list <- read.csv("Data/raw_inputs/SppList.csv", na.strings="")
summary(spp.list)

# Adding species group to tree info
tree.data <- merge(tree.data, spp.list[,c("Spp", "Spp.Group")], all.x=T, all.y=F)
summary(tree.data)

# Subsetting only core-able stuff for the study sites (no BLU, no IRND)
tree.data2 <- tree.data[tree.data$DBH>=5 & (tree.data$Trans=="A" | tree.data$Trans=="B" | tree.data$Trans=="C"), ]
summary(tree.data2)

# Aggregating to get the total number of stems per plot of each focal group
tree.spp <- aggregate(tree.data2$Stems, by=list(tree.data2$PlotID, tree.data2$Spp.Group), FUN=sum, na.rm=T)
names(tree.spp) <- c("PlotID", "Spp.Group", "n.Total")
summary(tree.spp)

################################################################################################
# Formatting Establishment
# Adding species group to establishment
estab.cores <- merge(estab.cores, spp.list[,c("Spp", "Spp.Group")], all.x=T, all.y=F)
summary(estab.cores)
summary(estab.cores$Spp.Group)

# Aggregating to get the number of establishment events for a species group in a given year
estab.group <- aggregate(estab.cores[,c("pith.use")], by=list(estab.cores$PlotID, estab.cores$Site, estab.cores$Spp.Group, estab.cores$pith.use), FUN=length)
names(estab.group) <- c("PlotID", "Site", "Spp.Group", "Year", "n.trees")
summary(estab.group)


# proportion of each species group with estimated regeneration date for a given window 
for(i in unique(estab.group$Spp.Group)){
	for(p in unique(estab.group$PlotID)){
		n.present <- tree.spp[tree.spp$PlotID==p & tree.spp$Spp.Group==i,"n.Total"]
		for(y in unique(estab.group$Year)){
			estab.group[estab.group$PlotID==p & estab.group$Spp.Group==i & estab.group$Year==y,"p.Group"] <- estab.group[estab.group$PlotID==p & estab.group$Spp.Group==i & estab.group$Year==y,"n.trees"]/n.present
		}	}
}
summary(estab.group)


write.csv(estab.group, "Data/processed_inputs/Establishment_SpeciesGroup_Plot.csv", row.names=F)

################################################################################################
# Adding in plot-level climate data & soils data
estab.group <- read.csv("Data/processed_inputs/Establishment_SpeciesGroup_Plot.csv")
estab.group$Year <- as.factor(estab.group$Year)
summary(estab.group)

release <- read.csv("Data/processed_inputs/ReleaseEvents_Plots.csv")
release$Year <- as.factor(release$Year)
summary(release)

estab.group2 <- merge(estab.group[,c("PlotID", "Year", "Spp.Group", "n.trees", "p.Group")], release[,c("PlotID", "Year", "Release.Minor", "Release.Major", "n.Minor", "n.Major", "n.Plot", "p.Minor", "p.Major")], all.x=T, all.y=T)
summary(estab.group2)


###############
# Years with no establishment get filled in weird, so need to do some magic to make it fill even (for smoothing later)
estab.melt <- melt(estab.group2[,c("PlotID", "Year", "Spp.Group", "n.trees", "p.Group")])
summary(estab.melt)

summary(estab.melt)
n.recast <- dcast(estab.melt[estab.melt$variable=="n.trees",], PlotID + Year ~ Spp.Group)
summary(n.recast)

n.stack <- stack(n.recast[,3:(ncol(n.recast)-1)])
names(n.stack) <- c("n.trees", "Spp.Group")
n.stack$n.trees <- ifelse(is.na(n.stack$n.trees),0,n.stack$n.trees)
n.stack$PlotID <- n.recast$PlotID
n.stack$Year <- n.recast$Year
summary(n.stack)

p.recast <- dcast(estab.melt[estab.melt$variable=="p.Group",], PlotID + Year ~ Spp.Group)
summary(p.recast)

p.stack <- stack(p.recast[,3:(ncol(p.recast)-1)])
names(p.stack) <- c("p.Group", "Spp.Group")
p.stack$p.Group <- ifelse(is.na(p.stack$p.Group),0,p.stack$p.Group)
p.stack$PlotID <- p.recast$PlotID
p.stack$Year <- p.recast$Year
summary(p.stack)

estab.merge <- merge(n.stack, p.stack, all.x=T, all.y=T)
summary(estab.merge)

#############
# Adding the release events back in
estab.group3 <- merge(estab.merge, release[,c("PlotID", "Year", "Release.Minor", "Release.Major", "n.Minor", "n.Major", "n.Plot", "p.Minor", "p.Major")], all.x=T, all.y=T)
estab.group3$n.Minor <- ifelse(is.na(estab.group3$n.Minor), 0, estab.group3$n.Minor)
estab.group3$n.Major <- ifelse(is.na(estab.group3$n.Major), 0, estab.group3$n.Major)
estab.group3$n.Plot <- ifelse(is.na(estab.group3$n.Plot), 0, estab.group3$n.Plot)
estab.group3$p.Minor <- ifelse(is.na(estab.group3$p.Minor), 0, estab.group3$p.Minor)
estab.group3$p.Major <- ifelse(is.na(estab.group3$p.Major), 0, estab.group3$p.Major)
summary(estab.group3)

###########################
# 1 km climate
###########################
climate.1km <- read.csv("Data/processed_inputs/CARCA_Plots_Climate_1km_Buffer_Wide.csv")
climate.1km$Year <- as.factor(climate.1km$Year)
summary(climate.1km)

length(unique(estab.group$PlotID)); length(unique(climate.1km$PlotID))

# Merging in establishment
estab.group4 <- merge(estab.group3, climate.1km, all.x=T, all.y=T)
estab.group4 <- estab.group4[!is.na(estab.group4$Spp.Group),]
summary(estab.group4)


write.csv(estab.group4, "Data/processed_inputs/Establishment_SpeciesGroup_Plot_Climate_1km.csv", row.names=F)

################################################################################################
# Adding in Static topogrpahic variables
estab.group4 <- read.csv("Data/processed_inputs/Establishment_SpeciesGroup_Plot_Climate_1km.csv")
summary(estab.group4)

soils <- read.csv("Data/raw_inputs/CARCA_Plots_SoilsInfo_Final.csv")
summary(soils)

topo <- read.csv("Data/raw_inputs/CARCA_Plots_TopoInputs_1km_Buffer.csv")
summary(topo)

plot.data <- read.csv("Data/raw_inputs/Plot_Data_AllYrs.csv")
summary(plot.data)


estab.group5 <- merge(estab.group4, soils, all.x=T, all.y=F)
summary(estab.group5)
dim(estab.group5); dim(estab.group4)

estab.group6 <- merge(estab.group5, topo[,c("PlotID", "elev", "flow", "TPI", "slope")], all.x=T, all.y=F)
summary(estab.group6)
dim(estab.group6); dim(estab.group5)

estab.group7 <- merge(estab.group6, plot.data[,c("PlotID", "Year", "BA.m2ha.plot", "BA.m2ha.plot.live", "Density.ha.plot")], all.x=T, all.y=F)
summary(estab.group7)
dim(estab.group7); dim(estab.group6)


write.csv(estab.group7, "Data/processed_inputs/Establishment_SpeciesGroup_Plot_Climate_1km.csv", row.names=F)

unique(estab.group7$Year)
summary(estab.group7$Year)
################################################################################################
estab.group <- read.csv("Data/processed_inputs/Establishment_SpeciesGroup_Plot_Climate_1km.csv")
summary(estab.group)


estab.smooth <- estab.group[estab.group$Year>=1900,c("PlotID", "Site", "Spp.Group", "Year")]
summary(estab.smooth)

print(Sys.time())
# Smoothing Establishment
for(p in unique(estab.smooth$PlotID)){
	data.plot <- estab.group[estab.group$PlotID==p,]
for(s in unique(estab.smooth$Spp.Group)){
	data.spp <- data.plot[data.plot$Spp.Group==s,]

for(y in min(estab.smooth$Year):max(estab.smooth$Year)){
	# Establishment Factors
	estab.smooth[estab.smooth$PlotID==p & estab.smooth$Spp.Group==s & estab.smooth$Year==y,"n.smooth"] <- sum(data.spp[data.spp$Year>=(y-5) & data.spp$Year<=(y+5),"n.trees"], na.rm=T)
	estab.smooth[estab.smooth$PlotID==p & estab.smooth$Spp.Group==s & estab.smooth$Year==y,"p.smooth"] <- sum(data.spp[data.spp$Year>=(y-5) & data.spp$Year<=(y+5),"p.Group"], na.rm=T)
}
}
}
summary(estab.smooth)
summary(estab.group)

write.csv(estab.smooth, "Data/processed_inputs/Establishment_SpeciesGroup_Plot_Climate_1km_smooth_1900-2013.csv", row.names=F)

################################################################################################
# Adding back in disturbance & plot characteristics
################################################################################################
estab.smooth <- read.csv("Data/processed_inputs/Establishment_SpeciesGroup_Plot_Climate_1km_smooth_1900-2013.csv")
summary(estab.smooth)


#######################
# Climate
#######################
climate.smooth <- read.csv("Data/processed_inputs/CARCA_Plots_Climate_1km_Smooth.csv")
summary(climate.smooth)

estab.smooth1 <- merge(estab.smooth, climate.smooth, all.x=T, all.y=T)
summary(estab.smooth1)
dim(estab.smooth1); dim(estab.smooth); dim(climate.smooth)

#######################
# Disturbance
#######################
release.smooth <- read.csv("Data/processed_inputs/ReleaseEvents_Plots_Smooth_TimeElapsed.csv")
summary(release.smooth)

estab.smooth2 <- merge(estab.smooth1, release.smooth, all.x=T, all.y=F)
summary(estab.smooth2)
dim(estab.smooth2); dim(estab.smooth2)


#######################
# Soils, topography & plot
#######################
soils <- read.csv("Data/raw_inputs/CARCA_Plots_SoilsInfo_Final.csv")
summary(soils)

topo <- read.csv("Data/raw_inputs/CARCA_Plots_TopoInputs_1km_Buffer.csv")
summary(topo)

plot.data <- read.csv("Data/raw_inputs/Plot_Data_AllYrs.csv")
summary(plot.data)

estab.group5 <- merge(estab.smooth2, soils, all.x=T, all.y=F)
summary(estab.group5)
dim(estab.group5); dim(estab.smooth2)

estab.group6 <- merge(estab.group5, topo[,c("PlotID", "elev", "flow", "TPI", "slope")], all.x=T, all.y=F)
summary(estab.group6)
dim(estab.group6); dim(estab.group5)

estab.group7 <- merge(estab.group6, plot.data[,c("PlotID", "Year", "BA.m2ha.plot.live")], all.x=T, all.y=F)
summary(estab.group7)
dim(estab.group7); dim(estab.group6)

estab.group7$PlotID.Year <- as.factor(paste(estab.group7$PlotID, estab.group7$Year, sep="."))
estab.group7 <- estab.group7[complete.cases(estab.group7),]
summary(estab.group7) 

write.csv(estab.group7, "Data/model_inputs/Establishment_SpeciesGroup_Climate_1km_smooth_1900-2013_RF.csv", row.names=F)

write.csv(estab.group7[estab.group7$Spp.Group=="QURU",], "Data/model_inputs/Establishment_SpeciesGroup_RF_QURU.csv", row.names=F)
write.csv(estab.group7[estab.group7$Spp.Group=="QUPR",], "Data/model_inputs/Establishment_SpeciesGroup_RF_QUPR.csv", row.names=F)
write.csv(estab.group7[estab.group7$Spp.Group=="NYSY",], "Data/model_inputs/Establishment_SpeciesGroup_RF_NYSY.csv", row.names=F)
write.csv(estab.group7[estab.group7$Spp.Group=="BELE",], "Data/model_inputs/Establishment_SpeciesGroup_RF_BELE.csv", row.names=F)
write.csv(estab.group7[estab.group7$Spp.Group=="ACRU",], "Data/model_inputs/Establishment_SpeciesGroup_RF_ACRU.csv", row.names=F)
