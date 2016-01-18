setwd("~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Establishment_Modeling")


climate.1km <- read.csv("Data/processed_inputs/CARCA_Plots_Climate_1km_Buffer_Wide.csv")
summary(climate.1km) 


plot.data <- read.csv("Data/raw_inputs/PlotData.csv")
names(plot.data) <- c("PlotID", "Site.Name", "Site", names(plot.data[,4:ncol(plot.data)]))
summary(plot.data) 

climate2 <- climate.1km[climate.1km$Year>=1990 & climate.1km$Year<=2011,] 

climate.yr.temp <- aggregate(climate2[,"Tmean.yr"], by=list(climate2$PlotID), FUN=mean)
names(climate.yr.temp) <- c("PlotID", "Tmean.yr")
summary(climate.yr.temp)

climate.yr.precip <- aggregate(climate2[,"Precip.yr"], by=list(climate2$PlotID), FUN=sum)
names(climate.yr.precip) <- c("PlotID", "Precip.yr")
summary(climate.yr.precip)


# HIK
mean(plot.data[plot.data$Site=="HIK", "latitude"])
mean(plot.data[plot.data$Site=="HIK", "longitude"])
range(plot.data[plot.data$Site=="HIK", "elevation"])
mean(plot.data[plot.data$Site=="HIK", "live.ba"]); sd(plot.data[plot.data$Site=="HIK", "live.ba"])

range(climate.yr.temp[substr(climate.yr.temp$PlotID, 1, 3)=="HIK","Tmean.yr"])
mean(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="HIK","Precip.yr"])
range(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="HIK","Precip.yr"])


# HNK
mean(plot.data[plot.data$Site=="HNK", "latitude"])
mean(plot.data[plot.data$Site=="HNK", "longitude"])
range(plot.data[plot.data$Site=="HNK", "elevation"])
mean(plot.data[plot.data$Site=="HNK", "live.ba"]); sd(plot.data[plot.data$Site=="HNK", "live.ba"])

range(climate.yr.temp[substr(climate.yr.temp$PlotID, 1, 3)=="HNK","Tmean.yr"])
mean(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="HNK","Precip.yr"])
range(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="HNK","Precip.yr"]) # Not same for all plots


# BLD
mean(plot.data[plot.data$Site=="BLD", "latitude"])
mean(plot.data[plot.data$Site=="BLD", "longitude"])
range(plot.data[plot.data$Site=="BLD", "elevation"])
mean(plot.data[plot.data$Site=="BLD", "live.ba"]); sd(plot.data[plot.data$Site=="BLD", "live.ba"])

range(climate.yr.temp[substr(climate.yr.temp$PlotID, 1, 3)=="BLD","Tmean.yr"])
mean(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="BLD","Precip.yr"])
range(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="BLD","Precip.yr"])


# MOR
mean(plot.data[plot.data$Site=="MOR", "latitude"])
mean(plot.data[plot.data$Site=="MOR", "longitude"])
range(plot.data[plot.data$Site=="MOR", "elevation"])
mean(plot.data[plot.data$Site=="MOR", "live.ba"]); sd(plot.data[plot.data$Site=="MOR", "live.ba"])

range(climate.yr.temp[substr(climate.yr.temp$PlotID, 1, 3)=="MOR","Tmean.yr"])
mean(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="MOR","Precip.yr"])
range(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="MOR","Precip.yr"])

# FLT
mean(plot.data[plot.data$Site=="FLT", "latitude"])
mean(plot.data[plot.data$Site=="FLT", "longitude"])
range(plot.data[plot.data$Site=="FLT", "elevation"])
mean(plot.data[plot.data$Site=="FLT", "live.ba"]); sd(plot.data[plot.data$Site=="FLT", "live.ba"])

range(climate.yr.temp[substr(climate.yr.temp$PlotID, 1, 3)=="FLT","Tmean.yr"])
mean(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="FLT","Precip.yr"])
range(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="FLT","Precip.yr"]) # NOTE: this one is not the same for all plots


# IRN
mean(plot.data[plot.data$Site=="IRN", "latitude"])
mean(plot.data[plot.data$Site=="IRN", "longitude"])
range(plot.data[plot.data$Site=="IRN", "elevation"])
mean(plot.data[plot.data$Site=="IRN", "live.ba"]); sd(plot.data[plot.data$Site=="IRN", "live.ba"])

range(climate.yr.temp[substr(climate.yr.temp$PlotID, 1, 3)=="IRN","Tmean.yr"])
mean(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="IRN","Precip.yr"])
range(climate.yr.precip[substr(climate.yr.precip$PlotID, 1, 3)=="IRN","Precip.yr"])






################################################################################################
# Looking at IV-weighted establishment conditions
################################################################################################
distrib.estab <- read.csv("Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled.csv")
summary(distrib.estab)

species <- c("ACRU", "BELE", "NYSY", "QUPR", "QURU")
var.cols <- c("IV.avg", "Precip.yr", "Precip.M_S", "Precip.MAM", "Tmean.M_S", "Tmean.MAM", "Tmean.JJA", "BD", "elev", "TPI")

sum.mean <- as.data.frame(species)
summary(sum.mean)


length(distrib.estab[distrib.estab$Spp=="ACRU" & distrib.estab$IV.avg>0,"Spp"])

for(i in unique(species)){
	for(j in unique(var.cols)){
		sum.mean[sum.mean$species==i,"n.plots"] <- length(distrib.estab[distrib.estab$Spp==i & distrib.estab$IV.avg>0,"Spp"])
		sum.mean[sum.mean$species==i,j] <- paste(round(mean(distrib.estab[distrib.estab$Spp==i & distrib.estab$IV.avg>0,j]), digits=1), " (", round(sd(distrib.estab[distrib.estab$Spp==i & distrib.estab$IV.avg>0,j]), digits=2), ")", sep="")
	}
}
sum.mean

write.csv(sum.mean, "Data/analyses/EstablishmentClimates_Species.csv", row.names=F)


# ACRU
summary(distrib.estab[distrib.estab$Spp=="ACRU" & distrib.estab$IV.avg>0,c("Spp", "IV.avg", "Precip.yr", "Precip.M_S", "Precip.MAM", "Tmean.M_S", "Tmean.MAM", "Tmean.JJA", "BD", "elev", "TPI")])

# BELE
summary(distrib.estab[distrib.estab$Spp=="BELE" & distrib.estab$IV.avg>0,c("Spp", "IV.avg", "Precip.yr", "Precip.M_S", "Precip.MAM", "Tmean.M_S", "Tmean.MAM", "Tmean.JJA", "BD", "elev", "TPI")])

# NYSY
summary(distrib.estab[distrib.estab$Spp=="NYSY" & distrib.estab$IV.avg>0,c("Spp", "IV.avg", "Precip.yr", "Precip.M_S", "Precip.MAM", "Tmean.M_S", "Tmean.MAM", "Tmean.JJA", "BD", "elev", "TPI")])

# QUPR
summary(distrib.estab[distrib.estab$Spp=="QUPR" & distrib.estab$IV.avg>0,c("Spp", "IV.avg", "Precip.yr", "Precip.M_S", "Precip.MAM", "Tmean.M_S", "Tmean.MAM", "Tmean.JJA", "BD", "elev", "TPI")])

# QURU
summary(distrib.estab[distrib.estab$Spp=="QURU" & distrib.estab$IV.avg>0,c("Spp", "IV.avg", "Precip.yr", "Precip.M_S", "Precip.MAM", "Tmean.M_S", "Tmean.MAM", "Tmean.JJA", "BD", "elev", "TPI")])

################################################################################################
# Looking at IV-weighted establishment conditions
################################################################################################
distrib.norm <- read.csv("Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF.csv")
summary(distrib.norm)

species <- c("ACRU", "BELE", "NYSY", "QUPR", "QURU")
var.cols2 <- c("IV.avg", "Precip.yr.norm", "Precip.M_S.norm", "Precip.MAM.norm", "Tmean.M_S.norm", "Tmean.MAM.norm", "Tmean.JJA.norm", "BD", "elev", "TPI")

sum.norm <- as.data.frame(species)
summary(sum.norm)


length(distrib.estab[distrib.estab$Spp=="ACRU" & distrib.estab$IV.avg>0,"Spp"])

for(i in unique(species)){
	for(j in unique(var.cols2)){
		sum.norm[sum.norm$species==i,"n.plots"] <- length(distrib.norm[distrib.norm$Spp==i,"Spp"])
		sum.norm[sum.norm$species==i, j] <- paste(round(mean(distrib.norm[distrib.norm$Spp==i,j]), digits=1), " (", round(sd(distrib.norm[distrib.norm$Spp==i,j]), digits=2), ")", sep="")
	}
}
sum.norm

write.csv(sum.norm, "Data/analyses/ClimateNorms_Species.csv", row.names=F)
