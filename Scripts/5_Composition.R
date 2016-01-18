setwd("~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Establishment_Modeling")

#################################################################
# Looking at correlations of environmental variables with plot composition
#################################################################
# Clear Memory

rm(list=ls())

# load("Composition_15Apr2014.RData")
set.seed(314) # set random seed so everything can be exactly reproduced


# Load required packages

library(vegan)

library(lattice)
library(lme4)
library(nlme)
library(reshape2)
library(ggplot2)
library(grid)
library(car)


# Standard Error Function
se <- function(x){
	sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}


large.axes2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=18), axis.text.y=element_text(color="black", size=18), axis.title.x=element_text(face="bold", size=20, vjust=-1),  axis.title.y=element_text(face="bold", size=20, vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines")) + theme(legend.position=c(0.2,0.8), legend.text=element_text(size=20), legend.title=element_text(size=20), legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + theme(strip.text=element_text(size=rel(1.25), face="bold"))



#################################################################
# Comparing species size class distributions
#################################################################
trees <- read.csv("Data/raw_inputs/TreeData.csv")
summary(trees)

spp.list <- read.csv("Data/raw_inputs/SppList.csv")
summary(spp.list)

trees2 <- merge(trees, spp.list[,c("Spp", "Spp.Group")], all.x=T, all.y=F)
trees2$Canopy.code <- recode(trees2$Canopy, "''='NA'; '*'='NA'; 'U'='1'; 'I'='2'; 'C'='3'; 'D'='4'")
levels(trees2$Canopy.code) <- c("U", "I", "C", "D", NA)
summary(trees2)

group.col <- read.csv("Data/raw_inputs/GroupColors.csv")
group.col <- group.col[order(group.col$Spp),]
summary(group.col)

group.col2 <- group.col[group.col$Spp.Group %in% unique(trees2$Spp.Group),]
group.col2
colors2 <- group.col2$Color


ggplot(data=trees2) + large.axes2 +
	geom_histogram(aes(x=DBH, fill=Spp.Group), binwidth=5) +
	scale_fill_manual(values=as.vector(group.col2$Color)) + 
	theme(legend.position=c(0.8,0.8), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.3)))

group.col3 <- group.col[group.col$Spp.Group %in% unique(trees2[!is.na(trees2$Canopy.code),"Spp.Group"]),]
group.col3
colors3 <- group.col3$Color

ggplot(data=trees2[!is.na(trees2$Canopy.code),]) + large.axes2 +
	geom_histogram(aes(x=Canopy.code, fill=Spp.Group), binwidth=5) +
	scale_fill_manual(values=as.vector(group.col3$Color)) + 
	theme(legend.position=c(0.9,0.8), legend.text=element_text(size=rel(1.25)), legend.title=element_text(size=rel(1.3)))



#################################################################
# Multivariate community Analyses
#################################################################

######################

# 0. Importing plots & species matrices

######################

plot.comp <- read.csv("Data/model_inputs/SpeciesMatrix_Final.csv", row.names=1)
summary(plot.comp)
plot.comp[1:10, 1:10]

plot.env <- read.csv("Data/processed_inputs/Plot_EnvironmentCharacteristics_IVweights.csv")
summary(plot.env)

###############################################################################################
# Looking at correlation of plot competition with climate & site characteristics
###############################################################################################

######################
# 1. Oridnation
######################

nms1 <- metaMDS(plot.comp)
nms1
nms1$ndim

#trying an interactive plot
#orditkplot(nms1, display="sites", main="CARCA plot composition, by importance value
#NMS, hulls showing sites")
ordiplot(nms1, display="sites")
rows.HIK <- as.numeric(substr(row.names(plot.comp),1,3)=="HIK")
rows.HNK <- as.numeric(row.names(plot.comp)=="HNK")
rows.BLD <- as.numeric(row.names(plot.comp)=="BLD")
rows.MOR <- as.numeric(row.names(plot.comp)=="MOR")
rows.FLT <- as.numeric(row.names(plot.comp)=="FLT")
rows.IRN <- as.numeric(row.names(plot.comp)=="IRN")


par(mar=c(5,5,1,1))
nms.plot <- ordiplot(nms1, type="none", cex.axis=1.5, cex.lab=1.5, font.lab=2)
# text(nms.plot$species, labels=names(plot.comp), font=1, cex=0.75)
points(nms.plot$species, cex=0.75)
points(nms.plot$sites[substr(row.names(nms.plot$sites),1,3)=="HIK",], pch=13, col="darkorchid4", cex=1.5)
points(nms.plot$sites[substr(row.names(nms.plot$sites),1,3)=="HNK",], pch=8, col="blue", cex=1.5)
points(nms.plot$sites[substr(row.names(nms.plot$sites),1,3)=="BLD",], pch=18, col="green4", cex=1.5)
points(nms.plot$sites[substr(row.names(nms.plot$sites),1,3)=="MOR",], pch=17, col="gold3", cex=1.5)
points(nms.plot$sites[substr(row.names(nms.plot$sites),1,3)=="FLT",], pch=19, col="darkorange2", cex=1.5)
points(nms.plot$sites[substr(row.names(nms.plot$sites),1,3)=="IRN",], pch=7, col="red3", cex=1.5)
legend("topleft", legend=c("HIK", "HNK", "BLD", "MOR", "FLT", "IRN"), pch=c(13, 8, 18, 17, 19, 7), col=c("darkorchid4", "blue", "green4", "gold3", "darkorange2", "red3"), bty="n", cex=1.25, title="Sites (N to S)")

#legend("topleft", legend=c("HIK", "HNK", "BLD", "MOR", "FLT", "IRN"), pch=c(13, 8, 18, 17, 19, 15), col=c("darkorchid4", "blue", "darkgreen", "gold3", "darkorange2", "red3"), bty="n", cex=1, title="Site", font=2)

#ordilabel(nms1, labels=plots$PlotID, cex=0.5)
#ordihull(nms1[, group=plots$site.code, show="BLU", col="blue")
#ordihull(nms1, group=plots$site.code, show="BLD", col="green")
#ordihull(nms1, group=plots$site.code, show="IRN", col="orange")
#ordihull(nms1, group=plots$site.code, show="MOR", col="green4")
#ordihull(nms1, group=plots$site.code, show="HIK", col="purple")
#ordihull(nms1, group=plots$site.code, show="HNK", col="red")
summary(plot.env)

# # fit1 <- envfit(nms1 ~ Tmean.yr.norm, data=plot.env, strata=plot.env$Site, perm=2e4, na.rm=TRUE)
# fit1 # R2 = 0.04, p = 0.26
# plot(fit1, col="black", cex=1, font=2, labels="PRISM Tavg")

# fit1 <- envfit(nms1 ~ Tmean.yr, data=plot.env, strata=plot.env$Site, perm=2e4, na.rm=TRUE)
# fit1 # R2 = 0.05, p = 0.3143
# plot(fit2, col="black", cex=1, font=2, labels="Tavg")

# fit.clim <- envfit(nms1 ~ Tmean.yr + Tmean.MAM + Tmean.JJA + Tmean.M_S + Precip.yr + Precip.MAM + Precip.JJA + Precip.M_S, data=plot.env, strata=plot.env$Site, perm=2e4, na.rm=TRUE)
# fit.clim 
# plot(fit3, col="black", cex=1, font=2, labels=c("Tavg", "Max Disturb", "Disturb Yr"))

# fit.clim.norm <- envfit(nms1 ~ Tmean.yr.norm + Tmean.MAM.norm + Tmean.JJA.norm + Tmean.M_S.norm + Precip.yr.norm + Precip.MAM.norm + Precip.JJA.norm + Precip.M_S.norm, data=plot.env, strata=plot.env$Site, perm=2e4, na.rm=TRUE)
# fit.clim.norm 

# # Env Fit with all variables
# fit.all1 <- envfit(nms1 ~ Tmean.yr + Tmean.MAM + Tmean.JJA + Tmean.M_S + Precip.yr + Precip.MAM + Precip.JJA + Precip.M_S + aws.0150 + brock.dep.min + Utisols + Inceptisols + ksat + pH + kffact + BD + clay + sieve10 + sieve200 + om + elev + flow + TPI + slope, data=plot.env, strata=plot.env$Site, perm=2e4, na.rm=TRUE)
# fit.all1 

# Env Fit with all variables; including normals vs. weighted climate
fit.all2 <- envfit(nms1 ~ time.Minor + peak.Minor.mag + peak.Minor.ext + time.Major + peak.Major.mag + peak.Major.ext + Tmean.yr + Tmean.yr.norm + Tmean.MAM + Tmean.MAM.norm + Tmean.JJA + Tmean.JJA.norm + Tmean.M_S + Tmean.M_S.norm + Precip.yr + Precip.yr.norm + Precip.MAM + Precip.MAM.norm + Precip.JJA + Precip.JJA.norm + Precip.M_S + Precip.M_S.norm + aws.0150 + brock.dep.min + Utisols + Inceptisols + ksat + pH + kffact + BD + clay + sieve10 + sieve200 + om + elev + flow + TPI + slope, data=plot.env, strata=plot.env$Site, perm=2e4, na.rm=TRUE)
fit.all2 



# Trying again removing the strata term
fit.all2b <- envfit(nms1 ~ time.Minor + peak.Minor.mag + peak.Minor.ext + time.Major + peak.Major.mag + peak.Major.ext + Tmean.yr + Tmean.yr.norm + Tmean.MAM + Tmean.MAM.norm + Tmean.JJA + Tmean.JJA.norm + Tmean.M_S + Tmean.M_S.norm + Precip.yr + Precip.yr.norm + Precip.MAM + Precip.MAM.norm + Precip.JJA + Precip.JJA.norm + Precip.M_S + Precip.M_S.norm + aws.0150 + brock.dep.min + Utisols + Inceptisols + ksat + pH + kffact + BD + clay + sieve10 + sieve200 + om + elev + flow + TPI + slope, data=plot.env, perm=2e4, na.rm=TRUE)
fit.all2b 

# Re-fitting while removing climate norms
fit.all3 <- envfit(nms1 ~ time.Minor + peak.Minor.mag + peak.Minor.ext + time.Major + peak.Major.mag + peak.Major.ext + Tmean.yr + Tmean.MAM + Tmean.JJA + Tmean.M_S + Precip.yr + Precip.MAM + Precip.JJA + + Precip.M_S + aws.0150 + brock.dep.min + Utisols + Inceptisols + ksat + pH + kffact + BD + clay + sieve10 + sieve200 + om + elev + flow + TPI + slope, data=plot.env, strata=plot.env$Site, perm=2e4, na.rm=TRUE)
fit.all3 

# Re-fitting while removing climate norms & strata
fit.all3b <- envfit(nms1 ~ time.Minor + peak.Minor.mag + peak.Minor.ext + time.Major + peak.Major.mag + peak.Major.ext + Tmean.yr + Tmean.MAM + Tmean.JJA + Tmean.M_S + Precip.yr + Precip.MAM + Precip.JJA + + Precip.M_S + aws.0150 + brock.dep.min + Utisols + Inceptisols + ksat + pH + kffact + BD + clay + sieve10 + sieve200 + om + elev + flow + TPI + slope, data=plot.env, perm=2e4, na.rm=TRUE)
fit.all3b 


nms1
summary(nms1$points)

nms.plot$species
quercus <- c("QUAL", "QUCO", "QUPR", "QURU", "QUVE")
acer <- c("ACRU", "ACSA")
betula <- c("BELE", "BEPA")
carya <- c("CAGL", "CATO", "CAOV")
pinaceae <- c("PIRI", "PIST", "PIVI", "TSCA")
taxa.grouped <- c(quercus, acer, betula, carya, pinaceae)

species.all <- rownames(nms.plot$species)
other <- species.all[!(species.all %in% taxa.grouped)] 

pdf("Figures/Ordination/Composition_NMS_EnvFit_p01_NoStrata.pdf", width=10, height=7.5)
par(mar=c(5,5,1,1))
plot <- ordiplot(nms1, type="none", main="", cex.lab=1.5, font.lab=2, cex.axis=1.5, font.axis=1)
# text(plot, "species", col="black", cex=0.9)
# points(plot, "species", col="black", pch=20)
points(plot, "species", col="red", pch=7, select=row.names(plot$species) %in%  quercus, cex=2)
points(plot, "species", col="orange3", pch=8, select=row.names(plot$species) %in%  carya, cex=2)
points(plot, "species", col="purple", pch=9, select=row.names(plot$species) %in%  acer, cex=2)
points(plot, "species", col="blue", pch=10, select=row.names(plot$species) %in%  betula, cex=2)
points(plot, "species", col="green3", pch=14, select=row.names(plot$species) %in%  pinaceae, cex=2)
points(plot, "species", col="black", pch=1, select=row.names(plot$species) %in%  other, cex=1.5)
plot(fit.all3b, p.max=0.01, col="black", cex=1.25, font=2)
legend("topright", legend=c("Quercus", "Carya", "Acer", "Betula", "Pinaceae", "Other"), pch=c(7,8,9,10,13,1), col=c("red", "orange3", "purple", "blue", "green3", "black"), cex=1.5)
dev.off()

pdf("Figures/Ordination/Composition_NMS_EnvFit_p01_NoStrata_Text.pdf", width=10, height=7.5)
par(mar=c(5,5,1,1))
plot <- ordiplot(nms1, type="none", main="", cex.lab=1.5, font.lab=2, cex.axis=1.5, font.axis=1)
# text(plot, "species", col="black", cex=0.9)
# text(plot, "species", col="black", pch=20)
text(plot, "species", col="red", select=row.names(plot$species) %in%  quercus, cex=0.9)
text(plot, "species", col="orange3", select=row.names(plot$species) %in%  carya, cex=0.9)
text(plot, "species", col="purple", select=row.names(plot$species) %in%  acer, cex=0.9)
text(plot, "species", col="blue", select=row.names(plot$species) %in%  betula, cex=0.9)
text(plot, "species", col="green3", select=row.names(plot$species) %in%  pinaceae, cex=0.9)
text(plot, "species", col="black",  select=row.names(plot$species) %in%  other, cex=0.9)
plot(fit.all3b, p.max=0.01, col="black", cex=1.25, font=2)
legend("topright", legend=c("Quercus", "Carya", "Acer", "Betula", "Pinaceae", "Other"), pch=c(7,8,9,10,13,1), col=c("red", "orange3", "purple", "blue", "green3", "black"), cex=1.5)
dev.off()


pdf("Figures/Ordination/Composition_NMS_EnvFit_p05_NoStrata.pdf", width=10, height=7.5)
par(mar=c(5,5,1,1))
plot <- ordiplot(nms1, type="none", main="", cex.lab=1.5, font.lab=2, cex.axis=1.5, font.axis=1)
# text(plot, "species", col="black", cex=0.9)
# points(plot, "species", col="black", pch=20)
points(plot, "species", col="red", pch=7, select=row.names(plot$species) %in%  quercus, cex=2)
points(plot, "species", col="orange3", pch=8, select=row.names(plot$species) %in%  carya, cex=2)
points(plot, "species", col="purple", pch=9, select=row.names(plot$species) %in%  acer, cex=2)
points(plot, "species", col="blue", pch=10, select=row.names(plot$species) %in%  betula, cex=2)
points(plot, "species", col="green3", pch=14, select=row.names(plot$species) %in%  pinaceae, cex=2)
points(plot, "species", col="black", pch=1, select=row.names(plot$species) %in%  other, cex=1.5)
plot(fit.all3b, p.max=0.05, col="black", cex=1.25, font=2)
legend("topright", legend=c("Quercus", "Carya", "Acer", "Betula", "Pinaceae", "Other"), pch=c(7,8,9,10,13,1), col=c("red", "orange3", "purple", "blue", "green3", "black"), cex=1.5)
dev.off()


pdf("Figures/Ordination/Composition_NMS_EnvFit_p01_Strata.pdf", width=10, height=7.5)
par(mar=c(5,5,1,1))
plot <- ordiplot(nms1, type="none", main="", cex.lab=1.5, font.lab=2, cex.axis=1.5, font.axis=1)
# text(plot, "species", col="black", cex=0.9)
# points(plot, "species", col="black", pch=20)
points(plot, "species", col="red", pch=7, select=row.names(plot$species) %in%  quercus, cex=2)
points(plot, "species", col="orange3", pch=8, select=row.names(plot$species) %in%  carya, cex=2)
points(plot, "species", col="purple", pch=9, select=row.names(plot$species) %in%  acer, cex=2)
points(plot, "species", col="blue", pch=10, select=row.names(plot$species) %in%  betula, cex=2)
points(plot, "species", col="green3", pch=14, select=row.names(plot$species) %in%  pinaceae, cex=2)
points(plot, "species", col="black", pch=1, select=row.names(plot$species) %in%  other, cex=1.5)
plot(fit.all3, p.max=0.01, col="black", cex=1.25, font=2)
legend("topright", legend=c("Quercus", "Carya", "Acer", "Betula", "Pinaceae", "Other"), pch=c(7,8,9,10,13,1), col=c("red", "orange3", "purple", "blue", "green3", "black"), cex=1.5)
dev.off()

pdf("Figures/Ordination/Composition_NMS_EnvFit_p05_Strata.pdf", width=10, height=7.5)
par(mar=c(5,5,1,1))
plot <- ordiplot(nms1, type="none", main="", cex.lab=1.5, font.lab=2, cex.axis=1.5, font.axis=1)
# text(plot, "species", col="black", cex=0.9)
# points(plot, "species", col="black", pch=20)
points(plot, "species", col="red", pch=7, select=row.names(plot$species) %in%  quercus, cex=2)
points(plot, "species", col="orange3", pch=8, select=row.names(plot$species) %in%  carya, cex=2)
points(plot, "species", col="purple", pch=9, select=row.names(plot$species) %in%  acer, cex=2)
points(plot, "species", col="blue", pch=10, select=row.names(plot$species) %in%  betula, cex=2)
points(plot, "species", col="green3", pch=14, select=row.names(plot$species) %in%  pinaceae, cex=2)
points(plot, "species", col="black", pch=1, select=row.names(plot$species) %in%  other, cex=1.5)
plot(fit.all3, p.max=0.05, col="black", cex=1.25, font=2)
legend("topright", legend=c("Quercus", "Carya", "Acer", "Betula", "Pinaceae", "Other"), pch=c(7,8,9,10,13,1), col=c("red", "orange3", "purple", "blue", "green3", "black"), cex=1.5)
dev.off()



######################
# 2. Effect of transect on composition
######################
# Hypothesis: Community composition is different among transects at a site
# Test: PERMANOVA
test0 <- adonis(plot.comp ~ Tmean.yr + Tmean.MAM + Tmean.JJA + Tmean.M_S + Precip.yr + Precip.MAM + Precip.JJA + Precip.M_S + aws.0150 + brock.dep.min + Utisols + Inceptisols + ksat + pH + kffact + BD + clay + sieve10 + sieve200 + om + elev + flow + TPI + slope, strata=plot.env$Site, data=plot.env, method = "bray", perm=5e4)
test0 	#Cumulative R2 = 0.65

test1 <- adonis(plot.comp ~ Tmean.yr*Precip.yr*ksat*kffact*sieve200, strata=plot.env$Site, data=plot.env, method = "bray", perm=5e4)
test1 #Cumulative R2 = 0.71


# With mean normal annual tem
test2 <- adonis(plot.comp ~ Tmean.yr + Precip.yr + aws.0150 + brock.dep.min + Utisols + Inceptisols + ksat + pH + kffact + BD + clay + sieve10 + sieve200 + om + elev + flow + TPI + slope, strata=plot.env$Site, data=plot.env, method = "bray", perm=5e4)
test2 	#Cumulative R2 = 0.58


test3 <- adonis(plot.comp ~ Tmean.yr.norm + Precip.yr.norm + aws.0150 + brock.dep.min + Utisols + Inceptisols + ksat + pH + kffact + BD + clay + sieve10 + sieve200 + om + elev + flow + TPI + slope, strata=plot.env$Site, data=plot.env, method = "bray", perm=5e4)
test3 	#Cumulative R2 = 0.68





###############################################################################################
# Looking at correlation of plot competition with climate & site characteristics
###############################################################################################
summary(plot.env)

ggplot(data=plot.env) +
	geom_point(aes(x=Precip.yr.norm, y=Tmean.yr.norm), color="black", size=5) +
	geom_point(aes(x=Precip.yr, y=Tmean.yr), color="red", size=5) +
	large.axes2

plot(Tmean.yr ~ Tmean.yr.norm, data=plot.env)	
abline(a=0, b=1, col="red")

plot(Precip.yr ~ Precip.yr.norm, data=plot.env)	
abline(a=0, b=1, col="red")

plot(Tmean.yr.norm ~ Precip.yr.norm, data=plot.env, ylim=c(8,12), xlim=c(70,120))
par(new=T)
plot(Tmean.yr ~ Precip.yr, data=plot.env, pch=19, col="red", ylim=c(8,12), xlim=c(70,120))
