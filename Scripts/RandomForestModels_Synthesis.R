###############################################################################################
# Synthesizing results from Random Forest Models
###############################################################################################
rm(list=ls())
setwd("~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Establishment_Modeling")

library(car)
library(ggplot2)
library(grid)
library(randomForest)


###################################################
# Do some initial setup
###################################################
se <- function(x){
	sd(x, na.rm=TRUE) / sqrt((length(!is.na(x))))}

species <- c("QURU", "QUPR", "NYSY", "ACRU")

q.blank <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=14, face="bold"), axis.text.y=element_text(color="black", size=12, face="bold"), axis.title.x=element_text(face="bold", size=14),  axis.title.y=element_text(face="bold", size=14))	

large.axes2 <- theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=rel(1.75)), axis.text.y=element_text(color="black", size=rel(1.75)), axis.title.x=element_text(face="bold", size=rel(2), vjust=-1),  axis.title.y=element_text(face="bold", size=rel(2), vjust=0.2), plot.margin=unit(c(2,2,2,2), "lines")) + theme(legend.background=element_rect(fill="white"), legend.key=element_rect(color="white", fill=NA)) + theme(strip.text=element_text(size=rel(1.25), face="bold"))

RF_data <- read.csv('Data/model_inputs/CurrentComp_SpeciesGroup_EstabClimate_RF_NAfilled.csv') # Data file
RF_data$Trans <- as.factor(substr(RF_data$PlotID, 4,4))
###################################################




###################################################
# Reading the results of the models
###################################################
# I'm tired so we're going to do this the long, ugly way
imp.vals <- list()
r2.vals <- list()
mse.vals <- list()



# ------------
# Current Climate, Presence-Absence 
# ------------
load("Data//analyses/RF_Current_PA_ClimOnly/ACRU_RFout.Rdata")
imp.vals[["PAClim"]] <- data.frame(var=row.names(importance(rf.mod)), ACRU=as.vector(importance(rf.mod, type=1)))
r2.vals [["PAClim"]] <- data.frame(ACRU=max(rf.mod$rsq))
mse.vals[["PAClim"]] <- data.frame(ACRU=min(rf.mod$mse))

load("Data//analyses/RF_Current_PA_ClimOnly/NYSY_RFout.Rdata")
imp.vals[["PAClim"]][,"NYSY"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["PAClim"]][,"NYSY"] <- max(rf.mod$rsq)
mse.vals[["PAClim"]][,"NYSY"] <- min(rf.mod$mse)

load("Data//analyses/RF_Current_PA_ClimOnly/QUPR_RFout.Rdata")
imp.vals[["PAClim"]][,"QUPR"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["PAClim"]][,"QUPR"] <- max(rf.mod$rsq)
mse.vals[["PAClim"]][,"QUPR"] <- min(rf.mod$mse)

load("Data//analyses/RF_Current_PA_ClimOnly/QURU_RFout.Rdata")
imp.vals[["PAClim"]][,"QURU"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["PAClim"]][,"QURU"] <- max(rf.mod$rsq)
mse.vals[["PAClim"]][,"QURU"] <- min(rf.mod$mse)
# ------------

# ------------
# Current Climate, Presence-Only 
# ------------
load("Data//analyses/RF_Current_ClimOnly/ACRU_RFout.Rdata")
imp.vals[["CurClim"]] <- data.frame(var=row.names(importance(rf.mod)), ACRU=as.vector(importance(rf.mod, type=1)))
r2.vals [["CurClim"]] <- data.frame(ACRU=max(rf.mod$rsq))
mse.vals[["CurClim"]] <- data.frame(ACRU=min(rf.mod$mse))

load("Data//analyses/RF_Current_ClimOnly/NYSY_RFout.Rdata")
imp.vals[["CurClim"]][,"NYSY"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["CurClim"]][,"NYSY"] <- max(rf.mod$rsq)
mse.vals[["CurClim"]][,"NYSY"] <- min(rf.mod$mse)

load("Data//analyses/RF_Current_ClimOnly/QUPR_RFout.Rdata")
imp.vals[["CurClim"]][,"QUPR"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["CurClim"]][,"QUPR"] <- max(rf.mod$rsq)
mse.vals[["CurClim"]][,"QUPR"] <- min(rf.mod$mse)

load("Data//analyses/RF_Current_ClimOnly/QURU_RFout.Rdata")
imp.vals[["CurClim"]][,"QURU"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["CurClim"]][,"QURU"] <- max(rf.mod$rsq)
mse.vals[["CurClim"]][,"QURU"] <- min(rf.mod$mse)
# ------------

# ------------
# Establishment Climate, Presence-Only 
# ------------
load("Data//analyses/RF_Estab_ClimOnly/ACRU_RFout.Rdata")
imp.vals[["EstabClim"]] <- data.frame(var=row.names(importance(rf.mod)), ACRU=as.vector(importance(rf.mod, type=1)))
r2.vals [["EstabClim"]] <- data.frame(ACRU=max(rf.mod$rsq))
mse.vals[["EstabClim"]] <- data.frame(ACRU=min(rf.mod$mse))

load("Data//analyses/RF_Estab_ClimOnly/NYSY_RFout.Rdata")
imp.vals[["EstabClim"]][,"NYSY"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["EstabClim"]][,"NYSY"] <- max(rf.mod$rsq)
mse.vals[["EstabClim"]][,"NYSY"] <- min(rf.mod$mse)

load("Data//analyses/RF_Estab_ClimOnly/QUPR_RFout.Rdata")
imp.vals[["EstabClim"]][,"QUPR"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["EstabClim"]][,"QUPR"] <- max(rf.mod$rsq)
mse.vals[["EstabClim"]][,"QUPR"] <- min(rf.mod$mse)

load("Data//analyses/RF_Estab_ClimOnly/QURU_RFout.Rdata")
imp.vals[["EstabClim"]][,"QURU"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["EstabClim"]][,"QURU"] <- max(rf.mod$rsq)
mse.vals[["EstabClim"]][,"QURU"] <- min(rf.mod$mse)
# ------------

# ------------
# Current Disturbance, Presence-Absence 
# ------------
load("Data//analyses/RF_Current_PA_Disturb/ACRU_RFout.Rdata")
imp.vals[["PADisturb"]] <- data.frame(var=row.names(importance(rf.mod)), ACRU=as.vector(importance(rf.mod, type=1)))
r2.vals [["PADisturb"]] <- data.frame(ACRU=max(rf.mod$rsq))
mse.vals[["PADisturb"]] <- data.frame(ACRU=min(rf.mod$mse))

load("Data//analyses/RF_Current_PA_Disturb/NYSY_RFout.Rdata")
imp.vals[["PADisturb"]][,"NYSY"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["PADisturb"]][,"NYSY"] <- max(rf.mod$rsq)
mse.vals[["PADisturb"]][,"NYSY"] <- min(rf.mod$mse)

load("Data//analyses/RF_Current_PA_Disturb/QUPR_RFout.Rdata")
imp.vals[["PADisturb"]][,"QUPR"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["PADisturb"]][,"QUPR"] <- max(rf.mod$rsq)
mse.vals[["PADisturb"]][,"QUPR"] <- min(rf.mod$mse)

load("Data//analyses/RF_Current_PA_Disturb/QURU_RFout.Rdata")
imp.vals[["PADisturb"]][,"QURU"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["PADisturb"]][,"QURU"] <- max(rf.mod$rsq)
mse.vals[["PADisturb"]][,"QURU"] <- min(rf.mod$mse)
# ------------

# ------------
# Current Disturbance, Presence-Only 
# ------------
load("Data//analyses/RF_Current_Disturb/ACRU_RFout.Rdata")
imp.vals[["CurDisturb"]] <- data.frame(var=row.names(importance(rf.mod)), ACRU=as.vector(importance(rf.mod, type=1)))
r2.vals [["CurDisturb"]] <- data.frame(ACRU=max(rf.mod$rsq))
mse.vals[["CurDisturb"]] <- data.frame(ACRU=min(rf.mod$mse))

load("Data//analyses/RF_Current_Disturb/NYSY_RFout.Rdata")
imp.vals[["CurDisturb"]][,"NYSY"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["CurDisturb"]][,"NYSY"] <- max(rf.mod$rsq)
mse.vals[["CurDisturb"]][,"NYSY"] <- min(rf.mod$mse)

load("Data//analyses/RF_Current_Disturb/QUPR_RFout.Rdata")
imp.vals[["CurDisturb"]][,"QUPR"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["CurDisturb"]][,"QUPR"] <- max(rf.mod$rsq)
mse.vals[["CurDisturb"]][,"QUPR"] <- min(rf.mod$mse)

load("Data//analyses/RF_Current_Disturb/QURU_RFout.Rdata")
imp.vals[["CurDisturb"]][,"QURU"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["CurDisturb"]][,"QURU"] <- max(rf.mod$rsq)
mse.vals[["CurDisturb"]][,"QURU"] <- min(rf.mod$mse)
# ------------

# ------------
# Establishment Disturbance, Presence-Only 
# ------------
load("Data//analyses/RF_Estab_Disturb/ACRU_RFout.Rdata")
imp.vals[["EstabDisturb"]] <- data.frame(var=row.names(importance(rf.mod)), ACRU=as.vector(importance(rf.mod, type=1)))
r2.vals [["EstabDisturb"]] <- data.frame(ACRU=max(rf.mod$rsq))
mse.vals[["EstabDisturb"]] <- data.frame(ACRU=min(rf.mod$mse))

load("Data//analyses/RF_Estab_Disturb/NYSY_RFout.Rdata")
imp.vals[["EstabDisturb"]][,"NYSY"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["EstabDisturb"]][,"NYSY"] <- max(rf.mod$rsq)
mse.vals[["EstabDisturb"]][,"NYSY"] <- min(rf.mod$mse)

load("Data//analyses/RF_Estab_Disturb/QUPR_RFout.Rdata")
imp.vals[["EstabDisturb"]][,"QUPR"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["EstabDisturb"]][,"QUPR"] <- max(rf.mod$rsq)
mse.vals[["EstabDisturb"]][,"QUPR"] <- min(rf.mod$mse)

load("Data//analyses/RF_Estab_Disturb/QURU_RFout.Rdata")
imp.vals[["EstabDisturb"]][,"QURU"] <- as.vector(importance(rf.mod, type=1))
r2.vals [["EstabDisturb"]][,"QURU"] <- max(rf.mod$rsq)
mse.vals[["EstabDisturb"]][,"QURU"] <- min(rf.mod$mse)
# ------------

# ------------
# Stacking everything into a mega-dataframe so it's easier to manipulate
# ------------
imp.vals[["PAClim"      ]][,"Climate"] <- r2.vals[["PAClim"      ]][,"Climate"] <- mse.vals[["PAClim"      ]][,"Climate"] <- as.factor("Modern PA")
imp.vals[["CurClim"     ]][,"Climate"] <- r2.vals[["CurClim"     ]][,"Climate"] <- mse.vals[["CurClim"     ]][,"Climate"] <- as.factor("Modern"   )
imp.vals[["EstabClim"   ]][,"Climate"] <- r2.vals[["EstabClim"   ]][,"Climate"] <- mse.vals[["EstabClim"   ]][,"Climate"] <- as.factor("Estab"    )
imp.vals[["PADisturb"   ]][,"Climate"] <- r2.vals[["PADisturb"   ]][,"Climate"] <- mse.vals[["PADisturb"   ]][,"Climate"] <- as.factor("Modern PA")
imp.vals[["CurDisturb"  ]][,"Climate"] <- r2.vals[["CurDisturb"  ]][,"Climate"] <- mse.vals[["CurDisturb"  ]][,"Climate"] <- as.factor("Modern"   )
imp.vals[["EstabDisturb"]][,"Climate"] <- r2.vals[["EstabDisturb"]][,"Climate"] <- mse.vals[["EstabDisturb"]][,"Climate"] <- as.factor("Estab"    )


imp.vals[["PAClim"      ]][,"Disturb"] <- r2.vals[["PAClim"      ]][,"Disturb"] <- mse.vals[["PAClim"      ]][,"Disturb"] <- as.factor("NO" )
imp.vals[["CurClim"     ]][,"Disturb"] <- r2.vals[["CurClim"     ]][,"Disturb"] <- mse.vals[["CurClim"     ]][,"Disturb"] <- as.factor("NO" )
imp.vals[["EstabClim"   ]][,"Disturb"] <- r2.vals[["EstabClim"   ]][,"Disturb"] <- mse.vals[["EstabClim"   ]][,"Disturb"] <- as.factor("NO" )
imp.vals[["PADisturb"   ]][,"Disturb"] <- r2.vals[["PADisturb"   ]][,"Disturb"] <- mse.vals[["PADisturb"   ]][,"Disturb"] <- as.factor("YES")
imp.vals[["CurDisturb"  ]][,"Disturb"] <- r2.vals[["CurDisturb"  ]][,"Disturb"] <- mse.vals[["CurDisturb"  ]][,"Disturb"] <- as.factor("YES")
imp.vals[["EstabDisturb"]][,"Disturb"] <- r2.vals[["EstabDisturb"]][,"Disturb"] <- mse.vals[["EstabDisturb"]][,"Disturb"] <- as.factor("YES")


for(i in 1:length(imp.vals)){
  imp.tmp <- stack(imp.vals[[i]][,species])
  names(imp.tmp) <- c("%IncMSE", "Species")
  imp.tmp[,c("var", "Climate", "Disturb")] <- imp.vals[[i]][,c("var", "Climate", "Disturb")]
    
  r2.tmp <- stack(r2.vals[[i]][,species])
  names(r2.tmp) <- c("R2", "Species")
  r2.tmp[,c("Climate", "Disturb")] <- r2.vals[[i]][,c("Climate", "Disturb")]

  mse.tmp <- stack(mse.vals[[i]][,species])
  names(mse.tmp) <- c("MSE", "Species")
  mse.tmp[,c("Climate", "Disturb")] <- mse.vals[[i]][,c("Climate", "Disturb")]
  
  if(i==1){
    imp.df <- imp.tmp
    r2.df  <- r2.tmp
    mse.df <- mse.tmp
  } else {
    imp.df <- rbind(imp.df, imp.tmp)
    r2.df  <- rbind(r2.df , r2.tmp)
    mse.df <- rbind(mse.df, mse.tmp)  
  }
}

# ------------

summary(imp.df)
summary(r2.df)
summary(mse.df)

r2.df[r2.df$Species=="QURU",]
r2.df[r2.df$Species=="QUPR",]
r2.df[r2.df$Species=="NYSY",]
r2.df[r2.df$Species=="ACRU",]

mse.df[mse.df$Species=="QURU",]
mse.df[mse.df$Species=="QUPR",]
mse.df[mse.df$Species=="NYSY",]
mse.df[mse.df$Species=="ACRU",]

###################################################



###################################################
# Importing and formatting model results 
###################################################
model.results <- read.csv("RandomForest_Results_AllModels.csv")
summary(model.results)
unique(model.results$variable)

var.orders <- read.csv("RandomForest_VariableOrders.csv")
var.orders$class.order1 <- as.factor(var.orders$class.order1)
var.orders$class.order2 <- as.factor(var.orders$class.order2)
var.orders$var.order <- as.factor(var.orders$var.order)
summary(var.orders)

var.topo <- c("elev", "slope", "TPI", "flow")
color.class1 <- c("green4", "blue", "red", "peru", "gray45")
color.class2 <- c("gray45", "peru", "red", "blue", "green4")


model.results <- merge(model.results, var.orders, all.x=T, all.y=T)
summary(model.results)

model.results$model.code <- recode(model.results$model, "'estab'='1'; 'current.estab'='2'; 'current.norm'='3'")
levels(model.results$model.code) <- c("Time", "Establishment", "Adult")
levels(model.results$class.order1) <- c("Disturbance", "Precipitation", "Temperature", "Soil", "Topogrpahy")
levels(model.results$class.order2) <- c("Topography", "Soil", "Temperature", "Precipitation", "Disturbance")
summary(model.results)


unique(model.results$variable)


pdf("RandomForestModels_Importance_All.pdf", height=7, width=10)
ggplot(data=model.results, aes(x=model.code, y=X.IncMSE)) + facet_grid(Spp ~ class.order1) +
	geom_boxplot() + 
	# geom_violin(adjust=2, scale="width") + 
	# geom_point(position=position_jitter(width=0.1, height=0.1), size=3) + 
	large.axes2 + 
	scale_y_continuous(name="% MSE Increase", limit=c(-100,500), breaks=seq(0,500,200)) + 
	scale_x_discrete(name="Model", labels=c("T", "E", "M"))
dev.off()

##################################################
# Agregating species responses to find cohesive patterns
##################################################
summary(model.results)

model.agg <- aggregate(model.results[,c("X.IncMSE", "IncNodePurity")], by=list(model.results$variable, model.results$var.name, model.results$var.order, model.results$model, model.results$model.code, model.results$class, model.results$class.order1, model.results$class.order2), FUN=mean)
names(model.agg) <- c("variable", "var.name", "var.order", "model","model.code", "class", "class.order1", "class.order2", "X.IncMSE", "IncNodePurity")
summary(model.agg)


model.agg.sd <- aggregate(model.results[,c("X.IncMSE", "IncNodePurity")], by=list(model.results$variable, model.results$var.name, model.results$var.order, model.results$model, model.results$model.code, model.results$class, model.results$class.order1, model.results$class.order2), FUN=sd)
names(model.agg.sd) <- c("variable", "var.name", "var.order","model","model.code", "class", "class.order1", "class.order2", "X.IncMSE.SD", "IncNodePurity.SD")
summary(model.agg.sd)

model.agg2 <- merge(model.agg, model.agg.sd)
summary(model.agg2)
# write.csv(model.agg, "RandomForest_Results_Aggregated.csv", row.names=F)

pdf("RandomForestModels_Importance_Aggregated_Violin.pdf", height=7, width=10)
ggplot(data=model.agg2, aes(x=model.code, y=X.IncMSE)) + facet_grid(. ~ class.order1) +
	geom_violin(adjust=2, scale="width") + 
	geom_point(position=position_jitter(width=0.1, height=0.1), size=3) + 
	large.axes2 + 
	scale_y_continuous(name="% MSE Increase", limit=c(-100,500), breaks=seq(0,500,200)) + 
	scale_x_discrete(name="Model", labels=c("T", "E", "M"))
dev.off()

vars <- c("Disturbance", "Temperature", "Precipitation")


pdf("RandomForestModels_Importance_Aggregated_Boxplots.pdf", height=7, width=10)
ggplot(data=model.agg2[,], aes(x=model.code, y=X.IncMSE)) + facet_grid(. ~ class.order1) +
	geom_boxplot(aes(fill=class.order1)) + 
	# geom_point(position=position_jitter(width=0.1, height=0.1), size=1, pch=1) + 
	large.axes2 + 
	scale_y_continuous(name="% MSE Increase") + 
	scale_x_discrete(name="Model",labels=c("T", "E", "M")) +
	scale_fill_manual(values=color.class1, name="Class") +
	guides(fill=F)
	# theme(legend.title=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.25)))
dev.off()

summary(model.agg2)

pdf("RandomForestModels_Importance_Aggregated_Simple.pdf", height=7, width=10)
ggplot(data=model.agg2[!(model.agg2$model.code=="Time") & (model.agg2$class.order1 %in% vars) & complete.cases(model.agg2),], aes(x=model.code, y=X.IncMSE)) + facet_grid(. ~ class.order1) +
	geom_boxplot(aes(fill=class.order1)) + 
	# geom_point(position=position_jitter(width=0.1, height=0.1), size=1, pch=1) + 
	large.axes2 + 
	scale_y_continuous(name="Predictor Importance") + 
	scale_x_discrete(name="Model", labels=c("Estab", "Adult")) + 
	scale_fill_manual(values=color.class1, name="Class") +
	guides(fill=F) 
	# +
	# theme(axis.text.x=element_text(angle=340, hjust=0.5, vjust=0.5))
dev.off()

pdf("RandomForestModels_Importance_Aggregated_Simple2.pdf", height=5, width=7)
ggplot(data=model.agg2[!(model.agg2$model.code=="Time") & (model.agg2$class.order1 %in% vars) & complete.cases(model.agg2),], aes(x=model.code, y=X.IncMSE)) + facet_grid(. ~ class.order1) +
	geom_boxplot(aes(fill=class.order1)) + 
	# geom_point(position=position_jitter(width=0.1, height=0.1), size=1, pch=1) + 
	large.axes2 + 
	# scale_y_continuous(name="Predictor Importance") + 
	# scale_x_discrete(name="Model", labels=c("Estab", "Adult")) + 
	scale_y_continuous(name="Distribution Importance") + 
	scale_x_discrete(name="Model", labels=c("Regen.", "Adult")) + 
	scale_fill_manual(values=color.class1, name="Class") +
	guides(fill=F) 
	# +
	# theme(axis.text.x=element_text(angle=340, hjust=0.5, vjust=0.5))
dev.off()



pdf("RandomForestModels_Importance_Aggregated_Simple_All.pdf", height=7, width=10)
ggplot(data=model.agg2[complete.cases(model.agg2),], aes(x=model.code, y=X.IncMSE)) + facet_grid(. ~ class.order1) +
	geom_boxplot(aes(fill=class.order1)) + 
	# geom_point(position=position_jitter(width=0.1, height=0.1), size=1, pch=1) + 
	large.axes2 + 
	scale_y_continuous(name="Predictor Importance") + 
	scale_x_discrete(name="Model", labels=c("Time", "Estab", "Adult")) + 
	scale_fill_manual(values=color.class1, name="Class") +
	guides(fill=F) +
	theme(axis.text.x=element_text(angle=340, hjust=0.5, vjust=0.5))
dev.off()



summary(var.orders)
var.orders <- var.orders[order(var.orders$var.order),]






pdf("RandomForestModels_Importance_Variables_Barplot.pdf", height=7, width=10)
ggplot(data=model.agg2[complete.cases(model.agg2),], aes(x=var.order, y=X.IncMSE, fill=class.order1)) + facet_grid(. ~ model.code, scales="free") + # Note: scales="free" does not work with non-cartesian coridantes with coords_flip; to make it work without flip, change to facet_wrap with ncol=1
	geom_bar(stat="identity", position="identity") + coord_flip() +
	# geom_vline(aes(xintercept=c(6.5, 10.5, 27.5))) + # Note: if 4 lines, it freaks out thinking it's a box
	# geom_vline(aes(xintercept=23.5)) +
	geom_errorbar(aes(ymin=X.IncMSE-X.IncMSE.SD, ymax=X.IncMSE+X.IncMSE.SD)) +
	large.axes2 + 
	scale_fill_manual(values=color.class1, name="Class") +
	scale_y_continuous(name="% MSE Increase", breaks=c(0, 150, 300)) + 
	scale_x_discrete(name="Predictor", labels=var.orders$var.code) + theme(axis.text.x=element_text(size=rel(2)), axis.text.y=element_text(size=rel(1)))
dev.off()

pdf("RandomForestModels_Importance_Variables_Boxplot.pdf", height=7, width=11)
ggplot(data=model.results[complete.cases(model.results),], aes(x=var.order, y=X.IncMSE, fill=class.order1)) + facet_grid(. ~ model.code, scales="free") + # Note: scales="free" does not work with non-cartesian coridantes with coords_flip; to make it work without flip, change to facet_wrap with ncol=1
	geom_boxplot() + coord_flip() +
	geom_hline(aes(yintercept=0), linetype="dashed", size=0.5) +
	large.axes2 + 
	scale_fill_manual(values=color.class1, name="Class") +
	scale_color_manual(values=color.class1, name="Class") +
	scale_y_continuous(name="% MSE Increase", breaks=c(0,200, 400)) + 
	scale_x_discrete(name="Predictor", labels=var.orders$var.code) + 	theme(axis.text.x=element_text(size=rel(2)), axis.text.y=element_text(size=rel(1)), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5))) +
	theme(legend.position=c(0.25, 0.3), legend.text=element_text(size=rel(1)), legend.title=element_text(size=rel(1))) + 
	theme(panel.border=element_rect(linetype="solid", fill=NA, size=1)) 
dev.off()



pdf("RandomForestModels_Importance_Aggregated_Boxplot2.pdf", height=7, width=10)
ggplot(data=model.agg2[complete.cases(model.agg2),], aes(x=class.order2, y=X.IncMSE, fill=class.order1)) + facet_grid(. ~ model.code, scales="free") + # Note: scales="free" does not work with non-cartesian coridantes with coords_flip; to make it work without flip, change to facet_wrap with ncol=1
	geom_boxplot() + coord_flip() +
	geom_hline(aes(yintercept=0), linetype="dashed", size=0.5) +
	large.axes2 + 
	scale_fill_manual(values=color.class1, name="Class") +
	scale_color_manual(values=color.class1, name="Class") +
	scale_y_continuous(name="% MSE Increase", limits=c(-25, 225), breaks=c(0,100,200)) + 
	scale_x_discrete(name="") + 
	theme(axis.text.x=element_text(size=rel(2)), axis.text.y=element_text(size=rel(2))) +
	theme(legend.text=element_text(size=rel(1)), legend.title=element_text(size=rel(1.25))) + 
	theme(panel.border=element_rect(linetype="solid", fill=NA, size=1)) 
dev.off()



####################################################################################################
# Plotting Species Separately for appendices
####################################################################################################
summary(model.results)

for(s in unique(model.results$Spp)){
	for(m in unique(model.results$model)){
		model.results[model.results$Spp==s & model.results$model==m,"imp.cutoff"] <- ifelse(min(model.results[model.results$Spp==s & model.results$model==m,"X.IncMSE"]) < 0, abs(min(model.results[model.results$Spp==s & model.results$model==m,"X.IncMSE"])), 0)
	}
}
summary(model.results)




pdf("RandomForestModels_Importance_Species_ACRU.pdf", height=10, width=9)
ggplot(data=model.results[complete.cases(model.results) & model.results$Spp=="ACRU",]) + facet_grid(. ~ model.code, scales="free") + # Note: scales="free" does not work with non-cartesian coridantes with coords_flip; to make it work without flip, change to facet_wrap with ncol=1
	geom_bar(aes(x=var.order, y=X.IncMSE, fill=class.order1), stat="identity", position="identity") + coord_flip() +
	geom_hline(aes(yintercept=imp.cutoff), linetype="dashed") + 
	large.axes2 + 
	scale_fill_manual(values=color.class1, name="Class") +
	scale_y_continuous(name="% MSE Increase", breaks=c(0, 150, 300)) + 
	scale_x_discrete(name="Predictor", labels=var.orders$var.code) + 	theme(axis.text.x=element_text(size=rel(2)), axis.text.y=element_text(size=rel(1.2)), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5))) +
	theme(legend.text=element_text(size=rel(1)), legend.title=element_text(size=rel(1.25))) + 
	theme(panel.border=element_rect(linetype="solid", fill=NA, size=1)) 
dev.off()


pdf("RandomForestModels_Importance_Species_BELE.pdf", height=10, width=9)
ggplot(data=model.results[complete.cases(model.results) & model.results$Spp=="BELE",]) + facet_grid(. ~ model.code, scales="free") + # Note: scales="free" does not work with non-cartesian coridantes with coords_flip; to make it work without flip, change to facet_wrap with ncol=1
	geom_bar(aes(x=var.order, y=X.IncMSE, fill=class.order1), stat="identity", position="identity") + coord_flip() +
	geom_hline(aes(yintercept=imp.cutoff), linetype="dashed") + 
	large.axes2 + 
	scale_fill_manual(values=color.class1, name="Class") +
	scale_y_continuous(name="% MSE Increase", breaks=c(0, 150, 300)) + 
	scale_x_discrete(name="Predictor", labels=var.orders$var.code) + 	theme(axis.text.x=element_text(size=rel(2)), axis.text.y=element_text(size=rel(1.2)), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5))) +
	theme(legend.text=element_text(size=rel(1)), legend.title=element_text(size=rel(1.25))) + 
	theme(panel.border=element_rect(linetype="solid", fill=NA, size=1)) 
dev.off()


pdf("RandomForestModels_Importance_Species_NYSY.pdf", height=10, width=9)
ggplot(data=model.results[complete.cases(model.results) & model.results$Spp=="NYSY",]) + facet_grid(. ~ model.code, scales="free") + # Note: scales="free" does not work with non-cartesian coridantes with coords_flip; to make it work without flip, change to facet_wrap with ncol=1
	geom_bar(aes(x=var.order, y=X.IncMSE, fill=class.order1), stat="identity", position="identity") + coord_flip() +
	geom_hline(aes(yintercept=imp.cutoff), linetype="dashed") + 
	large.axes2 + 
	scale_fill_manual(values=color.class1, name="Class") +
	scale_y_continuous(name="% MSE Increase", breaks=c(0, 150, 300)) + 
	scale_x_discrete(name="Predictor", labels=var.orders$var.code) + 	theme(axis.text.x=element_text(size=rel(2)), axis.text.y=element_text(size=rel(1.2)), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5))) +
	theme(legend.text=element_text(size=rel(1)), legend.title=element_text(size=rel(1.25))) + 
	theme(panel.border=element_rect(linetype="solid", fill=NA, size=1)) 
dev.off()

pdf("RandomForestModels_Importance_Species_QUPR.pdf", height=10, width=9)
ggplot(data=model.results[complete.cases(model.results) & model.results$Spp=="QUPR",]) + facet_grid(. ~ model.code, scales="free") + # Note: scales="free" does not work with non-cartesian coridantes with coords_flip; to make it work without flip, change to facet_wrap with ncol=1
	geom_bar(aes(x=var.order, y=X.IncMSE, fill=class.order1), stat="identity", position="identity") + coord_flip() +
	geom_hline(aes(yintercept=imp.cutoff), linetype="dashed") + 
	large.axes2 + 
	scale_fill_manual(values=color.class1, name="Class") +
	scale_y_continuous(name="% MSE Increase", breaks=c(0, 150, 300)) + 
	scale_x_discrete(name="Predictor", labels=var.orders$var.code) + 	theme(axis.text.x=element_text(size=rel(2)), axis.text.y=element_text(size=rel(1.2)), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5))) +
	theme(legend.text=element_text(size=rel(1)), legend.title=element_text(size=rel(1.25))) + 
	theme(panel.border=element_rect(linetype="solid", fill=NA, size=1)) 
dev.off()

pdf("RandomForestModels_Importance_Species_QURU.pdf", height=10, width=9)
ggplot(data=model.results[complete.cases(model.results) & model.results$Spp=="QURU",]) + facet_grid(. ~ model.code, scales="free") + # Note: scales="free" does not work with non-cartesian coridantes with coords_flip; to make it work without flip, change to facet_wrap with ncol=1
	geom_bar(aes(x=var.order, y=X.IncMSE, fill=class.order1), stat="identity", position="identity") + coord_flip() +
	geom_hline(aes(yintercept=imp.cutoff), linetype="dashed") + 
	large.axes2 + 
	scale_fill_manual(values=color.class1, name="Class") +
	scale_y_continuous(name="% MSE Increase", breaks=c(0, 200, 400)) + 
	scale_x_discrete(name="Predictor", labels=var.orders$var.code) + 	theme(axis.text.x=element_text(size=rel(2)), axis.text.y=element_text(size=rel(1.2)), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5))) +
	theme(legend.text=element_text(size=rel(1)), legend.title=element_text(size=rel(1.25))) + 
	theme(panel.border=element_rect(linetype="solid", fill=NA, size=1)) 
dev.off()

####################################################################################################
# Quantifying some model importances 
####################################################################################################
summary(model.results)

mean(model.results[model.results$model.code=="Time" & model.results$class=="Disturb", "X.IncMSE"]); sd(model.results[model.results$model.code=="Time" & model.results$class=="Disturb", "X.IncMSE"])

mean(model.results[model.results$model.code=="Time" & model.results$class=="Precip", "X.IncMSE"]); sd(model.results[model.results$model.code=="Time" & model.results$class=="Precip", "X.IncMSE"])

mean(model.results[model.results$model.code=="Time" & model.results$class=="Temp", "X.IncMSE"]); sd(model.results[model.results$model.code=="Time" & model.results$class=="Temp", "X.IncMSE"])

mean(model.results[model.results$model.code=="Time" & model.results$class=="Soil", "X.IncMSE"]); sd(model.results[model.results$model.code=="Time" & model.results$class=="Soil", "X.IncMSE"])

mean(model.results[model.results$model.code=="Time" & model.results$class=="Topo", "X.IncMSE"]); sd(model.results[model.results$model.code=="Time" & model.results$class=="Topo", "X.IncMSE"])

############
mean(model.results[model.results$model.code=="Establishment" & model.results$class=="Precip", "X.IncMSE"]); sd(model.results[model.results$model.code=="Establishment" & model.results$class=="Precip", "X.IncMSE"])

mean(model.results[model.results$model.code=="Establishment" & model.results$class=="Temp", "X.IncMSE"]); sd(model.results[model.results$model.code=="Establishment" & model.results$class=="Temp", "X.IncMSE"])

t.test(model.results[model.results$model.code=="Establishment" & model.results$class=="Precip", "X.IncMSE"], model.results[model.results$model.code=="Establishment" & model.results$class=="Temp", "X.IncMSE"], paired=T)

mean(model.results[model.results$model.code=="Establishment" & model.results$class=="Soil", "X.IncMSE"]); sd(model.results[model.results$model.code=="Establishment" & model.results$class=="Soil", "X.IncMSE"])

###########
mean(model.results[model.results$model.code=="Adult" & model.results$class=="Disturb", "X.IncMSE"]); sd(model.results[model.results$model.code=="Adult" & model.results$class=="Disturb", "X.IncMSE"])

mean(model.results[model.results$model.code=="Adult" & model.results$class=="Precip", "X.IncMSE"]); sd(model.results[model.results$model.code=="Adult" & model.results$class=="Precip", "X.IncMSE"])

mean(model.results[model.results$model.code=="Adult" & model.results$class=="Temp", "X.IncMSE"]); sd(model.results[model.results$model.code=="Adult" & model.results$class=="Temp", "X.IncMSE"])

mean(model.results[model.results$model.code=="Adult" & model.results$class=="Soil", "X.IncMSE"]); sd(model.results[model.results$model.code=="Adult" & model.results$class=="Soil", "X.IncMSE"])

mean(model.results[model.results$model.code=="Adult" & model.results$class=="Topo", "X.IncMSE"]); sd(model.results[model.results$model.code=="Adult" & model.results$class=="Topo", "X.IncMSE"])
