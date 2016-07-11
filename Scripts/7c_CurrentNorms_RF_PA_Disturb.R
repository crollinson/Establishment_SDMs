####################################################
# Random Forest Analysis of Species Distribution
# Response: Current Forest IV
# Predictors: weighted average from establishment events; (if IV = 0, climate norms & 2013 conditions)
# 
# Note: Compared with 2013 only Parameterzation & 30-year establishment/climate windows
# Note: Predictor variable suite based off of what is used by USFS for DISTRIB; modified to include competition/disturbance
####################################################
setwd("~/Desktop/Personal/Penn State/Research/PhD Research/CARCA/Establishment_Modeling")


# Clear workspace
rm(list=ls())
# option+command+L clears R console
# Load library to fit recursive partitioning and regression trees
library(randomForest)

set.seed(432) # Make results reproducible



RF_data <- read.csv('Data/model_inputs/CurrentComp_SpeciesGroup_30ynorms_RF.csv') # Data file
RF_data$Trans <- as.factor(substr(RF_data$PlotID, 4,4))

selLst = scan('Data/model_inputs/selvar_Current_Disturb.txt',what='string') # List of selected predictors; this is held constant across species
#nwdatv = read.csv('Cover_Climate_pred.csv') # Future climate predictors
respvar = 'IV.avg' # name of response variable: percentage of trees in the plot established in a given year
gisid = 'PlotID' # GIS-ID variable to map results - this is field should exist in the shapefile/coverage; Note: I'm just using it to tag indvidiual observations

species <- c("ACRU", "NYSY", "QUPR", "QURU") # Species to run through random forests
dat.out <- "Data/analyses/RF_Current_PA_Disturb/"
fig.out <- "Figures/RF_Current_PA_Disturb/"

if(!dir.exists(dat.out)) dir.create(dat.out, recursive=T)
if(!dir.exists(fig.out)) dir.create(fig.out, recursive=T)
############
############
for(s in species){
  
  print(paste0(" ============ ", s, " ============ "))
  
  # Initial data initialization stuff...
  incsvf <- RF_data[RF_data$Spp==s & complete.cases(RF_data),] # Subset only species of interest
  attach(incsvf)
  print(summary(incsvf[,]))
  print(dim(incsvf[]))
  
  
  respvarv = get(respvar)
  pred.df = data.frame(lapply(selLst,get))
  names(pred.df) = selLst
  gisidv = get(gisid) # Vector of GIS-ID
  
  # Automate tuning of mtry parameters...optional ; if not use mtry=(1/3)*numOfPredictors
  stst = trunc(sqrt(ncol(pred.df)))
  mt = tuneRF(pred.df, respvarv,ntreeTry=5000,mtrystart=stst)
  mt = mt[order(mt[,2]),]
  rf_mtry = mt[1,1] # Pick mtry with lowest OOB Error
  
  # Run RF model
  # Change the parameters as you see fit
  # NOTE: If you have memory problems, use 64-bit R on a 64bit OS with atleast 8 GB of memory - or you can try with smaller ntree
  rf.mod = randomForest(pred.df, respvarv, ntree=500000, mtry=rf_mtry, importance=TRUE, nodesize=5,keep.forest=TRUE,replace=TRUE,corr.bias=FALSE,proximity=FALSE,oob.prox=FALSE)
  
  # # Predict (omitted from description phase)
  # rf.curprd = round(na2zero(predict(rf.mod)), 4) # Current 
  # rf.newprd = round(na2zero(predict(rf.mod, newdata=nwdatv)),4) # Future Climate
  
  # # Assemble predictions and output to file -> Model_rf_prd.csv
  # rf.prdall = data.frame(gisidv,respvarv,rf.curprd,rf.newprd)
  # names(rf.prdall) = c(gisid,respvar,'rfCurPrd','rfNewPrd')
  # prdf = paste('TreeLarge_Model_All_prd','.csv',sep="")
  # write.table(format(rf.prdall,dig=4,trim=T), 
  # file=prdf,quote=F,sep=",",row.names=F,col.names=T,dec=".")
  
  # Calculate R-square
  rf_rsq = sort(rf.mod$rsq)
  rf_rsq = rf_rsq[length(rf_rsq)]
  
  # VariIMP plot -> Model_rf_varimplot.png
  varImpPlot(rf.mod)
  vipltn = paste0(fig.out, 'Current_Estab_',s, '_varimplot','.png')
  dev.print(png, vipltn, width=550, height=600)
  
  # MSE Plot -> Model_rf_mseplot.png
  mse = rf.mod$mse
  plot(mse, type='l', xlab="no. of trees", ylab="MSE(OOB)")
  msepltn = paste(fig.out,'Current_Estab_',s,'_mseplot','.png',sep="")
  dev.print(png, msepltn, width=550, height=600)
  
  # Variable Importance table -> Model_rf_vitable.csv
  vitabn = paste(dat.out, 'Current_Estab_',s,'_vitable','.csv',sep="")
  sink(vitabn)
  rf_vi = data.frame(importance(rf.mod))  
  rf_vis = rf_vi[rev(order(rf_vi[,1])),] # Sort on IncMSE
  print(rf_vis)
  sink()
  
  # Summary Statistics -> Model_rf_summ.txt
  rf_sumf = paste(dat.out, 'Current_Estab_',s,'_summ','.txt',sep="")
  sink(rf_sumf)
  print(rf.mod$call)
  print("R-squared")
  print(paste0("Mean (SD): ", round(mean(rf.mod$rsq), 4), " (", round(sd(rf.mod$rsq), 4), ")"))
  print(paste0("Max: ", round(max(rf.mod$rsq), 4)))
  print("")
  print("MSE")
  print(paste0("Mean (SD): ", round(mean(rf.mod$mse), 4), " (", round(sd(rf.mod$mse), 4), ")"))
  print(paste0("Min: ", round(min(rf.mod$mse), 4)))
  print(summary(rf.mod))
  print(rf_mtry)
  sink()
  
  save(rf.mod, file=paste0(dat.out, s, "_RFout.Rdata"))
  detach("incsvf")
  
  rm(pred.df, rf.mod)
}
