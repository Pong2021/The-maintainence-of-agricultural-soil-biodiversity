rm(list = ls())
setwd("F:/agricultural diveristy/datafilter")
library(skimr)
library (car)
load("data.RData")
https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html
https://github.com/drmarcogir/fagus



##### 1. agriculture total
agri_tot <- cbind(agri_local, natu_pool$bac_gamma, local_pool$bac_gamma)
colnames(agri_tot)[38:39] <- c("nature_gamma","local_gamma" )
#corr.test(agri_tot$bac_gamma, agri_tot[,38:39])


#agri_tot <- cbind(agri_local, natu_pool$bac_gamma)
#colnames(agri_tot)[38] <- "nature_gamma" 
colnames(agri_tot)
table(agri_tot$soiltype)

agri_tot$soiltype_num <- NA
agri_tot$soiltype_num[which(agri_tot$soiltype == "Alpine soil")] <- 1
agri_tot$soiltype_num[which(agri_tot$soiltype == "Anthrosols")] <- 2
agri_tot$soiltype_num[which(agri_tot$soiltype == "Arid soils")] <- 3
agri_tot$soiltype_num[which(agri_tot$soiltype == "Caliche Soils")] <- 4
agri_tot$soiltype_num[which(agri_tot$soiltype == "Ferralisols")] <- 5
agri_tot$soiltype_num[which(agri_tot$soiltype == "Hydromorphic soils")] <- 6
agri_tot$soiltype_num[which(agri_tot$soiltype == "Luvisols")] <- 7
agri_tot$soiltype_num[which(agri_tot$soiltype == "Saline soils")] <- 8
agri_tot$soiltype_num[which(agri_tot$soiltype == "semi-hydromorphic soil")] <- 9
agri_tot$soiltype_num[which(agri_tot$soiltype == "Semi-Luvisols")] <- 10
agri_tot$soiltype_num[which(agri_tot$soiltype == "Skeletol primitive soils")] <- 11

colnames(agri_tot)
# 5,6 Long and lati
# 8 plot size
# 9:14 soil factors
# 15:22 large factors
# 23 meanrichness
# 24 bac_gamma
# 25 completeness
# 32:37
# 38 nature_gamma
# 39 local_gamma
# soil type

da <- agri_tot
da[,c(4:5,9:22,32:37)] <- scale(da[,c(4:5,9:22,32:37)] )
da$bac_gamma <- da$bac_gamma/1000
da$nature_gamma <- da$nature_gamma/1000
da$local_gamma <- da$local_gamma/1000
da$meanrichness <- da$meanrichness/1000

####  RF and GLM
library(data.table)
library(h2o)
library(raster)
library(tictoc)
library(foreach)
library(doParallel)
library(tidyverse)
library(rfPermute)
library(A3)

### bac_gamma
# climate + soil + local
bands <- subset(da, select = c(pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, 
                               tot_pH_sd_log, tot_AP_sd_log, tot_OM_sd_log, tot_no3_sd_log, tot_nh4_sd_log, tot_moisture_sd_log, soiltype_num, local_gamma))
bandnames<-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'bac_gamma'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')
# rf_model<-h2o.randomForest(
#   y = vartomodel,
#   training_frame = regmatrixh2o,
#   ntrees = 100,
#   sample_rate = 0.632,
#   nfolds = 10,
#   fold_assignment = 'Modulo',
#   keep_cross_validation_predictions = TRUE,
#   seed = 0
# )
# p1<-h2o.varimp_plot(rf_model)
# h2o.r2(rf_model)
# p1
# print(rf_model)


# rf_model<-h2o.glm(
#   y = vartomodel,
#   training_frame = regmatrixh2o,
#   nfolds = 10,
#   fold_assignment = 'Modulo',
#   keep_cross_validation_predictions = TRUE,
#   seed = 0
# )
# 
# p1<-h2o.varimp_plot(rf_model)
# h2o.r2(rf_model)
# rf_model@model$coefficients_table
# h2o.std_coef_plot(rf_model)


rf_model<-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_bac_gamma_local.csv")
getwd()

# climate + soil
bands <- subset(da, select = c(pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, 
                               tot_pH_sd_log, tot_AP_sd_log, tot_OM_sd_log, tot_no3_sd_log, tot_nh4_sd_log, tot_moisture_sd_log, soiltype_num))
bandnames <-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'bac_gamma'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')

rf_model <-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_bac_gamma_climate_soil.csv")


# climate 
bands <- subset(da, select = c( pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, soiltype_num))
bandnames <-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'bac_gamma'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')

rf_model <-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_bac_gamma_climate.csv")






### meanrichness
# climate + soil + local
bands <- subset(da, select = c(pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, 
                               tot_pH_sd_log, tot_AP_sd_log, tot_OM_sd_log, tot_no3_sd_log, tot_nh4_sd_log, tot_moisture_sd_log, soiltype_num, local_gamma))
bandnames<-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'meanrichness'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')

rf_model<-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_meanrichness_local.csv")
getwd()

# climate + soil
bands <- subset(da, select = c(pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, 
                               tot_pH_sd_log, tot_AP_sd_log, tot_OM_sd_log, tot_no3_sd_log, tot_nh4_sd_log, tot_moisture_sd_log, soiltype_num))
bandnames <-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'meanrichness'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')

rf_model <-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_meanrichness_climate_soil.csv")


# climate 
bands <- subset(da, select = c( pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, soiltype_num))
bandnames <-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'meanrichness'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')

rf_model <-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_meanrichness_climate.csv")








### completeness
# climate + soil + local
bands <- subset(da, select = c(pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, 
                               tot_pH_sd_log, tot_AP_sd_log, tot_OM_sd_log, tot_no3_sd_log, tot_nh4_sd_log, tot_moisture_sd_log, soiltype_num, local_gamma))
bandnames<-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'completeness'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')

rf_model<-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_completeness_local.csv")
getwd()

# climate + soil
bands <- subset(da, select = c(pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, 
                               tot_pH_sd_log, tot_AP_sd_log, tot_OM_sd_log, tot_no3_sd_log, tot_nh4_sd_log, tot_moisture_sd_log, soiltype_num))
bandnames <-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'completeness'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')

rf_model <-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_completeness_climate_soil.csv")


# climate 
bands <- subset(da, select = c( pH, AP, OM, no3, nh4, moisture, SAT, SAP, TA, PA, soiltype_num))
bandnames <-names(bands)
print(bandnames)
rawmatrix <- da
vartomodel <- 'completeness'
regressionmatrix <- subset(rawmatrix, select = c(bandnames,vartomodel))
localH2O<-h2o.init(nthreads = 7, max_mem_size = '10g', ignore_config = TRUE)
regmatrixh2o <- as.h2o(regressionmatrix, destination_frame = 'regMatrixH2O')

rf_model <-h2o.glm(
  y = vartomodel,
  training_frame = regmatrixh2o,
  nfolds = 10,
  fold_assignment = 'Modulo',
  lambda = 0,
  compute_p_values = TRUE,
  keep_cross_validation_predictions = TRUE
)
p1<-h2o.varimp_plot(rf_model)
h2o.r2(rf_model)
rf_model@model$coefficients_table
rf_model@model$coefficients_table$standardized_coefficients
h2o.std_coef_plot(rf_model)
write.csv(rf_model@model$coefficients_table, file = "glm_completeness_climate.csv")



# #### Boosted Regression Tree models
# library(dismo);library(piecewiseSEM);library(XLConnect)
# library(lme4);library(pgirmess);library(vegan)
# library(stringi);library(nlme);library(MASS)
# library(stringr);library(boot);library(dismo)
# library(doMC);library(foreach);library(MASS)
# library(nlme);library(spdep);library(ggplot2);
# library(WriteXLS);
# install.packages("WriteXLS")
# install.packages("doMC", repos="http://R-Forge.R-project.org")
# 
# 
# da <- agri_tot
# table(da$plotsize)
# da$soiltype <- as.factor(da$soiltype)
# 
# # list of models 
# predresp<-list(
#   c("bac_gamma","soiltype","pH","AP","OM","no3","nh4","moisture","SAT","SAP","TA","PA",
#     "tot_moisture_sd","tot_pH_sd_log","tot_AP_sd_log","tot_OM_sd_log","tot_no3_sd_log","tot_nh4_sd_log", "tot_moisture_sd_log",
#     "local_gamma"),
#   c("bac_gamma","soiltype","pH","AP","OM","no3","nh4","moisture","SAT","SAP","TA","PA",
#     "tot_moisture_sd","tot_pH_sd_log","tot_AP_sd_log","tot_OM_sd_log","tot_no3_sd_log","tot_nh4_sd_log", "tot_moisture_sd_log"),
#   c("bac_gamma","soiltype","SAT","SAP","TA","PA",
#     "local_gamma"),
#   c("bac_gamma","soiltype","SAT","SAP","TA","PA"),
#   
#   c("meanrichness","soiltype","pH","AP","OM","no3","nh4","moisture","SAT","SAP","TA","PA",
#     "tot_moisture_sd","tot_pH_sd_log","tot_AP_sd_log","tot_OM_sd_log","tot_no3_sd_log","tot_nh4_sd_log", "tot_moisture_sd_log",
#     "local_gamma"),
#   c("meanrichness","soiltype","pH","AP","OM","no3","nh4","moisture","SAT","SAP","TA","PA",
#     "tot_moisture_sd","tot_pH_sd_log","tot_AP_sd_log","tot_OM_sd_log","tot_no3_sd_log","tot_nh4_sd_log", "tot_moisture_sd_log"),
#   c("meanrichness","soiltype","SAT","SAP","TA","PA",
#     "local_gamma"),
#   c("meanrichness","soiltype","SAT","SAP","TA","PA"),
#   
#   c("completeness","soiltype","pH","AP","OM","no3","nh4","moisture","SAT","SAP","TA","PA",
#     "tot_moisture_sd","tot_pH_sd_log","tot_AP_sd_log","tot_OM_sd_log","tot_no3_sd_log","tot_nh4_sd_log", "tot_moisture_sd_log",
#     "local_gamma"),
#   c("completeness","soiltype","pH","AP","OM","no3","nh4","moisture","SAT","SAP","TA","PA",
#     "tot_moisture_sd","tot_pH_sd_log","tot_AP_sd_log","tot_OM_sd_log","tot_no3_sd_log","tot_nh4_sd_log", "tot_moisture_sd_log"),
#   c("completeness","soiltype","SAT","SAP","TA","PA",
#     "local_gamma"),
#   c("completeness","soiltype","SAT","SAP","TA","PA")
#   )
# 
# fitbrt_wrapper<-function(inputdat,modlist,family){
#   # list where to store results
#   results<-vector("list",length(predresp))
#   # fit models according to list
#   for (i in 1:length(predresp)){
#     cols<-match(predresp[[1]],names(inputdat))
#     dat2<-inputdat[,cols]
#     dat3<-standat1(indatc=dat2,pred=names(dat2[2:10]))
#     dat4<-dat3[!is.na(dat3[,predresp[[i]][1]]),]
#     # fit model
#     mod<-gbm.step(data=dat4,gbm.x=match(predresp[[i]][-1],names(dat4)),gbm.y=match(predresp[[i]][1],
#                                                                                    names(dat4)),family=family,tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5,step.size=100)
#     # save object
#     objname<-paste("mod_",i,sep="")
#     save(mod,file=objname)
#     # extract variable importance
#     importance<-summary(mod,plotit=FALSE)
#     importance$rel.inf<-round(importance$rel.inf,digits=2)
#     # create title
#     resp<-paste(mod$gbm.call$response.name,"~",sep="")
#     pred<-paste(mod$gbm.call$predictor.names,collapse="+")
#     title<-paste(resp,pred,sep="")
#     # deviance explained
#     d2<-round(100-(mod$cv.statistics$deviance.mean*100)/mod$self.statistics$mean.null,digits=2)
#     # final data frame
#     df<-data.frame(model=title,importance,d2)
#     results[[i]]<-df
#   }
#   results1<-do.call("rbind",results)
#   return(results1)
# }
# 
# 
# 
# 
# 
# ??standat
# 
# 
# # fit models
# brtresults<-fitbrt_wrapper(inputdat=dat1,modlist=predresp)
# 
# 
# cols<-match(predresp[[4]],names(da))
# dat2<-da[,cols]
# #dat3<-standat1(indatc=dat2,pred=names(dat2[2:20]))
# #dat4<-dat3[!is.na(dat3[,predresp[[i]][1]]),]
# # fit model
# mod<-gbm.step(data=dat2,gbm.x=match(predresp[[4]][-1],names(dat2)),gbm.y=match(predresp[[4]][1],
#                                                                                names(dat2)),family="gaussian",tree.complexity = 5, learning.rate = 0.001)
# # save object
# objname<-paste("mod_",3,sep="")
# save(mod,file=objname)
# # extract variable importance
# importance<-summary(mod,plotit=FALSE)
# importance$rel.inf<-round(importance$rel.inf,digits=2)
# # create title
# resp<-paste(mod$gbm.call$response.name,"~",sep="")
# pred<-paste(mod$gbm.call$predictor.names,collapse="+")
# title<-paste(resp,pred,sep="")
# # deviance explained
# d2<-round(100-(mod$cv.statistics$deviance.mean*100)/mod$self.statistics$mean.null,digits=2)
# # final data frame
# df<-data.frame(model=title,importance,d2)
# results[[i]]<-df