rm(list = ls())
setwd("F:/agricultural diveristy/datafilter")
library(skimr)
library (car)
load("data.RData")


##### 1. agriculture total
agri_tot <- cbind(agri_local, natu_pool$bac_gamma, local_pool$bac_gamma)
colnames(agri_tot)[38:39] <- c("nature_gamma","local_gamma" )

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

### agricultural total species
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






### community diversity
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



### community completeness
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
