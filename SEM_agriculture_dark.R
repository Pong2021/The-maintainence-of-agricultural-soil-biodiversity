rm(list = ls())
library(skimr)
library (car)
library(psych)
setwd("F:/1 agricultural diveristy/datafilter")
load("data.RData")



##### 1. agriculture total
agri_tot <- cbind(agri_local, natu_pool$bac_gamma, local_pool$bac_gamma)
colnames(agri_tot)[38:39] <- c("nature_gamma","local_gamma" )
corr.test(agri_tot$bac_gamma, agri_tot[,38:39])


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

skim(agri_tot)
str(agri_tot)
multi.hist(agri_tot[,c(4:5,9:22,32:37,40)])
multi.hist(agri_tot[,c(16)])


agri_dark <- read.csv("diversity_agriculture_site_bac_filter_38_dark.csv", row.names = 1)
agri_tot <- cbind(agri_tot, agri_dark[,38:40])
colnames(agri_tot)
# 23 meanrichness
# 24 bac_gamma
# 25 completeness
# 38 nature_gamma
# 39 local_gamma


corr.test(agri_tot[,c(4:5,9:22,32:37,39)], method = "pearson", adjust = "none")
colnames(agri_tot)
#cor(agri_tot[,c(4:5,9:22,32:37,39)], method = "pearson")
library(corrplot)
pdf("agri_local_corrplot.pdf",width = 16,height = 16)
par(mfrow = c(1,1))
corrplot(corr = cor(agri_tot[,c(4:5,9:22,32:37)],method = "pearson"),
         method = "color",type="upper", addCoef.col = "#9c6644",tl.col = "black" )
dev.off()



da <- agri_tot
da[,c(4:5,9:22,32:37)] <- scale(da[,c(4:5,9:22,32:37)] )
pdf("agri_local_corrplot_scale.pdf",width = 16,height = 16)
par(mfrow = c(1,1))
corrplot(corr = cor(da[,c(4:5,9:22,32:37)],method = "pearson"),
         method = "color",type="upper", addCoef.col = "#9c6644",tl.col = "black" )
dev.off()



library(lavaan)
library(semPlot)
da$bac_gamma <- da$bac_gamma/1000
da$nature_gamma <- da$nature_gamma/1000
da$local_gamma <- da$local_gamma/1000
#### 1. bac_gamma
alm <- lm(bac_gamma ~ pH + AP + OM + no3 + nh4 + moisture +  SAT +  SAP +  TA + PA +
            tot_pH_sd_log + tot_AP_sd_log + tot_OM_sd_log + tot_no3_sd_log + tot_nh4_sd_log + tot_moisture_sd_log + soiltype_num, data = da)
summary(alm)
vif(alm)

corr.test(da[,c(4:5,9:22,32:40)],da$bac_gamma, method = "pearson", adjust = "none")
#corr.test(agri_tot[,c(4:5,9:22,32:39)],agri_tot$bac_gamma, method = "pearson", adjust = "none")
ggplot(agri_tot, aes(x = nature_gamma, y = bac_gamma))+
  geom_point()+
  geom_smooth(method = "lm")


# model1 <- '
# bac_gamma ~ pH + AP + nh4 + moisture + MAP_curr + SAP  + PA + 
#              tot_pH_sd_log + tot_AP_sd_log + tot_OM_sd_log  + tot_nh4_sd_log
# pH ~  MAP_curr + SAP
# OM ~ SAT + SAP
# nh4 ~  MAP_curr
# pH ~~  moisture
# OM ~~ moisture + tot_OM_sd_log
# moisture ~ SAP
# tot_OM_sd_log ~ SAP
# tot_OM_sd_log  ~~ tot_pH_sd_log
# tot_pH_sd_log  ~  SAP
# tot_pH_sd_log  ~~  pH
# pH  ~ SAT
# nh4 ~~ pH
# '
# fit1 <- sem(model1, data=da,std.lv = TRUE)
# summary(fit1,standardized=T,fit.measures = TRUE, rsq=T)
# mi1 <- modindices(fit1)
# print(mi1[mi1$mi>3.0,])
# semPaths(fit1,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
# inspect(fit1, 'r2')



model2 <- '
bac_gamma ~ pH + AP  + nh4 + moisture +   SAP  + PA + TA + SAT + 
           tot_pH_sd_log  + tot_OM_sd_log  + tot_nh4_sd_log
pH ~  SAP
OM ~ SAT + SAP
moisture ~ SAP
tot_OM_sd_log ~ SAP
tot_pH_sd_log  ~  SAP
pH ~~  tot_pH_sd_log + OM + moisture
OM ~~       moisture
OM ~~  tot_OM_sd_log
tot_OM_sd_log ~~  tot_pH_sd_log
pH  ~   TA
pH ~~ nh4
nh4  ~  TA + SAP
'
fit2 <- sem(model2, data=da,std.lv = TRUE)
summary(fit2,standardized=T,fit.measures = TRUE, rsq=T)
mi2 <- modindices(fit2)
print(mi2[mi2$mi>3.0,])
semPaths(fit2,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
inspect(fit2, 'r2') # 0.627
fit2

# model3 <- '
# bac_gamma ~ pH + AP  + nh4 + moisture +  SAP  + PA + TA +
#            tot_pH_sd_log  + tot_OM_sd_log  + tot_nh4_sd_log + nature_gamma
# pH ~  SAP
# OM ~ SAT + SAP
# moisture ~ SAP
# tot_OM_sd_log ~ SAP
# tot_pH_sd_log  ~  SAP
# pH ~~  tot_pH_sd_log + OM + moisture
# OM ~~  moisture
# OM ~~  tot_OM_sd_log
# tot_OM_sd_log ~~  tot_pH_sd_log
# pH  ~   TA
# pH ~~ nh4
# nh4  ~  TA + SAP
# nature_gamma ~ SAT 
# '
# fit3 <- sem(model3, data=da,std.lv = TRUE)
# summary(fit3,standardized=T,fit.measures = TRUE, rsq=T)
# mi3 <- modindices(fit3)
# print(mi3[mi3$mi>3.0,])
# semPaths(fit3,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
# inspect(fit3, 'r2')



model4 <- '
bac_gamma ~ pH + AP  + nh4 + moisture +  SAP  + PA + TA + SAT +
           tot_pH_sd_log  + tot_OM_sd_log  + tot_nh4_sd_log + local_gamma
local_gamma ~ SAT + PA
pH ~  SAP
OM ~ SAT + SAP
moisture ~ SAP
tot_OM_sd_log ~ SAP
tot_pH_sd_log  ~  SAP
pH ~~  tot_pH_sd_log + OM + moisture
OM ~~  moisture
OM ~~  tot_OM_sd_log
tot_OM_sd_log ~~  tot_pH_sd_log
pH  ~   TA
pH ~~ nh4
nh4  ~  TA + SAP
'
fit4 <- sem(model4, data=da,std.lv = TRUE)
summary(fit4,standardized=T,fit.measures = TRUE, rsq=T)
mi4 <- modindices(fit4)
print(mi4[mi4$mi>3.0,])
semPaths(fit4,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
inspect(fit4, 'r2') # 0.829 




da$meanrichness <- da$meanrichness/1000
#### 2. bac_meanrichness
alm <- lm(meanrichness ~ pH + AP + OM + no3 + nh4 + moisture +  SAT +  SAP +  TA + PA +
            tot_pH_sd_log + tot_AP_sd_log + tot_OM_sd_log + tot_no3_sd_log + tot_nh4_sd_log + tot_moisture_sd_log + soiltype_num, data = da)
summary(alm)
vif(alm)

corr.test(da[,c(4:5,9:22,24,32:40)],da$meanrichness, method = "pearson", adjust = "none")
#corr.test(agri_tot[,c(4:5,9:22,32:39)],agri_tot$bac_gamma, method = "pearson", adjust = "none")
# ggplot(agri_tot, aes(x = nature_gamma, y = bac_gamma))+
#   geom_point()+
#   geom_smooth(method = "lm")


model1 <- '
meanrichness ~ pH    + nh4  + SAP   + TA + SAT + 
              + tot_AP_sd_log + tot_no3_sd_log 
pH ~  SAP
OM ~ SAT + SAP
moisture ~ SAP
tot_OM_sd_log ~ SAP
tot_pH_sd_log  ~  SAP
pH ~~  tot_pH_sd_log + OM + moisture
OM ~~       moisture
OM ~~  tot_OM_sd_log
tot_OM_sd_log ~~  tot_pH_sd_log
pH  ~   TA
pH ~~ nh4
nh4  ~  TA + SAP
'
fit1 <- sem(model1, data=da,std.lv = TRUE)
summary(fit1,standardized=T,fit.measures = TRUE, rsq=T)
mi1 <- modindices(fit1)
print(mi1[mi1$mi>3.0,])
semPaths(fit1,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
inspect(fit1, 'r2') # 0.617


# model11 <- '
# meanrichness ~ pH   + nh4   + TA + SAT + SAP +
#               + tot_AP_sd_log   + tot_OM_sd_log 
# pH ~  SAP
# OM ~ SAT + SAP
# moisture ~ SAP
# tot_OM_sd_log ~ SAP
# tot_pH_sd_log  ~  SAP
# pH ~~  tot_pH_sd_log + OM + moisture
# OM ~~       moisture
# OM ~~  tot_OM_sd_log
# tot_OM_sd_log ~~  tot_pH_sd_log
# pH  ~   TA
# pH ~~ nh4
# nh4  ~  TA + SAP
# '
# fit11 <- sem(model11, data=da,std.lv = TRUE)
# summary(fit11,standardized=T,fit.measures = TRUE, rsq=T)
# mi11 <- modindices(fit11)
# print(mi11[mi11$mi>3.0,])
# semPaths(fit11,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
# inspect(fit11, 'r2') # 0.617


# model3 <- '
# meanrichness ~ pH + AP  + nh4 + moisture +   SAP  + PA + TA + SAT + 
#            tot_pH_sd_log  + tot_OM_sd_log  + tot_nh4_sd_log
# pH ~  SAP
# OM ~ SAT + SAP
# moisture ~ SAP
# tot_OM_sd_log ~ SAP
# tot_pH_sd_log  ~  SAP
# pH ~~  tot_pH_sd_log + OM + moisture
# OM ~~       moisture
# OM ~~  tot_OM_sd_log
# tot_OM_sd_log ~~  tot_pH_sd_log
# pH  ~   TA
# pH ~~ nh4
# nh4  ~  TA + SAP
# '
# fit3 <- sem(model3, data=da,std.lv = TRUE)
# summary(fit3,standardized=T,fit.measures = TRUE, rsq=T)
# mi3 <- modindices(fit3)
# print(mi3[mi3$mi>3.0,])
# semPaths(fit3,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
# inspect(fit3, 'r2') # 0.555




model5 <- '
meanrichness ~ pH    + nh4  + SAP + TA + SAT + 
              + tot_AP_sd_log + tot_no3_sd_log + local_gamma
local_gamma ~ SAT + PA
pH ~  SAP
OM ~ SAT + SAP
moisture ~ SAP
tot_OM_sd_log ~ SAP
tot_pH_sd_log  ~  SAP
pH ~~  tot_pH_sd_log + OM + moisture
OM ~~  moisture
OM ~~  tot_OM_sd_log
tot_OM_sd_log ~~  tot_pH_sd_log
pH  ~   TA
pH ~~ nh4
nh4  ~  TA + SAP
'
fit5 <- sem(model5, data=da,std.lv = TRUE)
summary(fit5,standardized=T,fit.measures = TRUE, rsq=T)
mi5 <- modindices(fit5)
print(mi5[mi5$mi>3.0,])
semPaths(fit5,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
inspect(fit5, 'r2') # 0.724





#### 3. completeness
alm <- lm(complete ~ pH + AP + OM + no3 + nh4 + moisture +  SAT +  SAP +  TA + PA +
            tot_pH_sd_log + tot_AP_sd_log + tot_OM_sd_log + tot_no3_sd_log + tot_nh4_sd_log + tot_moisture_sd_log + soiltype_num, data = da)
summary(alm)
vif(alm)

corr.test(da[,c(4:5,9:22,23:24,32:40)],da$completeness, method = "pearson", adjust = "none")
#corr.test(agri_tot[,c(4:5,9:22,32:39)],agri_tot$bac_gamma, method = "pearson", adjust = "none")
# ggplot(agri_tot, aes(x = nature_gamma, y = bac_gamma))+
#   geom_point()+
#   geom_smooth(method = "lm")


model6 <- '
complete ~  moisture + no3
               + TA + SAT
              +  tot_AP_sd_log  + tot_no3_sd_log + tot_moisture_sd_log + tot_pH_sd_log
pH ~  SAP
OM ~ SAT + SAP
moisture ~ SAP
tot_OM_sd_log ~ SAP
tot_pH_sd_log  ~  SAP
pH ~~  tot_pH_sd_log + OM + moisture
OM ~~       moisture
OM ~~  tot_OM_sd_log
tot_OM_sd_log ~~  tot_pH_sd_log
pH  ~   TA
pH ~~ nh4
nh4  ~  TA + SAP
nh4  ~~ tot_moisture_sd_log
tot_moisture_sd_log  ~~ tot_no3_sd_log
tot_no3_sd_log  ~~ OM

'
fit6 <- sem(model6, data=da,std.lv = TRUE)
summary(fit6,standardized=T,fit.measures = TRUE, rsq=T)
mi6 <- modindices(fit6)
print(mi6[mi6$mi>3.0,])
semPaths(fit6,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
inspect(fit6, 'r2') # 0.474




model7 <- '
complete ~  moisture + no3
               + TA + SAT
              +  tot_AP_sd_log  + tot_no3_sd_log + tot_moisture_sd_log + tot_pH_sd_log + local_gamma
local_gamma ~ SAT + PA
pH ~  SAP
OM ~ SAT + SAP
moisture ~ SAP
tot_OM_sd_log ~ SAP
tot_pH_sd_log  ~  SAP
pH ~~  tot_pH_sd_log + OM + moisture
OM ~~       moisture
OM ~~  tot_OM_sd_log
tot_OM_sd_log ~~  tot_pH_sd_log
pH  ~   TA
pH ~~ nh4
nh4  ~  TA + SAP
nh4  ~~ tot_moisture_sd_log
tot_moisture_sd_log  ~~ tot_no3_sd_log
tot_no3_sd_log  ~~ OM
'
fit7 <- sem(model7, data=da,std.lv = TRUE)
summary(fit7,standardized=T,fit.measures = TRUE, rsq=T)
mi7 <-  modindices(fit7)
print(mi7[mi7$mi>3.0,])
semPaths(fit7,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
inspect(fit7, 'r2') # 0.526



# model8 <- '
# completeness ~ pH + nh4 + moisture + SAP +  PA  +
#               + tot_pH_sd_log + tot_AP_sd_log  + tot_no3_sd_log + tot_nh4_sd_log + tot_moisture_sd_log + bac_gamma
# pH ~  SAP
# OM ~ SAT + SAP
# moisture ~ SAP
# tot_OM_sd_log ~ SAP
# tot_pH_sd_log  ~  SAP
# pH ~~  tot_pH_sd_log + OM + moisture
# OM ~~       moisture
# OM ~~  tot_OM_sd_log
# tot_OM_sd_log ~~  tot_pH_sd_log
# pH  ~   TA
# pH ~~ nh4
# nh4  ~  TA + SAP
# nh4  ~~ tot_moisture_sd_log
# tot_moisture_sd_log  ~~ tot_no3_sd_log
# tot_no3_sd_log  ~~ OM
# tot_no3_sd_log ~ PA
# '
# fit8 <- sem(model8, data=da,std.lv = TRUE)
# summary(fit8,standardized=T,fit.measures = TRUE, rsq=T)
# mi8 <- modindices(fit8)
# print(mi8[mi8$mi>3.0,])
# semPaths(fit8,whatLabels = "std", layout="spring", edge.label.cex = 1.1,sizeMan = 11, intercepts=FALSE,nCharNodes=0)
# inspect(fit8, 'r2')