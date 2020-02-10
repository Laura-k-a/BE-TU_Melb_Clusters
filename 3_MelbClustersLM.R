# Title:                  Melbourne BE-TR Clustered stops linear Model
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script performs pretreatment (factor analysis) followed by multivariate multiple linear regression of built environment and sociodemographic variables on transit ridership for a sample of tram and bus locations in Melbourne. 
# Data:                   Ridership data includes average normal (school) weekday ridership 
#                         Train, Tram ridership, averaged for 2018, by Victorian Department of Transport. 
#                         Bus ridership, averaged for 4 months from August - November 2018, provided by Chris Loader from the bus planning team at the Victorian Department of Transport
#                         Refer to [insert doi for figshare] for ontology and reference for built environment.
#                         Copyright statement: This script is the product of Laura Aston


#This is the script for the third run through of factor and cluster analysis, involving
#modes: train/tram/bus
#variables: all
#unit of analysis: 800m (train)/600 (tram)/ 400 (bus)
#outlier: 
#Sampling: Generate clusters; sample from 'multimodal' clusters to achieve balanced covariates. # of stops to take from each cluster   constrained by the mode of which there is the least number of stops of that type in each cluster. Non-limiting modes are randomyl sampled from the clusters. 
#analysis method: Linear regression


install.packages("tidyverse")  # data manipulation
install.packages("cluster")  # clustering algorithms
install.packages("factoextra") # clustering visualization
install.packages("gridExtra")
library(tidyverse)  # data manipulation
library(cluster)   # clustering algorithms
library(factoextra) # clustering visualization
library(gridExtra)
library(dplyr)# mutate function
library(car)#VIF function
library(Hmisc)#rcorr function
library(lm.beta)

options(max.print= 1000000)

setwd("C:/Users/lkast1/Google Drive/PhD/2.Analysis/2. Empirical Analysis/BE-TR_Multi Country Samples/Melbourne/Melb.All.Stops/Melb.AllStops.Repo/Data")
Melb_Data<- read.csv("BE-TR_AllStops_data.csv", header=TRUE, sep=",")
row.names(Melb_Data) <- Melb_Data[,c(2)]

Allmodes.Melb.800<- Melb_Data[which(Melb_Data$Target.break=='800'),]

Allmodes.Melb.800.test<-sample(Allmodes.Melb.800$OBJECTID_MODE, 5314, replace = FALSE, prob = NULL)
Allmodes.Melb.800.test<-as.data.frame(Allmodes.Melb.800.test)
Allmodes.Melb.800.test<-Allmodes.Melb.800.test %>% 
  rename(OBJECTID_MODE=Allmodes.Melb.800.test 
  )
Allmodes.Melb.800.test<-Allmodes.Melb.800.test %>%
  select(OBJECTID_MODE)%>%
  left_join(Allmodes.Melb.800, by="OBJECTID_MODE")

#scale and select numeric columns
#all var
#1ln.X1ln.ln_Patronage ~ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv X11_PedConnect + X12_PBN + X13_DestScore + X16_CBDDist + X17_ACDist + X21_PropUrban + X22_EmpAccess + X28_PropFTE + X29_MeanSize + X31_PropOS + X32_PropBach +  ln_pop

Melb.800.Cluster.Z<-scale(Allmodes.Melb.800.test[,c(24, 26:31, 34, 35, 39, 40, 49, 50, 54, 55, 56)])

k3.Melb.800.Cluster.Z <- kmeans(Melb.800.Cluster.Z, centers = 3, nstart = 25)
k4.Melb.800.Cluster.Z <- kmeans(Melb.800.Cluster.Z, centers = 4, nstart = 25)
k5.Melb.800.Cluster.Z <- kmeans(Melb.800.Cluster.Z, centers = 5, nstart = 25)
k6.Melb.800.Cluster.Z <- kmeans(Melb.800.Cluster.Z, centers = 6, nstart = 25)
k7.Melb.800.Cluster.Z <- kmeans(Melb.800.Cluster.Z, centers = 7, nstart = 25)
k8.Melb.800.Cluster.Z <- kmeans(Melb.800.Cluster.Z, centers = 8, nstart = 25)
k9.Melb.800.Cluster.Z <- kmeans(Melb.800.Cluster.Z, centers = 9, nstart = 25)
k10.Melb.800.Cluster.Z <- kmeans(Melb.800.Cluster.Z, centers = 10, nstart = 25)

#to change legend labels: labs(title = "Temperatures\n", x = "TY [°C]", y = "Txxx")
#https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot
# plots to compare
p3Test <- fviz_cluster(k3.Melb.800.Cluster.Z, geom = "point", data = Melb.800.Cluster.Z) + ggtitle("k = 3 (n = 5314)")

p4Test <- fviz_cluster(k4.Melb.800.Cluster.Z, geom = "point", data = Melb.800.Cluster.Z) + ggtitle("k = 4 (n = 5314)")

p5Test <- fviz_cluster(k5.Melb.800.Cluster.Z, geom = "point", data = Melb.800.Cluster.Z) + ggtitle("k = 5 (n = 5314)")

p6Test <- fviz_cluster(k6.Melb.800.Cluster.Z, geom = "point", data = Melb.800.Cluster.Z) + ggtitle("k = 6 (n = 5314)")

p7Test <- fviz_cluster(k7.Melb.800.Cluster.Z, geom = "point", data = Melb.800.Cluster.Z) + ggtitle("k = 7 (n = 5314)")

p8Test <- fviz_cluster(k8.Melb.800.Cluster.Z, geom = "point", data = Melb.800.Cluster.Z) + ggtitle("k = 8 (n = 5314)")

p9Test <- fviz_cluster(k9.Melb.800.Cluster.Z, geom = "point", data = Melb.800.Cluster.Z) + ggtitle("k = 9 (n = 5314)")

p10Test <- fviz_cluster(k10.Melb.800.Cluster.Z, geom = "point", data = Melb.800.Cluster.Z) + ggtitle("k = 10 (n = 5314)")

grid.arrange(p3Test, p4Test, p5Test, p6Test, nrow=2)
#3 or 6-cluster solution

grid.arrange(p7Test, p8Test, p9Test, p10Test, nrow=2)
#8-cluster solution

grid.arrange(p3Test, p6Test, p7Test, nrow=1)
#6-cluster soution

#run for all data to validate
Melb.800.Cluster.Z.validate<-scale(Allmodes.Melb.800[,c(24, 26:31, 34, 35, 39, 40, 49, 50, 54, 55, 56)])
row.names(Melb.800.Cluster.Z.validate)<-Allmodes.Melb.800[,c(2)]

k3.Melb.800.Cluster.Z.validate <- kmeans(Melb.800.Cluster.Z.validate, centers = 3, nstart = 25)
k4.Melb.800.Cluster.Z.validate <- kmeans(Melb.800.Cluster.Z.validate, centers = 4, nstart = 25)
k5.Melb.800.Cluster.Z.validate <- kmeans(Melb.800.Cluster.Z.validate, centers = 5, nstart = 25)
k6.Melb.800.Cluster.Z.validate <- kmeans(Melb.800.Cluster.Z.validate, centers = 6, nstart = 25)


p3_All_Validate <- fviz_cluster(k3.Melb.800.Cluster.Z.validate, geom = "point", data = Melb.800.Cluster.Z.validate) + ggtitle("k = 3 (n = 10,629)")

p4_All_Validate <- fviz_cluster(k4.Melb.800.Cluster.Z.validate, geom = "point", data = Melb.800.Cluster.Z.validate) + ggtitle("k = 4 (n = 10,629)")

p5_All_Validate <- fviz_cluster(k5.Melb.800.Cluster.Z.validate, geom = "point", data = Melb.800.Cluster.Z.validate) + ggtitle("k = 5 (n = 10,629)")

p6_All_Validate <- fviz_cluster(k6.Melb.800.Cluster.Z.validate, geom = "point", data = Melb.800.Cluster.Z.validate) + ggtitle("k = 6 (n = 10,629)")

grid.arrange(p3Test, p4Test, p5Test, p6Test, p3_All_Validate ,p4_All_Validate, p5_All_Validate, p6_All_Validate , nrow=2)
#Try both 5 and 6 cluster solution for interpretability

Melb.800.Cluster.Z.validate<-as.data.frame(Melb.800.Cluster.Z.validate)
row.names(Melb.800.Cluster.Z.validate)<-Allmodes.Melb.800[,c(2)]

Melb.800.Cluster.Z6.validate<-Melb.800.Cluster.Z.validate %>%
  mutate(Cluster = k6.Melb.800.Cluster.Z.validate$cluster,
         OBJECTID_MODE = row.names(Melb.800.Cluster.Z.validate),)
row.names(Melb.800.Cluster.Z6.validate)<-Allmodes.Melb.800[,c(2)]

Melb.800.Cluster.Z5.validate<-Melb.800.Cluster.Z.validate %>%
  mutate(Cluster = k5.Melb.800.Cluster.Z.validate$cluster,
         OBJECTID_MODE = row.names(Melb.800.Cluster.Z.validate),)
row.names(Melb.800.Cluster.Z5.validate)<-Allmodes.Melb.800[,c(2)]

capture.output(Melb.800.Cluster.Z6.validate,file="k6_ClusterMembership.csv")
capture.output(print(k6.Melb.800.Cluster.Z.validate),file ="k6_ClusterCentroids.csv")
capture.output(Melb.800.Cluster.Z5.validate,file="k5_ClusterMembership.csv")
capture.output(print(k5.Melb.800.Cluster.Z.validate),file ="k5_ClusterCentroids.csv")


#Refer to sampling script
Melb_Data<- read.csv("BE-TR_AllStops_data.csv", header=TRUE, sep=",")
row.names(Melb_Data) <- Melb_Data[,c(2)]

Clustersample.bus.400<- Melb_Data[which (Melb_Data$Mode=='bus'
                                                  & Melb_Data$Sample=='Yes'),]
row.names(Clustersample.bus.400) <- Clustersample.bus.400[,c(2)]
Clustersample.tram.600<- Melb_Data[which (Melb_Data$Mode=='tram'
                                          & Melb_Data$Sample=='Yes'),]
row.names(Clustersample.tram.600) <- Clustersample.tram.600[,c(2)]
Clustersample.train.800<- Melb_Data[which (Melb_Data$Mode=='train'
                                           &Melb_Data$Sample=='Yes'),]
row.names(Clustersample.train.800) <- Clustersample.train.800[,c(2)]

#Col headers: X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X27_O_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach

#Bus
#step 3 Check for multicolinearity
Bus.cluster.LM.VIF<-vif(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X15_Parkiteer + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Clustersample.bus.400))
Bus.cluster.LM.VIF
#ln_pop is 5.127; leave for now as density indicator


#step 4 Simple correlations
Corrdata.buscluster<-Clustersample.bus.400[,c(52, 54, 24, 26, 27:31, 33:44, 49, 50)]

#Option 1 for Correlation matrices with p-values
Corrdata.buscluster<-rcorr(as.matrix(Corrdata.buscluster))

#option 2 for flat correlation matrix
#Set up a custom function to flatten
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

options(max.print=1000000)

Corrdata.buscluster<-flattenCorrMatrix(Corrdata.buscluster$r,Corrdata.buscluster$P)
capture.output(Corrdata.buscluster,file="Corrdata.buscluster.csv")

#not significant for ln_bus
#X9_LUEntropy
#X15_Parkiteer
#X24_O_Bus_LOS


setwd("C:\Users\lkast1\Google Drive\PhD\2.Analysis\2. Empirical Analysis\BE-TR_Multi Country Samples\Melbourne\Melb.All.Stops\Melb.AllStops.Repo\Regression outputs\Cluster")

#Step 4 maximally adjusted model
Melb.buscluster.LM.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Clustersample.bus.400)
summary(Melb.buscluster.LM.1)

Melb.buscluster.LM.1<-lm.beta(Melb.buscluster.LM.1)
capture.output(summary(Melb.buscluster.LM.1), file = "buscluster.MA.txt")

#function for backward (starts with maximal model)
Melb.buscluster.LM.1.regb<-step(Melb.buscluster.LM.1,
                                  direction = "backward",
                                  trace = 0) #don't print steps
summary(Melb.buscluster.LM.1.regb)

Melb.buscluster.LM.1.regb<-lm.beta(Melb.buscluster.LM.1.regb)
capture.output(summary(Melb.buscluster.LM.1.regb), file = "buscluster.PM.txt")

#diagnostics
par(mfrow=c(2,2))
plot(Melb.buscluster.LM.1.regb)
#866-bus approaching cooks distance, but generally fits assumptions


#Tram
#step 3 Check for multicolinearity
Tram.cluster.LM.VIF<-vif(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X21_PropUrban + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X26_O_Train_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X31_PropOS + X32_PropBach, data =Clustersample.tram.600))
#removed parkiteer to get rid of singularity
Tram.cluster.LM.VIF
#ln_pop is 5.127; leave for now as density indicator

#step 4 Simple correlations
Corrdata.tramcluster<-Clustersample.tram.600[,c(52, 54, 24, 26, 27:31, 34:44, 49, 50)]

#Option 1 for Correlation matrices with p-values
Corrdata.tramcluster<-rcorr(as.matrix(Corrdata.tramcluster))



#option 2 for flat correlation matrix
#Set up a custom function to flatten
options(scipen = 1)
#option(scipen = 999)

Corrdata.tramcluster<-flattenCorrMatrix(Corrdata.tramcluster$r,Corrdata.tramcluster$P)
capture.output(Corrdata.tramcluster,file="Corrdata.tramcluster.csv")


#not significant for ln_tram
#X8_Balance
#X9_LUEntropy
#X17_ACDist
#X20.Parking_m.2
#X21_PropUrban
#X24_O_Bus_LOS
#X26_O_Train_LOS
#X31_PropOS


#MA tram
Melb.tramcluster.LM.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X18_ACCount + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X32_PropBach, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.1)

Melb.tramcluster.LM.1<-lm.beta(Melb.tramcluster.LM.1)
capture.output(Melb.tramcluster.LM.1, file = "tramcluster.MA.txt")

Melb.tramcluster.LM.1.regb<-step(Melb.tramcluster.LM.1,
                                  direction = "backward",
                                  trace = 0) #don't print steps

summary(Melb.tramcluster.LM.1.regb)

Melb.tramcluster.LM.1.regb<-lm.beta(Melb.tramcluster.LM.1.regb)
capture.output(summary(Melb.tramcluster.LM.1.regb), file = "tramcluster.PM.txt")

#diagnostics
par(mfrow=c(2,2))
plot(Melb.tramcluster.LM.1.regb)
#278 is influential outlier

which(rownames(Clustersample.tram.600) == "278-tram") #81

Clustersample.tram.600.rd2 <- Clustersample.tram.600[-c(81),]

#MA tram rd 2
Melb.tramcluster.LM.2<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop + X6_PropComm + X10_HousingDiv + X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist +  X18_ACCount + X22_EmpAccess + X23_C_LOS + X25_O_Tram_LOS + X28_Censored_PropFTE	+ X29_Censored_MeanSize + X32_PropBach, data =Clustersample.tram.600.rd2)
summary(Melb.tramcluster.LM.2)

Melb.tramcluster.LM.2<-lm.beta(Melb.tramcluster.LM.2)
capture.output(Melb.tramcluster.LM.2, file = "tramcluster.MA.2.txt")

Melb.tramcluster.LM.2.regb<-step(Melb.tramcluster.LM.2,
                                 direction = "backward",
                                 trace = 0) #don't print steps

summary(Melb.tramcluster.LM.2.regb)

Melb.tramcluster.LM.2.regb<-lm.beta(Melb.tramcluster.LM.2.regb)
capture.output(summary(Melb.tramcluster.LM.2.regb), file = "tramcluster.PM.2.txt")

#diagnostics
par(mfrow=c(2,2))
plot(Melb.tramcluster.LM.2.regb)
#Fits assumption. Most outlying observatoins are: 2213, 1913, 214)

#Train
#step 3 Check for multicolinearity
Train.cluster.LM.VIF<-vif(lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X12_CycleConnect + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X28_PropFTE	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Clustersample.train.800))
#removed  overlapping (total) level of service and overlapping trian to get rid of singularity
Train.cluster.LM.VIF
#removed Prop Urban to get rid of pop density VIF


#step 4 Simple correlations
Corrdata.traincluster<-Clustersample.train.800[,c(52, 54, 24, 26:31, 33:36, 38, 40:43, 46, 47, 49, 50)]

#Option 1 for Correlation matrices with p-values
Corrdata.traincluster<-rcorr(as.matrix(Corrdata.traincluster))

#option 2 for flat correlation matrix
#Set up a custom function to flatten
options(scipen = 1)
#option(scipen = 999)

Corrdata.traincluster<-flattenCorrMatrix(Corrdata.traincluster$r,Corrdata.traincluster$P)
capture.output(Corrdata.traincluster,file="Corrdata.traincluster.csv")

#not significant for ln_train
#X12_CycleConnect
#X28_PropFTE

#maxmially adjusted model
Melb.traincluster.LM.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X28_PropFTE	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Clustersample.train.800)
summary(Melb.traincluster.LM.1)

Melb.traincluster.LM.1<-lm.beta(Melb.traincluster.LM.1)
capture.output(summary(Melb.traincluster.LM.1), file = "traincluster.MA.txt")

Melb.traincluster.LM.1.regb<-step(Melb.traincluster.LM.1,
                                 direction = "backward",
                                 trace = 0) #don't print steps
summary(Melb.traincluster.LM.1.regb)

Melb.traincluster.LM.1.regb<-lm.beta(Melb.traincluster.LM.1.regb)
capture.output(summary(Melb.traincluster.LM.1.regb), file = "traincluster.PM.txt")

#diagnostic
plot(Melb.traincluster.LM.1.regb)
#signs of heteroskedasticity with most outlying 597, 628, 621

which(rownames(Clustersample.train.800) == "597-train") #15
which(rownames(Clustersample.train.800) == "628-train") #195
which(rownames(Clustersample.train.800) == "621-train") #2


Clustersample.train.800.rd2 <- Clustersample.train.800[-c(15, 195, 2),]

Melb.traincluster.LM.2.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X28_PropFTE	+ X29_MeanSize + X31_PropOS + X32_PropBach, data = Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.1)

Melb.traincluster.LM.2.1<-lm.beta(Melb.traincluster.LM.2.1)
capture.output(summary(Melb.traincluster.LM.2.1), file = "traincluster.MA.rd.2.txt")

Melb.traincluster.LM.2.1.regb<-step(Melb.traincluster.LM.2.1,
                                  direction = "backward",
                                  trace = 0) #don't print steps
summary(Melb.traincluster.LM.2.1.regb)
#actually reduced the explanatory power by removing the outliers. 

Melb.traincluster.LM.2.1<-lm.beta(Melb.traincluster.LM.2.1)
capture.output(summary(Melb.traincluster.LM.2.1), file = "traincluster.PM.rd.2.txt")

#diagnostics
plot(Melb.traincluster.LM.2.1.regb)
#609, affecting assumptions. 609, 627 and 4 bordering toCook's distance --> remove

which(rownames(Clustersample.train.800.rd2) == "609-train") #1 
which(rownames(Clustersample.train.800.rd2) == "627-train") #2 
which(rownames(Clustersample.train.800.rd2) == "4-train") #186

Clustersample.train.800.rd3 <- Clustersample.train.800.rd2[-c(1, 2, 186),]

#rd 3 maximally adjusted
Melb.traincluster.LM.3.1<-lm(X1ln.ln_Patronage ~ X3ln.ln_Pop+ X6_PropComm + X8_Balance + X9_LUEntropy + X10_HousingDiv +  X11_IntDensity + X13_DestScore + X16_CBDDist + X15_Parkiteer + X17_ACDist + X18_ACCount + X20.Parking_m.2 + X22_EmpAccess + X23_C_LOS + X24_O_Bus_LOS + X25_O_Tram_LOS + X28_PropFTE	+ X29_MeanSize + X31_PropOS + X32_PropBach, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.1)

Melb.traincluster.LM.3.1<-lm.beta(Melb.traincluster.LM.3.1)
capture.output(summary(Melb.traincluster.LM.3.1), file = "traincluster.MA.rd.3.txt")

Melb.traincluster.LM.3.1.regb<-step(Melb.traincluster.LM.3.1,
                                    direction = "backward",
                                    trace = 0) #don't print steps
summary(Melb.traincluster.LM.3.1.regb)


Melb.traincluster.LM.3.1.regb<-lm.beta(Melb.traincluster.LM.3.1.regb)
capture.output(summary(Melb.traincluster.LM.3.1.regb), file = "traincluster.PM.rd.3.txt")

plot(Melb.traincluster.LM.3.1.regb)
#467, 9 and 569 are outliers; Sclae-location is only graph not holding to assumptions. Retain. 