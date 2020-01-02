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


#This is the script for the first run through of factor and cluster analysis, involving
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
#26_PropComm	28_Balance31_PedConnect	32_PBN	35_Parkiteer	37_ACDist	38_ACCount	39 _FTZ	40_Parking	41_PropUrban	42_PropRural	43_EmpAccess	44_C_LOS	45_O_Bus_LOS	46_O_Tram_LOS	47_O_Train_LOS	48_O_LOS	49_PropFTE	50_MeanSize	51_MedInc	52_PropOS	53_PropBach Factor 1 Factor 2 Factor 3
Melb.800.BE.Cluster.Z<-scale(Allmodes.Melb.800.test[,c(26, 28, 31, 32, 37, 40:43, 59:61)])

k3.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 3, nstart = 25)
k4.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 4, nstart = 25)
k5.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 5, nstart = 25)
k6.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 6, nstart = 25)
k7.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 7, nstart = 25)
k8.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 8, nstart = 25)
k9.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 9, nstart = 25)
k10.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 10, nstart = 25)

#to change legend labels: labs(title = "Temperatures\n", x = "TY [°C]", y = "Txxx")
#https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot
# plots to compare
p3Test <- fviz_cluster(k3.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 3 (n = 5314)")

p4Test <- fviz_cluster(k4.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 4 (n = 5314)")

p5Test <- fviz_cluster(k5.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 5 (n = 5314)")

p6Test <- fviz_cluster(k6.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 6 (n = 5314)")

p7Test <- fviz_cluster(k7.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 7 (n = 5314)")

p8Test <- fviz_cluster(k8.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 8 (n = 5314)")

p9Test <- fviz_cluster(k9.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 9 (n = 5314)")

p10Test <- fviz_cluster(k10.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 10 (n = 5314)")

grid.arrange(p3Test, p4Test, p5Test, p6Test, nrow=2)
#4-cluster solution

grid.arrange(p7Test, p8Test, p9Test, p10Test, nrow=2)
#4-cluster solution

#run for all data to validate
Melb.800.BE.Cluster.Z.validate<-scale(Allmodes.Melb.800[,c(26, 28, 31, 32, 37, 40:43, 59:61)])
row.names(Melb.800.BE.Cluster.Z.validate)<-Allmodes.Melb.800[,c(2)]

k3.Melb.800.BE.Cluster.Z.validate <- kmeans(Melb.800.BE.Cluster.Z.validate, centers = 3, nstart = 25)
k4.Melb.800.BE.Cluster.Z.validate <- kmeans(Melb.800.BE.Cluster.Z.validate, centers = 4, nstart = 25)


p4_All_Validate <- fviz_cluster(k4.Melb.800.BE.Cluster.Z.validate, geom = "point", data = Melb.800.BE.Cluster.Z.validate) + ggtitle("k = 4 (n = 10,629)")

p3_All_Validate <- fviz_cluster(k3.Melb.800.BE.Cluster.Z.validate, geom = "point", data = Melb.800.BE.Cluster.Z.validate) + ggtitle("k = 3 (n = 10,629)")

grid.arrange(p3Test, p4Test, p3_All_Validate , p4_All_Validate , nrow=2)
#accept 4-factor solution
Melb.800.BE.Cluster.Z.validate<-as.data.frame(Melb.800.BE.Cluster.Z.validate)


Melb.800.BE.Cluster.Z.validate<-Melb.800.BE.Cluster.Z.validate %>%
  mutate(Cluster = k4.Melb.800.BE.Cluster.Z.validate$cluster,
         OBJECTID_MODE = row.names(Melb.800.BE.Cluster.Z.validate),)

capture.output(Melb.800.BE.Cluster.Z.validate,file="BEClusterMembership.csv")
capture.output(print(k4.Melb.800.BE.Cluster.Z.validate),file ="BElusterCentroids.csv")

#sociodemographic clusters
#call in censored Prop FTE and Mean size
Melb.800.SD.Cluster.Z<-scale(Allmodes.Melb.800.test[,c(54,55,51,52,53)])
Melb.800.SD.Cluster.Z<-as.data.frame(Melb.800.SD.Cluster.Z)
row.names(Melb.800.SD.Cluster.Z) <- Allmodes.Melb.800[,c(2)]

k3.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 3, nstart = 25)
k4.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 4, nstart = 25)
k5.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 5, nstart = 25)
k6.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 6, nstart = 25)
k7.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 7, nstart = 25)
k8.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 8, nstart = 25)
k9.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 9, nstart = 25)
k10.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 10, nstart = 25)

p3Test.SD <- fviz_cluster(k3.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 3 (n = 5314)")

p4Test.SD <- fviz_cluster(k4.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 4 (n = 5314)")

p5Test.SD <- fviz_cluster(k5.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 5 (n = 5314)")

p6Test.SD <- fviz_cluster(k6.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 6 (n = 5314)")

p7Test.SD <- fviz_cluster(k7.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 7 (n = 5314)")

p8Test.SD <- fviz_cluster(k8.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 8 (n = 5314)")

p9Test.SD <- fviz_cluster(k9.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 9 (n = 5314)")

p10Test.SD <- fviz_cluster(k10.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 10 (n = 5314)")

grid.arrange(p3Test.SD, p4Test.SD, p5Test.SD, p6Test.SD, p7Test.SD, p8Test.SD, p9Test.SD, p10Test.SD, nrow=2)
#suspect 3-factor solution

Melb.800.SD.Cluster.Z.validate<-scale(Allmodes.Melb.800[,c(51, 52, 53 ,54, 55)])
Melb.800.SD.Cluster.Z.validate<-as.data.frame(Melb.800.SD.Cluster.Z.validate)

row.names(Melb.800.SD.Cluster.Z.validate)<-Allmodes.Melb.800[,c(2)]

k3.Melb.800.SD.Cluster.Z.validate <- kmeans(Melb.800.SD.Cluster.Z.validate, centers = 3, nstart = 25)
k4.Melb.800.SD.Cluster.Z.validate <- kmeans(Melb.800.SD.Cluster.Z.validate, centers = 4, nstart = 25)


p4_All_SDValidate <- fviz_cluster(k4.Melb.800.SD.Cluster.Z.validate, geom = "point", data = Melb.800.SD.Cluster.Z.validate) + ggtitle("k = 4 (n = 10,622)")

p3_All_SDValidate <- fviz_cluster(k3.Melb.800.SD.Cluster.Z.validate, geom = "point", data = Melb.800.SD.Cluster.Z.validate) + ggtitle("k = 3 (n = 10,622)")

grid.arrange(p3Test.SD, p4Test.SD, p3_All_SDValidate , p4_All_SDValidate , nrow=2)
#3-cluster solution

capture.output(print(k3.Melb.800.SD.Cluster.Z.validate),file ="SDClusterCentroids.csv")

print(k3.Melb.800.SD.Cluster.Z.validate)
#Centroids, containing original data

Melb.800.SD.Cluster.Z.validate<-Melb.800.SD.Cluster.Z.validate %>%
  mutate(Cluster = k3.Melb.800.SD.Cluster.Z.validate$cluster,
         OBJECTID_MODE = row.names(Melb.800.SD.Cluster.Z.validate),)
         
capture.output(Melb.800.SD.Cluster.Z.validate,file="SDClusterMembership.csv")


#Refer to sampling script
MMLR_Data<- read.csv("Melb.AllStops.Data.1Jan20.csv", header=TRUE, sep=",")
row.names(MMLR_Data) <- MMLR_Data[,c(2)]

Clustersample.bus.400<- MMLR_Data[which (MMLR_Data$Mode=='bus'
                                                  & MMLR_Data$Sample=='Yes'),]
row.names(Clustersample.bus.400) <- Clustersample.bus.400[,c(2)]
Clustersample.tram.600<- MMLR_Data[which (MMLR_Data$Mode=='tram'
                                          & MMLR_Data$Sample=='Yes'),]
row.names(Clustersample.tram.600) <- Clustersample.tram.600[,c(2)]
Clustersample.train.800<- MMLR_Data[which (MMLR_Data$Mode=='train'
                                           &MMLR_Data$Sample=='Yes'),]
row.names(Clustersample.train.800) <- Clustersample.train.800[,c(2)]

#Col headers: ln_Patronage + PropComm +	Balance +	LUEntropy	+ PedConnect+	PBN	+ DestScore	+ Parkiteer	+ ACDist	+ ACCount	+ FTZ	+ Parking	+ PropUrban	+ PropRural	+ EmpAccess	+ C_LOS	+ O_Bus_LOS	+ O_Tram_LOS	+ O_Train_LOS	+ O_LOS	MedInc +	PropOS +	PropBach	+ censored_PropFTE	+ censored_MeanSize	+ 	ln_Emp +	ln_Pop


#Bus
#step 3 Check for multicolinearity
Bus.cluster.LM.VIF<-vif(lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect+	PBN	+ DestScore	+ Parkiteer	+ ACDist	+ ACCount	+ Parking	+ PropUrban	+ EmpAccess	+ C_LOS	+ O_Tram_LOS	+ O_Train_LOS	+ MedInc +	PropOS +	PropBach	+ censored_PropFTE	+ censored_MeanSize	+	ln_Pop, data =Clustersample.bus.400))
#removed  rural, overlapping (total) level of service, FTZ to get rid of singularity
Bus.cluster.LM.VIF
#removed ln_Emp (high VIF >7)


#step 4 Simple correlations
Corrdata.buscluster<-Clustersample.bus.400[,c(19:27, 29:30, 32:42, 49, 51)]

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
#O_Bus_LOS
#MedInc
#censored_PropFTE


#Step 4 maximally adjusted model
Melb.buscluster.LM.1<-lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect+	PBN	+ DestScore	+ Parkiteer	+ ACDist	+ ACCount	+ Parking	+ PropUrban	+ EmpAccess	+ C_LOS	+ O_Tram_LOS	+ O_Train_LOS +	PropOS +	PropBach	+ censored_MeanSize	+	ln_Pop, data =Clustersample.bus.400)
summary(Melb.buscluster.LM.1)

capture.output(summary(Melb.buscluster.LM.1), file = "buscluster.MA.txt")

#function for backward (starts with maximal model)
Melb.buscluster.LM.1.regb<-step(Melb.buscluster.LM.1,
                                  direction = "backward",
                                  trace = 0) #don't print steps
summary(Melb.buscluster.LM.1.regb)

capture.output(summary(Melb.buscluster.LM.1.regb), file = "buscluster.PM.txt")

#diagnostics
plot(Melb.buscluster.LM.1.regb)
#1498 is influential outlier

which(rownames(Clustersample.bus.400) == "1498-bus") #212
Clustersample.bus.400.rd2 <- Clustersample.bus.400[-c(212),]

#rd 2 MA
Melb.buscluster.LM.2.1<-lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect+	PBN	+ DestScore	+ Parkiteer	+ ACDist	+ ACCount	+ Parking	+ PropUrban	+ EmpAccess	+ C_LOS	+ O_Tram_LOS	+ O_Train_LOS +	PropOS +	PropBach	+ censored_MeanSize	+	ln_Pop, data =Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.1)

capture.output(summary(Melb.buscluster.LM.2.1), file = "buscluster.MA.rd.2.txt")

#PM
Melb.buscluster.LM.2.1.regb<-step(Melb.buscluster.LM.2.1,
                                direction = "backward",
                                trace = 0) #don't print steps
summary(Melb.buscluster.LM.2.1.regb)

capture.output(summary(Melb.buscluster.LM.2.1.regb), file = "buscluster.PM.rd.2.txt")

plot(Melb.buscluster.LM.2.1.regb)
#1131 is bordernig cook's distance ; 2218 is an outlier in all other plots; Shape generally fine; leave as is. 


#Tram
#step 3 Check for multicolinearity
Tram.cluster.LM.VIF<-vif(lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect+	PBN	+ DestScore	+ FTZ + ACDist	+ ACCount	+ Parking + EmpAccess	+ C_LOS	+ O_Bus_LOS	+ O_Train_LOS +	PropOS +	PropBach	+ censored_PropFTE	+ censored_MeanSize	+	ln_Pop, data =Clustersample.tram.600))
#removed  rural, parkiteer, overlapping (total) level of service to get rid of singularity
Tram.cluster.LM.VIF
#removed Med Inc, propUrban, ln_Emp (high VIF >7)


#step 4 Simple correlations
Corrdata.tramcluster<-Clustersample.tram.600[,c(19:24, 26:29, 32:37, 39:42, 49:51)]

#Option 1 for Correlation matrices with p-values
Corrdata.tramcluster<-rcorr(as.matrix(Corrdata.tramcluster))



#option 2 for flat correlation matrix
#Set up a custom function to flatten
options(scipen = 1)
#option(scipen = 999)

Corrdata.tramcluster<-flattenCorrMatrix(Corrdata.tramcluster$r,Corrdata.tramcluster$P)
capture.output(Corrdata.tramcluster,file="Corrdata.tramcluster.csv")
Corrdata.tramcluster

#not significant for ln_tram
#LUEntropy
#Parking
#O_Bus_LOS
#O_Train_LOS
#censored_PropFTE

#MA tram
Melb.tramcluster.LM.1<-lm(ln_Patronage ~ PropComm +	Balance + PedConnect+	PBN	+ DestScore	+ FTZ + ACDist	+ ACCount	+ EmpAccess	+ C_LOS	+	PropOS +	PropBach	+ censored_MeanSize	+	ln_Pop, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.1)

capture.output(Melb.tramcluster.LM.1, file = "tramcluster.MA.txt")

Melb.tramcluster.LM.1.regb<-step(Melb.tramcluster.LM.1,
                                  direction = "backward",
                                  trace = 0) #don't print steps

summary(Melb.tramcluster.LM.1.regb)
capture.output(Melb.tramcluster.LM.1.regb, file = "tramcluster.PM.txt")

#diagnostics
par(mfrow=c(2,2))
plot(Melb.tramcluster.LM.1.regb)
#276 is far outlying on all plots, however generally conforms to assumptions/ Leave but investigate.

#Train
#step 3 Check for multicolinearity
Train.cluster.LM.VIF<-vif(lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect+	PBN	+ DestScore	+ FTZ + ACDist	+ ACCount	+ Parking +Parkiteer  + PropRural + EmpAccess	+ C_LOS	+ O_Bus_LOS	+ O_Tram_LOS +	PropOS +	PropBach	+ censored_PropFTE + censored_MeanSize	+	ln_Pop, data =Clustersample.train.800))
#removed  overlapping (total) level of service to get rid of singularity
Train.cluster.LM.VIF
#removed Med Inc, propUrban, ln_Emp (high VIF >7)


#step 4 Simple correlations
Corrdata.traincluster<-Clustersample.train.800[,c(19:29, 31:35, 39:42, 49:51)]

#Option 1 for Correlation matrices with p-values
Corrdata.traincluster<-rcorr(as.matrix(Corrdata.traincluster))

#option 2 for flat correlation matrix
#Set up a custom function to flatten
options(scipen = 1)
#option(scipen = 999)

Corrdata.traincluster<-flattenCorrMatrix(Corrdata.traincluster$r,Corrdata.traincluster$P)
capture.output(Corrdata.traincluster,file="Corrdata.traincluster.csv")

#not significant for ln_train
##PBN
#PropRural
#censored_PropFTE

#maxmially adjusted model
Melb.traincluster.LM.1<-lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect	+ DestScore	+ FTZ + ACDist	+ ACCount	+ Parking +Parkiteer + EmpAccess	+ C_LOS	+ O_Bus_LOS	+ O_Tram_LOS +	PropOS +	PropBach + censored_MeanSize	+	ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.1)

capture.output(summary(Melb.traincluster.LM.1), file = "traincluster.MA.txt")

Melb.traincluster.LM.1.regb<-step(Melb.traincluster.LM.1,
                                 direction = "backward",
                                 trace = 0) #don't print steps
summary(Melb.traincluster.LM.1.regb)
capture.output(summary(Melb.traincluster.LM.1.regb), file = "traincluster.PM.txt")

#diagnostic
plot(Melb.traincluster.LM.1.regb)
#signs of heteroskedasticity with most outlying 597, 605, 636
#exceeds Cook's distance: 2, 628, 4

which(rownames(Clustersample.train.800) == "597-train") #93
which(rownames(Clustersample.train.800) == "605-train") #125
which(rownames(Clustersample.train.800) == "636-train") #79
which(rownames(Clustersample.train.800) == "2-train") #209
which(rownames(Clustersample.train.800) == "628-train") #210
which(rownames(Clustersample.train.800) == "4-train") #211

Clustersample.train.800.rd2 <- Clustersample.train.800[-c(93, 125, 79, 209, 210, 211),]

Melb.traincluster.LM.2.1<-lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect	+ DestScore	+ FTZ + ACDist	+ ACCount	+ Parking +Parkiteer + EmpAccess	+ C_LOS	+ O_Bus_LOS	+ O_Tram_LOS +	PropOS +	PropBach + censored_MeanSize	+	ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.1)

capture.output(summary(Melb.traincluster.LM.2.1), file = "traincluster.MA.rd.2.txt")

Melb.traincluster.LM.2.1.regb<-step(Melb.traincluster.LM.2.1,
                                  direction = "backward",
                                  trace = 0) #don't print steps
summary(Melb.traincluster.LM.2.1.regb)
#actually reduced the explanatory power by removing the outliers. 

capture.output(summary(Melb.traincluster.LM.2.1), file = "traincluster.PM.rd.2.txt")

#diagnostics
plot(Melb.traincluster.LM.2.1.regb)
#609, 627 affecting assumptions. 8 is also an outlier but will retain.

which(rownames(Clustersample.train.800.rd2) == "609-train") #72
which(rownames(Clustersample.train.800.rd2) == "627-train") #73


Clustersample.train.800.rd3 <- Clustersample.train.800.rd2[-c(72, 73),]

#rd 3 maximally adjusted
Melb.traincluster.LM.3.1<-lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect	+ DestScore	+ FTZ + ACDist	+ ACCount	+ Parking +Parkiteer + EmpAccess	+ C_LOS	+ O_Bus_LOS	+ O_Tram_LOS +	PropOS +	PropBach + censored_MeanSize	+	ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.1)

capture.output(summary(Melb.traincluster.LM.3.1), file = "traincluster.MA.rd.3.txt")

Melb.traincluster.LM.3.1.regb<-step(Melb.traincluster.LM.3.1,
                                    direction = "backward",
                                    trace = 0) #don't print steps
summary(Melb.traincluster.LM.3.1.regb)

capture.output(summary(Melb.traincluster.LM.3.1.regb), file = "traincluster.PM.rd.3.txt")

plot(Melb.traincluster.LM.3.1.regb)
#570(row index 204) is extreme outlier. #642, #632, #8 also outlying and now seem to be impacting assumptions


which(rownames(Clustersample.train.800.rd3) == "642-train") #66
which(rownames(Clustersample.train.800.rd3) == "632-train") #72
which(rownames(Clustersample.train.800.rd3) == "8-train") #63
which(rownames(Clustersample.train.800.rd3) == "570-train") #204


Clustersample.train.800.rd4 <- Clustersample.train.800.rd3[-c(66, 72, 204, 63),]

#maxmially adjusted model
Melb.traincluster.LM.4.1<-lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect	+ DestScore	+ FTZ + ACDist	+ ACCount	+ Parking +Parkiteer + EmpAccess	+ C_LOS	+ O_Bus_LOS	+ O_Tram_LOS +	PropOS +	PropBach + censored_MeanSize	+	ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.1)

capture.output(summary(Melb.traincluster.LM.4.1), file = "traincluster.MArd.4.txt")

Melb.traincluster.LM.4.1.regb<-step(Melb.traincluster.LM.4.1,
                                  direction = "backward",
                                  trace = 0) #don't print steps
summary(Melb.traincluster.LM.4.1.regb)
capture.output(summary(Melb.traincluster.LM.4.1.regb), file = "traincluster.PM.rd.4.txt")

#diagnostic
plot(Melb.traincluster.LM.4.1.regb)
#almost adheres to assumptions - one more influential outlier affecting assumptions: 615. 

which(rownames(Clustersample.train.800.rd4) == "615-train") #69
Clustersample.train.800.rd5 <- Clustersample.train.800.rd4[-c(69),]

#maxmially adjusted model #rd 5
Melb.traincluster.LM.5.1<-lm(ln_Patronage ~ PropComm +	Balance +	LUEntropy	+ PedConnect	+ DestScore	+ FTZ + ACDist	+ ACCount	+ Parking +Parkiteer + EmpAccess	+ C_LOS	+ O_Bus_LOS	+ O_Tram_LOS +	PropOS +	PropBach + censored_MeanSize	+	ln_Pop, data =Clustersample.train.800.rd5)
summary(Melb.traincluster.LM.5.1)

capture.output(summary(Melb.traincluster.LM.5.1), file = "traincluster.MArd.5.txt")

Melb.traincluster.LM.5.1.regb<-step(Melb.traincluster.LM.5.1,
                                    direction = "backward",
                                    trace = 0)#don't print steps
summary(Melb.traincluster.LM.5.1.regb)
capture.output(summary(Melb.traincluster.LM.5.1.regb), file = "traincluster.PM.rd.5.txt")

#diagnostic
plot(Melb.traincluster.LM.5.1.regb)
#conforms to assumptions, no influential outliers although 467 comes closest to Cook's distance