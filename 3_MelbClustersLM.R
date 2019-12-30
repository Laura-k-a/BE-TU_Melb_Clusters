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
MMLR_Data<- read.csv("Melb.AllStops.Data.18Dec19.csv", header=TRUE, sep=",")
row.names(MMLR_Data) <- MMLR_Data[,c(2)]

#transpose density and patronage

MMLR_Data<-mutate(MMLR_Data,
                  ln_Patronage= log(X19_Total_Pat),
                  ln_Emp = log(X21_Empden),
                  ln_Pop = log(X23_Popden),)

MMLR_Data[MMLR_Data == -Inf] <- 0


Clustersample.bus.400<- MMLR_Data[which (MMLR_Data$Mode=='bus'
                                                  & MMLR_Data$Sample=='Yes'),]
row.names(Clustersample.bus.400) <- Clustersample.bus.400[,c(2)]
Clustersample.tram.600<- MMLR_Data[which (MMLR_Data$Mode=='tram'
                                          & MMLR_Data$Sample=='Yes'),]
row.names(Clustersample.tram.600) <- Clustersample.tram.600[,c(2)]
Clustersample.train.800<- MMLR_Data[which (MMLR_Data$Mode=='train'
                                           &MMLR_Data$Sample=='Yes'),]
row.names(Clustersample.train.800) <- Clustersample.train.800[,c(2)]

#Col headers: X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X 39._FTZ	+X40_Parking	+X41_PropUrban	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X48_O_LOS	+X51_MedInc	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Emp + ln_Pop


#Bus
#step 3 Check for multicolinearity
Bus.cluster.LM.VIF<-vif(lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X51_MedInc	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.bus.400))
#removed  rural, overlapping (total) level of service, FTZ to get rid of singularity
Bus.cluster.LM.VIF
#removed ln_Emp (high VIF >7)


#step 4 Simple correlations
Corrdata.buscluster<-Clustersample.bus.400[,c(64, 26, 28, 29, 31, 32, 33, 35, 37, 38, 40, 41, 43, 44, 45, 46, 47, 51, 52, 53, 54, 55, 66)]

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
#X45_O_Bus_LOS
#X51_MedInc
#X49_censored_PropFTE

#Step 4 maximally adjusted model
Melb.buscluster.LM.1<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.1)

#remove balance
Melb.buscluster.LM.2<-lm(ln_Patronage ~ X26_PropComm+ + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.2)


#tram LOS
Melb.buscluster.LM.3<-lm(ln_Patronage ~ X26_PropComm+ + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.3)
Anova(Melb.buscluster.LM.3)

#prop OS
Melb.buscluster.LM.4<-lm(ln_Patronage ~ X26_PropComm+ + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X47_O_Train_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.4)


#trainOS
Melb.buscluster.LM.5<-lm(ln_Patronage ~ X26_PropComm+ + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.5)


#propBach
Melb.buscluster.LM.6<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.6)
Anova(Melb.buscluster.LM.6)

#pedconnect
Melb.buscluster.LM.7<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.7)


#empAccess
Melb.buscluster.LM.8<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.8)


#propComm
Melb.buscluster.LM.9<-lm(ln_Patronage ~  + X29_LUEntropy +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.9)


#parkiteer
Melb.buscluster.LM.10<-lm(ln_Patronage ~  + X29_LUEntropy +X32_PBN	+X33_DestScore+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.10)


#propurban
Melb.buscluster.LM.11<-lm(ln_Patronage ~  + X29_LUEntropy +X32_PBN	+X33_DestScore+X37_ACDist	+ X38_ACCount	+X40_Parking	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.11)


#account
Melb.buscluster.LM.12<-lm(ln_Patronage ~  + X29_LUEntropy +X32_PBN	+X33_DestScore+X37_ACDist	+	+X40_Parking	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400)
summary(Melb.buscluster.LM.12)

#diagnostics
par(mfrow=c(2,2))
plot(lm(ln_Patronage ~  X29_LUEntropy +X32_PBN	+X33_DestScore+X37_ACDist	+	+X40_Parking	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400))
#remove 1498 (outside cook's distance)

which(rownames(Clustersample.bus.400) == "1498-bus") #212

#remove potentially influential outliers (just the ones close to cook's distance)
Clustersample.bus.400.rd2 <- Clustersample.bus.400[-c(212),]


#rerun maximally adjusted model
#Step 4 maximally adjusted model
Melb.buscluster.LM.2.1<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.1)

#parkiteer
Melb.buscluster.LM.2.2<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.2)

#AC Dist
Melb.buscluster.LM.2.3<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.3)

#Train LOS
Melb.buscluster.LM.2.4<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.4)

#parking
Melb.buscluster.LM.2.5<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.5)

#Tram LOS
Melb.buscluster.LM.2.6<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.6)

#propOS
Melb.buscluster.LM.2.7<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.7)

#balance
Melb.buscluster.LM.2.8<-lm(ln_Patronage ~ X26_PropComm+ X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.8)

#pedconnect
Melb.buscluster.LM.2.9<-lm(ln_Patronage ~ X26_PropComm+ X29_LUEntropy+X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.9)

#propbach
Melb.buscluster.LM.2.10<-lm(ln_Patronage ~ X26_PropComm+ X29_LUEntropy+X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.10)

#empaccess
Melb.buscluster.LM.2.11<-lm(ln_Patronage ~ X26_PropComm+ X29_LUEntropy+X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.11)

#propcomm
Melb.buscluster.LM.2.12<-lm(ln_Patronage ~ X29_LUEntropy+X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.12)

#propurban
Melb.buscluster.LM.2.13<-lm(ln_Patronage ~ X29_LUEntropy+X32_PBN	+X33_DestScore+ X38_ACCount	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.13)

#pbn (potentially leave in since adjusted R^2 went down)
Melb.buscluster.LM.2.14<-lm(ln_Patronage ~ X29_LUEntropy	+X33_DestScore+ X38_ACCount	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2)
summary(Melb.buscluster.LM.2.14)

#diagnostics
plot(lm(ln_Patronage ~ X29_LUEntropy	+X33_DestScore+ X38_ACCount	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd2))

#1131 is borderline on Cook's distance. Otherwise, assumptions appear to be met. Try removing. 
which(rownames(Clustersample.bus.400.rd2) == "1131-bus") #208

#remove potentially influential outliers (just the ones close to cook's distance)
Clustersample.bus.400.rd3 <- Clustersample.bus.400.rd2[-c(208),]

#rerun maximally adjusted model (rd3)
Melb.buscluster.LM.3.1<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+X37_ACDist	+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.1)

#AC dist
Melb.buscluster.LM.3.2<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.2)

#Tram LOS
Melb.buscluster.LM.3.3<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.3)

#TBalance
Melb.buscluster.LM.3.4<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X47_O_Train_LOS	+X52_PropOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.4)

#PropOS
Melb.buscluster.LM.3.5<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+ X38_ACCount	+X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X47_O_Train_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.5)

#parking
Melb.buscluster.LM.3.6<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X35_Parkiteer+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X47_O_Train_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.6)

#pedconnect
Melb.buscluster.LM.3.7<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy+X32_PBN	+X33_DestScore+X35_Parkiteer+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X47_O_Train_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.7)

#trainLOS
Melb.buscluster.LM.3.8<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy+X32_PBN	+X33_DestScore+X35_Parkiteer+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.8)

#parkiteer
Melb.buscluster.LM.3.9<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy+X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.9)

#prop bach
Melb.buscluster.LM.3.10<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy+X32_PBN	+X33_DestScore+ X38_ACCount	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.10)

#ACCOunt
Melb.buscluster.LM.3.11<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy+X32_PBN	+X33_DestScore	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.11)

#empaccess
Melb.buscluster.LM.3.12<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy+X32_PBN	+X33_DestScore	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.12)

#propcomm
Melb.buscluster.LM.3.13<-lm(ln_Patronage ~ X29_LUEntropy+X32_PBN	+X33_DestScore	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.13)

#PBN
Melb.buscluster.LM.3.14<-lm(ln_Patronage ~ X29_LUEntropy	+X33_DestScore	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3)
summary(Melb.buscluster.LM.3.14)
#same variables as before

#diagnostics
plot(lm(ln_Patronage ~ X29_LUEntropy	+X33_DestScore	+X41_PropUrban	+X44_C_LOS+X50_censored_MeanSize + ln_Pop, data = Clustersample.bus.400.rd3))
#extremely well-behaved plots

Melb.buscluster.LM.3.14<-lm.beta(Melb.buscluster.LM.3.14)
summary(Melb.buscluster.LM.3.14)
capture.output(summary(Melb.buscluster.LM.3.14), file = "Melb.buscluster.LM.3.14.txt")

#Tram
#step 3 Check for multicolinearity
Tram.cluster.LM.VIF<-vif(lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X37_ACDist	+ X38_ACCount	+X39._FTZ +X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS +X47_O_Train_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.tram.600))
#removed  rural, overlapping (total) level of service, parkiteer to get rid of singularity
Tram.cluster.LM.VIF
#removed Med_Inc and ln_Emp to solve all large VIF


#step 4 Simple correlations
Corrdata.tramcluster<-Clustersample.tram.600[,c(64, 26, 28, 29, 31, 32, 33, 37, 38, 39, 40, 41, 43, 44, 45, 46, 47, 52, 53, 54, 55, 66)]

#Option 1 for Correlation matrices with p-values
Corrdata.tramcluster<-rcorr(as.matrix(Corrdata.tramcluster))

#option 2 for flat correlation matrix
options(max.print=1000000)

Corrdata.tramcluster<-flattenCorrMatrix(Corrdata.tramcluster$r,Corrdata.tramcluster$P)
capture.output(Corrdata.tramcluster,file="Corrdata.tramcluster.csv")

#not significant for ln_patronage
#X40_Parking
#X41_PropUrban
#X45_O_Bus_LOS
#X47_O_Train_LOS
#X49_censored_PropFTE

#Step 4 maximally adjusted model
Melb.tramcluster.LM.1<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X37_ACDist	+ X38_ACCount	+X39._FTZ	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.1)

#remove AC Count
Melb.tramcluster.LM.2<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X37_ACDist	+ X39._FTZ	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.2)

#remove pedconnect
Melb.tramcluster.LM.3<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X32_PBN	+X33_DestScore+X37_ACDist	+ X39._FTZ	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.3)

#remove ln_Pop (Strange, need to probe)
Melb.tramcluster.LM.4<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X32_PBN	+X33_DestScore+X37_ACDist	+ X39._FTZ	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.4)

#REMOVE lueNTROPY
Melb.tramcluster.LM.5<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+ X32_PBN	+X33_DestScore+X37_ACDist	+ X39._FTZ	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.5)

#Remove PBN
Melb.tramcluster.LM.6<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X33_DestScore+X37_ACDist	+ X39._FTZ	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.6)

#remove FTZ (problematic, since different fare rules apply which would be reasonably expected to impact results)
Melb.tramcluster.LM.7<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X33_DestScore+X37_ACDist	+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.7)

#remove AC Dist
Melb.tramcluster.LM.8<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X33_DestScore+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.8)

#remove PropBach
Melb.tramcluster.LM.9<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X33_DestScore+X43_EmpAccess	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS		+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.9)

#removeEmpAccess
#rsults are jumping around a lot, suggesting collinearity
Melb.tramcluster.LM.10<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X33_DestScore	+X44_C_LOS	+X46_O_Tram_LOS	+X52_PropOS		+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.10)

#remove PropOS
Melb.tramcluster.LM.11<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X33_DestScore	+X44_C_LOS	+X46_O_Tram_LOS	+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.11)

#remove DestScore
Melb.tramcluster.LM.12<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X44_C_LOS	+X46_O_Tram_LOS	+X50_censored_MeanSize, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.12)

#removeMeansize
Melb.tramcluster.LM.13<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X44_C_LOS	+X46_O_Tram_LOS, data =Clustersample.tram.600)
summary(Melb.tramcluster.LM.13)

#run diagnostics
#diagnostics
par(mfrow=c(2,2))
plot(lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X44_C_LOS	+X46_O_Tram_LOS, data = Clustersample.tram.600))

#Fits assumptions nicely. Investigate outliers: 1898 and 276 (on border of Cook's distance, so try removing)
#compare results if not includig free tram zone

Melb.tramcluster.LM.13<-lm.beta(Melb.tramcluster.LM.13)
summary(Melb.tramcluster.LM.13)
capture.output(summary(Melb.tramcluster.LM.13), file = "Melb.tramcluster.LM.13.txt")



#tram.NoFTZ
Clustersample.tram.600.NoFTZ<- MMLR_Data[which (MMLR_Data$Mode=='tram'
                                          & MMLR_Data$Sample=='Yes'
                                          & MMLR_Data$X39._FTZ =='0'),]
row.names(Clustersample.tram.600.NoFTZ) <- Clustersample.tram.600.NoFTZ[,c(2)]

#step 3 Check for multicolinearity
Tram.cluster.noFTZ.LM.VIF<-vif(lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X37_ACDist	+ X38_ACCount +X40_Parking	+X41_PropUrban	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS +X47_O_Train_LOS +X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ))
#removed  rural, overlapping (total) level of service, parkiteer to get rid of singularity
Tram.cluster.noFTZ.LM.VIF
#removed Med_Inc and ln_Pop to solve all large VIF


#step 4 Simple correlations
Corrdata.tramcluster.noFTZ<-Clustersample.tram.600.NoFTZ[,c(64, 26, 28, 29, 31, 32, 33, 37, 38, 40, 41, 43, 44, 45, 46, 47, 52, 53, 54, 55, 65)]

#Option 1 for Correlation matrices with p-values
Corrdata.tramcluster.noFTZ<-rcorr(as.matrix(Corrdata.tramcluster.noFTZ))

#option 2 for flat correlation matrix
options(max.print=1000000)

Corrdata.tramcluster.noFTZ<-flattenCorrMatrix(Corrdata.tramcluster.noFTZ$r,Corrdata.tramcluster.noFTZ$P)
capture.output(Corrdata.tramcluster.noFTZ,file="Corrdata.tramcluster.noFTZ.csv")

#not significant for ln_patronage
#X41_PropUrban
#X45_O_Bus_LOS
#X46_O_Tram_LOS
#X47_O_Train_LOS
#X52_PropOS
#X49_censored_PropFTE


#Step 4 maximally adjusted model
Melb.tramcluster.noFTZ.LM.1<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X37_ACDist	+ X38_ACCount +X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.1)

#remove ACDist
Melb.tramcluster.noFTZ.LM.2<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount +X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.2)

#remove EmpAccess
Melb.tramcluster.noFTZ.LM.3<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.3)

#remove LuEntropy
Melb.tramcluster.noFTZ.LM.4<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.4)

#remove PBN
Melb.tramcluster.noFTZ.LM.5<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X31_PedConnect 	+X33_DestScore+ X38_ACCount +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.5)

#remove ACCount
Melb.tramcluster.noFTZ.LM.6<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X31_PedConnect 	+X33_DestScore+X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.6)

#remove PedConnect
Melb.tramcluster.noFTZ.LM.7<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X33_DestScore+X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.7)

#remove DestScore
Melb.tramcluster.noFTZ.LM.8<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.8)

#removePropBach
Melb.tramcluster.noFTZ.LM.9<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X40_Parking	+X44_C_LOS+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ)
summary(Melb.tramcluster.noFTZ.LM.9)

#diagnostics
plot(lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X40_Parking	+X44_C_LOS+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.NoFTZ))

#this time 276 crosses cook's distance. Outliers from the original run (i.e. 1898) including FTZ persist: Try removing 276 from this version; since in the prior, FTZ was excluded yet that did not make thoeretical sense because the fare is different in FTZ. 

which(rownames(Clustersample.tram.600.NoFTZ) == "276-tram") #48

#remove potentially influential outliers (just the ones close to cook's distance)
Clustersample.tram.600.noFTZ.rd2 <- Clustersample.tram.600.NoFTZ[-c(48),]

Melb.tramcluster.noFTZ.LM.2.1<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+X37_ACDist	+ X38_ACCount +X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2)
summary(Melb.tramcluster.noFTZ.LM.2.1)

#ACDist
Melb.tramcluster.noFTZ.LM.2.2<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount +X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2)
summary(Melb.tramcluster.noFTZ.LM.2.2)

#empaccess
Melb.tramcluster.noFTZ.LM.2.3<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2)
summary(Melb.tramcluster.noFTZ.LM.2.3)

#LUEntropy
Melb.tramcluster.noFTZ.LM.2.4<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+ X31_PedConnect +X32_PBN	+X33_DestScore+ X38_ACCount +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2)
summary(Melb.tramcluster.noFTZ.LM.2.4)

#PBN
Melb.tramcluster.noFTZ.LM.2.5<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+ X31_PedConnect	+X33_DestScore+ X38_ACCount +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2)
summary(Melb.tramcluster.noFTZ.LM.2.5)

#ACCount
Melb.tramcluster.noFTZ.LM.2.6<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+ X31_PedConnect	+X33_DestScore +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2)
summary(Melb.tramcluster.noFTZ.LM.2.6)

#pedconnect
Melb.tramcluster.noFTZ.LM.2.7<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance	+X33_DestScore +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2)
summary(Melb.tramcluster.noFTZ.LM.2.7)

#destscore
Melb.tramcluster.noFTZ.LM.2.8<-lm(ln_Patronage ~ X26_PropComm+ + X28_Balance +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2)
summary(Melb.tramcluster.noFTZ.LM.2.8)

#doesn't make sense
#run diagnostics
plot(lm(ln_Patronage ~ X26_PropComm+ + X28_Balance +X40_Parking	+X44_C_LOS	+X53_PropBach	+X50_censored_MeanSize +ln_Emp, data =Clustersample.tram.600.noFTZ.rd2))
#fits outlier assumptions; generally well-behaved. 1898 still an outlier. Generally converging on the same results; balance is negatively related to patronage; without FTZ sees ln_pop used instead of ln_Emp (due to VIF scores); and ln_Pop falls out of the model whereas ln_Emp stays. noFTZ model has parking as an explanatory variable; whereas it is not significant for FTZ included model. Need to probe this finding. 


Melb.tramcluster.noFTZ.LM.2.8<-lm.beta(Melb.tramcluster.noFTZ.LM.2.8)
summary(Melb.tramcluster.noFTZ.LM.2.8)
capture.output(summary(Melb.tramcluster.noFTZ.LM.2.8), file = "Melb.tramcluster.noFTZ.LM.2.8.txt")

#train
#step 3 Check for multicolinearity
Train.cluster.LM.VIF<-vif(lm(ln_Patronage ~ X26_PropComm + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800))
#removed  overlapping train LOS and overlapping (total) level of serviceto get rid of singularity
Train.cluster.LM.VIF
#removed ln_Emp, MedInc to solve VIF
#ln_Pop still has VIF = 6.08. Try removing Prop Urban
#fixed

#step 4 Simple correlations
Corrdata.traincluster<-Clustersample.train.800[,c(64, 26, 28, 29, 31, 32, 33, 35, 37, 38, 39, 40, 42, 43, 44, 45, 46, 52, 53, 54, 55, 66)]

#Option 1 for Correlation matrices with p-values
Corrdata.traincluster<-rcorr(as.matrix(Corrdata.traincluster))

#option 2 for flat correlation matrix
options(max.print=1000000)

Corrdata.traincluster<-flattenCorrMatrix(Corrdata.traincluster$r,Corrdata.traincluster$P)
capture.output(Corrdata.traincluster,file="Corrdata.traincluster.csv")

#not significant for ln_patronage
#X32_PBN
#X42_PropRural
#X49_censored_PropFTE


#Step 4 maximally adjusted model
Melb.traincluster.LM.1<-lm(ln_Patronage ~ X26_PropComm + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.1)

#LUEntropy
Melb.traincluster.LM.2<-lm(ln_Patronage ~ X26_PropComm + X28_Balance	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.2)

#pedconnect
Melb.traincluster.LM.3<-lm(ln_Patronage ~ X26_PropComm + X28_Balance	+X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.3)

#ACCount
Melb.traincluster.LM.4<-lm(ln_Patronage ~ X26_PropComm + X28_Balance	+X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.4)

#PropOS
Melb.traincluster.LM.5<-lm(ln_Patronage ~ X26_PropComm + X28_Balance	+X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.5)

#DestScore
Melb.traincluster.LM.6<-lm(ln_Patronage ~ X26_PropComm + X28_Balance	+X32_PBN + X35_Parkiteer	+ X37_ACDist+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.6)

#propComm
Melb.traincluster.LM.7<-lm(ln_Patronage ~ X28_Balance	+X32_PBN + X35_Parkiteer	+ X37_ACDist+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.7)

#PropBach
Melb.traincluster.LM.8<-lm(ln_Patronage ~ X28_Balance	+X32_PBN + X35_Parkiteer	+ X37_ACDist+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.8)

#PropRural
Melb.traincluster.LM.9<-lm(ln_Patronage ~ X28_Balance	+X32_PBN + X35_Parkiteer	+ X37_ACDist+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.9)

#propFTE
Melb.traincluster.LM.10<-lm(ln_Patronage ~ X28_Balance	+X32_PBN + X35_Parkiteer	+ X37_ACDist+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800)
summary(Melb.traincluster.LM.10)

#PBN is negative, doesn't make sense
#rest makes sense

#diagnostics
plot(lm(ln_Patronage ~ X28_Balance	+X32_PBN + X35_Parkiteer	+ X37_ACDist+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800))
#major assumption violations. remove outside cook's distance:
#2, 628, 4

which(rownames(Clustersample.train.800) == "2-train") #209
which(rownames(Clustersample.train.800) == "628-train") #210
which(rownames(Clustersample.train.800) == "4-train") #211

#remove potentially influential outliers (just the ones close to cook's distance)
Clustersample.train.800.rd2 <- Clustersample.train.800[-c(209, 210, 211),]

Melb.traincluster.LM.2.1<-lm(ln_Patronage ~ X26_PropComm + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.1)

#PedConnect
Melb.traincluster.LM.2.2<-lm(ln_Patronage ~ X26_PropComm + X28_Balance + X29_LUEntropy	+X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.2)

#balance
Melb.traincluster.LM.2.3<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy	+X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.3)

#destscore
Melb.traincluster.LM.2.4<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy	+X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.4)

#ACCount
Melb.traincluster.LM.2.5<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy	+X32_PBN + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.5)

#PropOS
Melb.traincluster.LM.2.6<-lm(ln_Patronage ~ X26_PropComm + X29_LUEntropy	+X32_PBN + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.6)

#LUEntropy
Melb.traincluster.LM.2.7<-lm(ln_Patronage ~ X26_PropComm +X32_PBN + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.7)

#FTE
Melb.traincluster.LM.2.8<-lm(ln_Patronage ~ X26_PropComm +X32_PBN + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.8)

#PropComm
Melb.traincluster.LM.2.9<-lm(ln_Patronage ~X32_PBN + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.9)

#PropRural
Melb.traincluster.LM.2.10<-lm(ln_Patronage ~X32_PBN + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.10)

#PBN
Melb.traincluster.LM.2.11<-lm(ln_Patronage ~ X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.11)

#TRamLOS
Melb.traincluster.LM.2.12<-lm(ln_Patronage ~ X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.12)

#PropBach
Melb.traincluster.LM.2.13<-lm(ln_Patronage ~ X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2)
summary(Melb.traincluster.LM.2.13)

#results are logical
#diagnostic
plot(lm(ln_Patronage ~ X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd2))
#violation of equal variance assumption. 
#cook's distance is not exceeded; this is because of the large variance in patronage
#significant outliers are 597, 609 and 636. Try removing

which(rownames(Clustersample.train.800.rd2) == "597-train") #93
which(rownames(Clustersample.train.800.rd2) == "609-train") #72
which(rownames(Clustersample.train.800.rd2) == "636-train") #79

#remove potentially influential outliers (just the ones close to cook's distance)
Clustersample.train.800.rd3 <- Clustersample.train.800.rd2[-c(93, 72, 79),]

Melb.traincluster.LM.3.1<-lm(ln_Patronage ~ X26_PropComm + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.1)

##propcomm
Melb.traincluster.LM.3.2<-lm(ln_Patronage ~  X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.2)

#PedConnect
Melb.traincluster.LM.3.3<-lm(ln_Patronage ~  X28_Balance + X29_LUEntropy +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.3)

#ACCount
Melb.traincluster.LM.3.4<-lm(ln_Patronage ~  X28_Balance + X29_LUEntropy +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.4)

#DestScore
Melb.traincluster.LM.3.5<-lm(ln_Patronage ~  X28_Balance + X29_LUEntropy +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.5)

#PropOS
Melb.traincluster.LM.3.6<-lm(ln_Patronage ~  X28_Balance + X29_LUEntropy +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.6)

#PropFTE
Melb.traincluster.LM.3.7<-lm(ln_Patronage ~  X28_Balance + X29_LUEntropy +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.7)

#Balance
Melb.traincluster.LM.3.8<-lm(ln_Patronage ~ X29_LUEntropy +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.8)

#proprural
Melb.traincluster.LM.3.9<-lm(ln_Patronage ~ X29_LUEntropy +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X53_PropBach	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.9)

#propbach
Melb.traincluster.LM.3.10<-lm(ln_Patronage ~ X29_LUEntropy +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.10)

#LUEntropy
Melb.traincluster.LM.3.11<-lm(ln_Patronage ~ +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.11)

#meansize
Melb.traincluster.LM.3.12<-lm(ln_Patronage ~ +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+ ln_Pop, data =Clustersample.train.800.rd3)
summary(Melb.traincluster.LM.3.12)

#again PBN is significant with negative association
#diagnostic plots
plot(lm(ln_Patronage ~ +X32_PBN + X35_Parkiteer	+ X37_ACDist	+ X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+ ln_Pop, data =Clustersample.train.800.rd3))

#outliers (not past cook's distance but heteroscedasticity assumption violated): 642, 627, 632
which(rownames(Clustersample.train.800.rd3) == "642-train") #66
which(rownames(Clustersample.train.800.rd3) == "627-train") #72
which(rownames(Clustersample.train.800.rd3) == "632-train") #73

#remove potentially influential outliers (just the ones close to cook's distance)
Clustersample.train.800.rd4 <- Clustersample.train.800.rd3[-c(66,72,73),]

#check correlations since have changed the sample a lot

#step 3 Check for multicolinearity
Train.cluster.rd4.LM.VIF<-vif(lm(ln_Patronage ~ X26_PropComm + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking + X42_PropRural	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	 + X52_PropOS	+X53_PropBach	+X49_censored_PropFTE	+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd4))
#removed  overlapping train LOS and overlapping (total) level of serviceto get rid of singularity
Train.cluster.rd4.LM.VIF
#removed ln_Emp, MedInc to solve VIF
#removed PropUrban instead of Ln_pop as suspected these were colinear; and solved colinearity of ln_Pop
Corrdata.traincluster.rd4<-Clustersample.train.800.rd4[,c(64, 26, 28, 29, 31, 32, 33, 35, 37, 38, 39, 40, 42, 43, 44, 45, 46, 52, 53, 54, 55, 66)]

#Option 1 for Correlation matrices with p-values
Corrdata.traincluster.rd4<-rcorr(as.matrix(Corrdata.traincluster.rd4))

#option 2 for flat correlation matrix
options(max.print=1000000)

Corrdata.traincluster.rd4<-flattenCorrMatrix(Corrdata.traincluster.rd4$r,Corrdata.traincluster.rd4$P)
capture.output(Corrdata.traincluster.rd4,file="Corrdata.traincluster.rd4.csv")

#notsignificant
#X42_PropRural
#X49_censored_PropFTE

#maximally adjusted model
Melb.traincluster.LM.4.1<-lm(ln_Patronage ~ X26_PropComm + X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	 + X52_PropOS	+X53_PropBach		+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.1)

#function for backward (starts with maximal model)
Melb.traincluster.LM.4.regb<-step(Melb.traincluster.LM.4.1,
                                  direction = "backward",
                                  trace = 0) #don't print steps

#alternative is forward regression

#propcomm
Melb.traincluster.LM.4.2<-lm(ln_Patronage ~ X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	+X33_DestScore + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	 + X52_PropOS	+X53_PropBach		+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.2)

#destscore
Melb.traincluster.LM.4.3<-lm(ln_Patronage ~ X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	 + X52_PropOS	+X53_PropBach		+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.3)

#propOS
Melb.traincluster.LM.4.4<-lm(ln_Patronage ~ X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach		+X50_censored_MeanSize + ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.4)

#meansize
Melb.traincluster.LM.4.5<-lm(ln_Patronage ~ X28_Balance + X29_LUEntropy	+ X31_PedConnect +X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach + ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.5)

#pedconnect
Melb.traincluster.LM.4.6<-lm(ln_Patronage ~ X28_Balance + X29_LUEntropy	+X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach + ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.6)

#luEntropy
Melb.traincluster.LM.4.7<-lm(ln_Patronage ~ X28_Balance +	+X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS		+X53_PropBach + ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.7)

#propbach
Melb.traincluster.LM.4.8<-lm(ln_Patronage ~ X28_Balance +	+X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+ ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.8)

#balance
Melb.traincluster.LM.4.9<-lm(ln_Patronage ~	+X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+ X38_ACCount	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+ ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.9)

#ACCount
Melb.traincluster.LM.4.10<-lm(ln_Patronage ~	+X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS	+ ln_Pop, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.10)

#lnPop
Melb.traincluster.LM.4.11<-lm(ln_Patronage ~	+X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS, data =Clustersample.train.800.rd4)
summary(Melb.traincluster.LM.4.11)
#population dropped out as an explanatory variable. 
#overlapping tram frequency influential?
#PBN is negative
#lots of illogical results need probing; comparing across samples with and without outliers

#diagnostics
plot(lm(ln_Patronage ~	+X32_PBN	 + X35_Parkiteer	+ X37_ACDist	+X39._FTZ	+X40_Parking	+X43_EmpAccess	+X44_C_LOS	+X45_O_Bus_LOS	+X46_O_Tram_LOS, data =Clustersample.train.800.rd4))

#much closer to adherance with assumption of heteroscedasticity
#closest to Cook's distance is 615-train and 467-train.
#residuals outliers: 8-train and 493-train

options(scipen = 999)
Melb.traincluster.LM.4.11<-lm.beta(Melb.traincluster.LM.4.11)
summary(Melb.traincluster.LM.4.11)
capture.output(summary(Melb.traincluster.LM.4.11), file = "Melb.traincluster.LM.4.11.txt")

summary(Melb.traincluster.LM.4.regb)
#gives most parsimonious model based on AIC, not p. 

