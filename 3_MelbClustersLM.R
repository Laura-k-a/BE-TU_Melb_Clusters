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
library(dplyr) # mutate function

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
Melb.800.BE.Cluster.Z<-scale(Allmodes.Melb.800.test[,c(26, 28, 31, 32, 35, 37:43, 54:56)])

k3.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 3, nstart = 25)
k4.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 4, nstart = 25)
k5.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 5, nstart = 25)
k6.Melb.800.BE.Cluster.Z <- kmeans(Melb.800.BE.Cluster.Z, centers = 6, nstart = 25)

#to change legend labels: labs(title = "Temperatures\n", x = "TY [°C]", y = "Txxx")
#https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot
# plots to compare
p3Test <- fviz_cluster(k3.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 3 (n = 5314)")

p4Test <- fviz_cluster(k4.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 4 (n = 5314)")

p5Test <- fviz_cluster(k5.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 5 (n = 5314)")

p6Test <- fviz_cluster(k6.Melb.800.BE.Cluster.Z, geom = "point", data = Melb.800.BE.Cluster.Z) + ggtitle("k = 6 (n = 5314)")

grid.arrange(p3Test, p4Test, p5Test, p6Test, nrow=2)
#4-cluster solution

#run for all data to validate
Melb.800.BE.Cluster.Z.validate<-scale(Allmodes.Melb.800[,c(26, 28, 31, 32, 35, 37:43, 54:56)])

k3.Melb.800.BE.Cluster.Z.validate <- kmeans(Melb.800.BE.Cluster.Z.validate, centers = 3, nstart = 25)
k4.Melb.800.BE.Cluster.Z.validate <- kmeans(Melb.800.BE.Cluster.Z.validate, centers = 4, nstart = 25)


p4_All_Validate <- fviz_cluster(k4.Melb.800.BE.Cluster.Z.validate, geom = "point", data = Melb.800.BE.Cluster.Z.validate) + ggtitle("k = 4 (n = 10,629)")

p3_All_Validate <- fviz_cluster(k3.Melb.800.BE.Cluster.Z.validate, geom = "point", data = Melb.800.BE.Cluster.Z.validate) + ggtitle("k = 3 (n = 10,629)")

grid.arrange(p3Test, p4Test, p3_All_Validate , p4_All_Validate , nrow=2)
#accept 4-factor solution

Melb.800.BE.Cluster.Z.validate<-as.data.frame(Melb.800.BE.Cluster.Z.validate)
Melb.800.BE.Cluster.Z.validate<-Melb.800.BE.Cluster.Z.validate %>%
  mutate(Cluster = k4.Melb.800.BE.Cluster.Z.validate$cluster)

capture.output(Melb.800.BE.Cluster.Z.validate,file="BEClusterMembership.csv")
capture.output(print(k4.Melb.800.BE.Cluster.Z.validate),file ="BElusterCentroids.csv")

#sociodemographic clusters
Melb.800.SD.Cluster.Z<-scale(Allmodes.Melb.800.test[,c(49,50,51,52,53)])

Melb.800.SD.Cluster.Z <-Melb.800.SD.Cluster.Z[-c(4302, 4358, 4259, 2417, 322, 4258, 4257, 2279, 2225, 3747, 2203),]

k3.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 3, nstart = 25)
k4.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 4, nstart = 25)
k5.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 5, nstart = 25)
k6.Melb.800.SD.Cluster.Z <- kmeans(Melb.800.SD.Cluster.Z, centers = 6, nstart = 25)


p3Test.SD <- fviz_cluster(k3.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 3 (n = 5303)")

p4Test.SD <- fviz_cluster(k4.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 4 (n = 5303)")

p5Test.SD <- fviz_cluster(k5.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 5 (n = 5303)")

p6Test.SD <- fviz_cluster(k6.Melb.800.SD.Cluster.Z, geom = "point", data = Melb.800.SD.Cluster.Z) + ggtitle("k = 6 (n = 5303)")


grid.arrange(p3Test.SD, p4Test.SD, p5Test.SD, p6Test.SD, nrow=2)
#suspect 3-factor solution

Melb.800.SD.Cluster.Z.validate<-scale(Allmodes.Melb.800[,c(49:53)])
Melb.800.SD.Cluster.Z.validate<-as.data.frame(Melb.800.SD.Cluster.Z.validate)

which(rownames(Melb.800.SD.Cluster.Z.validate) == "6210-bus") #3364
which(rownames(Melb.800.SD.Cluster.Z.validate) == "6312-bus") #4030
which(rownames(Melb.800.SD.Cluster.Z.validate) == "6064-bus") #3510
which(rownames(Melb.800.SD.Cluster.Z.validate) == "8826-bus") #215
which(rownames(Melb.800.SD.Cluster.Z.validate) == "9350-bus") #214
which(rownames(Melb.800.SD.Cluster.Z.validate) == "6690-bus") #7195
which(rownames(Melb.800.SD.Cluster.Z.validate) == "12383-bus") #2310


#remove influential outlier
Melb.800.SD.Cluster.Z.validate <- Melb.800.SD.Cluster.Z.validate[-c(3364, 4030, 3510, 215, 214, 7195, 2310),]


k3.Melb.800.SD.Cluster.Z.validate <- kmeans(Melb.800.SD.Cluster.Z.validate, centers = 3, nstart = 25)
k4.Melb.800.SD.Cluster.Z.validate <- kmeans(Melb.800.SD.Cluster.Z.validate, centers = 4, nstart = 25)


p4_All_SDValidate <- fviz_cluster(k4.Melb.800.SD.Cluster.Z.validate, geom = "point", data = Melb.800.SD.Cluster.Z.validate) + ggtitle("k = 4 (n = 10,622)")

p3_All_SDValidate <- fviz_cluster(k3.Melb.800.SD.Cluster.Z.validate, geom = "point", data = Melb.800.SD.Cluster.Z.validate) + ggtitle("k = 3 (n = 10,622)")

grid.arrange(p3Test.SD, p4Test.SD, p3_All_SDValidate , p4_All_SDValidate , nrow=2)
#need to rerun; changing test case to df as well before running. looks like 3 clusters but skwewed by outliers. 

#capture.output(print(k3.Melb.800.SD.Cluster.Z.validate),file ="SDClusterCentroids.csv")
#Centroids, containing original data

Melb.800.SD.Cluster.Z.validate<-Melb.800.SD.Cluster.Z.validate %>%
  mutate(Cluster = k3.Melb.800.SD.Cluster.Z.validate$cluster)

capture.output(Melb.800.SD.Cluster.Z.validate,file="SDClusterMembership.csv")
