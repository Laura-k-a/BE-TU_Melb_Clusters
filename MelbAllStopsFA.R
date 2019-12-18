# Title:                  Melbourne All Stops (cleaned version)
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script performs pretreatment (factor analysis) of built environment variables measured for transit stations in Melbourne
# Data:                   Refer to [insert doi for figshare] for ontology and reference for built environment.
# Copyright statement: This script is the product of Laura Aston

# Sample attributes
#modes: all
#variables: BE and sociodemographic
#unit of analysis: 800m
#outlier: all in

#Set working directory
setwd("C:/Users/lkast1/Google Drive/PhD/2.Analysis/2. Empirical Analysis/BE-TR_Multi Country Samples/Melbourne/Melb.All.Stops/Melb.AllStops.Repo")

install.packages("nFactors")
install.packages("REdaS")
library(nFactors)
library(psych)
library(REdaS) #package for Bartlett's sphericity test
citation("REdaS")
citation("psych")

#master variable list 

#Step 1:Test the variables are adequately correlated to use factor analysis (significant Bartlett's sphericity test)
Allmodes.Melb<- read.csv("Melb.AllStops.Data.18Dec19.csv", header=TRUE, sep=",")

#subset for 800m catchment
Allmodes.Melb.800<-Allmodes.Melb[ which(Allmodes.Melb$Target.break=='800'),]
row.names(Allmodes.Melb) <- Allmodes.Melb[,c(2)]

cormat.Allmodes.800<-Allmodes.Melb.800[,c(19:53)]

bartletts_Allmodes.Melb.800<-bart_spher(cormat.Allmodes.800, use = c("complete.obs"))
bartletts_Allmodes.Melb.800

capture.output(bartletts_Allmodes.Melb.800,file="bartletts_Allmodes.Melb.800.csv")

#scree plot
install.packages("nFactors")
library(nFactors)

fa.data.Allmodes.800.BE<-Allmodes.Melb.800[,c(21,23,24,26:37,45,49, 50,53)] #Column index for BE variables.
fa.data.Allmodes.800.SD<-Allmodes.Melb.800[,c(46:48,51,52)] #column index for SD variables.

ev_BE <- eigen(cor(fa.data.Allmodes.800.BE))# get eigenvalues
ap_BE <- parallel(subject=nrow(fa.data.Allmodes.800.BE),var=ncol(fa.data.Allmodes.800.BE),
                                 rep=100,cent=.05)
nS_BE<- nScree(x=ev_BE$values, aparallel=ap_BE$eigen$qevpea)
plotnScree(nS_BE) #4 or 5 eigenvalues

#BE_Unrotated factor matrix
fa.BE.800.5 <- factanal(fa.data.Allmodes.800.BE, factors = 5, rotation = "none")
fa.BE.800.4 <- factanal(fa.data.Allmodes.800.BE, factors = 4, rotation = "none")
#can't optimise
fa.BE.800.3 <- factanal(fa.data.Allmodes.800.BE, factors = 3, rotation = "none")
fa.BE.800.3

#remove high uniqueness variables prop commercial, Land use entropy, cycle connectivity, parkiteer, AC Dist, Parking area

fa.data.Allmodes.800.BE<-Allmodes.Melb.800[,c(21,23,24,27:28, 30:31, 33:34, 36,49, 50,53)]
#could also be due to outliers
#update scree plot
ev_BE <- eigen(cor(fa.data.Allmodes.800.BE))# get eigenvalues
ap_BE <- parallel(subject=nrow(fa.data.Allmodes.800.BE),var=ncol(fa.data.Allmodes.800.BE),
                  rep=100,cent=.05)
nS_BE<- nScree(x=ev_BE$values, aparallel=ap_BE$eigen$qevpea)
plotnScree(nS_BE) #3 eigenvalues



capture.output(fa.BE.800.3.promax, file = "fa.BE.800.3.promax.csv")
#cross loading --> accept promax solution

#Step 2 - append factor scores to dataset
Melb_All_fs <- factor.scores(fa.data.Allmodes.800.BE, fa.BE.800.3.promax)
Melb_All_fs <-Melb_All_fs$scores      #get the columns of factor scores for each case
Allmodes.Melb.800<- cbind(Allmodes.Melb.800,Melb_All_fs) #append factor scores to dataset

capture.output(Allmodes.Melb.800, file = "Allmodes.Melb.800.csv")
#factor1 = local access
#factor2 = residential density
#factor 3 = Employment opportunities



##Sociodemographic factors


#SD not working
#ev_SD <- eigen(cor(fa.data.Allmodes.800.SD))# get eigenvalues
#ap_SD <- parallel(subject=nrow(fa.data.Allmodes.800.SD),var=ncol(fa.data.Allmodes.800.SD),
rep=100,cent=.05)
#nS_SD<- nScree(x=ev_SD$values, aparallel=ap_SD$eigen$qevpea)
#plotnScree(nS_SD)