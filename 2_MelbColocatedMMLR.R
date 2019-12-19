# Title:                  Melbourne BE-TR Colocated stops linear Model
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
#unit of analysis: 
#                 train-bus sample: 800m catchment originating at train centroid
#                 tram-bus sample: 600 catchment originating at tram centroid
#outlier: TBC
# Analysis method: multivariate multiple regression

#Col headers: 8_Total_Pat	9_Bus_patronage	10_PropComm	11_Balance	12_LUEntropy	13_PedConnect	14_PBN	15_Parkiteer	16_ACDist	17_ACCount	18_FTZ	19_Parking	20_PropUrban	21_PropRural	22_EmpAccess	23_C_LOS	24_O_Bus_LOS	25_O_Tram_LOS	26_O_Train_LOS	27_O_LOS	28_MedInc	29_PropOS	30_PropBach	31_LocalAccess_800	32_EmpDen_800	33_ResidentialDen_800	34_censored_PropFTE	35_censored_MeanSize

library(lm.beta)
library(car)
library(Hmisc)
library(pysych)

#turn off scientific notation
options(scipen = 999)

#read in data
MMLR_Data<-read.csv(file="BE-TR_Co-Located_data.csv")

#Transform patronage into natural log and add as a column to the dataframe
MMLR_Data$X9_Bus_patronage<-as.numeric(MMLR_Data$X9_Bus_patronage)

MMLR_Data<-mutate(MMLR_Data,
                  ln_Bus = log(X9_Bus_patronage),
                  ln_Centroid_Mode = log(X8_Total_Pat),)

MMLR_Data[MMLR_Data == -Inf] <- 0

#optional - assign the Sample ID as the row names
row.names(MMLR_Data) <- MMLR_Data[,c(2)]

#subsetting for mode in the census of all eligible stops

Melb.Trambus.noFTZ<- MMLR_Data[ which(MMLR_Data$Mode=='tram'
                                & MMLR_Data$X18_FTZ=='0'),]
Melb.Trainbus<- MMLR_Data[ which(MMLR_Data$Mode=='train'),]

#Step 2 estimate covariance of the outcome variables 
cov_Trambus.noFTZ<-cor.test(Melb.Trambus.noFTZ$X9_Bus_patronage, Melb.Trambus.noFTZ$X8_Total_Pat, method = "pearson", conf.level = 0.95)
cov_Trambus.noFTZ #0.12  p < 0.02

cov_Trambus_ln.noFTZ<-cor.test(Melb.Trambus.noFTZ$ln_Bus, Melb.Trambus.noFTZ$ln_Centroid_Mode, method = "pearson", conf.level = 0.95)
cov_Trambus_ln.noFTZ #0.30 p = <0.000

capture.output(cov_Trambus_ln.noFTZ,file="cov_Trambus_ln.noFTZ.txt")

cov_Trainbus<-cor.test(Melb.Trainbus$X9_Bus_patronage, Melb.Trainbus$X8_Total_Pat, method = "pearson", conf.level = 0.95)
cov_Trainbus #0.76, 

cov_Trainbus_ln<-cor.test(Melb.Trainbus$ln_Bus, Melb.Trainbus$ln_Centroid_Mode, method = "pearson", conf.level = 0.95)
cov_Trainbus_ln #0.66, p=<0.000

capture.output(cov_Trainbus_ln,file="cov_Trainbus_ln.txt")

#step 3 Check for multicolinearity
Melb.Trambus.noFTZ.VIF<-vif(lm(ln_Centroid_Mode ~ X10_PropComm+X11_Balance+ X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X19_Parking+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ))
#removed parking, rural, overlapping level of service to get rid of singularity
#Removed Residential density and Median Income to get rid of collinearity
Melb.Trambus.noFTZ.VIF

#step 4 Simple correlations
Corrdata.Trambus.noFTZ<-Melb.Trambus.noFTZ[,c(36,37,10,11,12,13,14,16,17,19,20,22,23,24,26,29,30,31,32,34,35)]

#Option 1 for Correlation matrices with p-values
Corrdata.Trambus.noFTZ<-rcorr(as.matrix(Corrdata.Trambus.noFTZ))

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

Corrdata.Trambus.noFTZ<-flattenCorrMatrix(Corrdata.Trambus.noFTZ$r,Corrdata.Trambus.noFTZ$P)
capture.output(Corrdata.Trambus.noFTZ,file="FlatCor.Trambus.noFTZ.csv")

#not significant for ln_bus
#X11_Balance
#X12_LUEntropy
#X23_C_LOS
#X29_PropOS
#X30_PropBach
#X35_censored_MeanSize

#ln_Tram
#X11_Balance
#X19_Parking
#X31_LocalAccess_800

#exclude balance

#Step 4 maximally adjusted model
#reciprocally sinular until Parking removed)
Melb.Trambus.noFTZ.MMLR.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.1)
Anova(Melb.Trambus.noFTZ.MMLR.1)

#remove AC DIst
Melb.Trambus.noFTZ.MMLR.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.2)
Anova(Melb.Trambus.noFTZ.MMLR.2)

#remove pedconnect
Melb.Trambus.noFTZ.MMLR.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X14_PBN+ X17_ACCount+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.3)
Anova(Melb.Trambus.noFTZ.MMLR.3)

#ACCount
Melb.Trambus.noFTZ.MMLR.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.4)
Anova(Melb.Trambus.noFTZ.MMLR.4)

#remove Mean Size
Melb.Trambus.noFTZ.MMLR.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.5)
Anova(Melb.Trambus.noFTZ.MMLR.5)

#remove local access
Melb.Trambus.noFTZ.MMLR.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ)
summary(Melb.Trambus.noFTZ.MMLR.6)
Anova(Melb.Trambus.noFTZ.MMLR.6)

#run diagnostics
par(mfrow=c(2,2))
plot(lm(ln_Centroid_Mode ~ X10_PropComm+X12_LUEntropy+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ))

#remove 249-tram
which(rownames(Melb.Trambus.noFTZ) == "249-tram") #151

#remove influential outlier
Melb.Trambus.noFTZ.rd2 <- Melb.Trambus.noFTZ[-c(151),]

plot(lm(ln_Bus ~ X10_PropComm+X12_LUEntropy+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ))

#rd 2
Melb.Trambus.noFTZ.MMLR.2.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.1)
Anova(Melb.Trambus.noFTZ.MMLR.2.1)

#remove ACDist
Melb.Trambus.noFTZ.MMLR.2.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.2)
Anova(Melb.Trambus.noFTZ.MMLR.2.2)

#remove meansize
Melb.Trambus.noFTZ.MMLR.2.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.3)
Anova(Melb.Trambus.noFTZ.MMLR.2.3)

#remove ACCount
Melb.Trambus.noFTZ.MMLR.2.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.4)
Anova(Melb.Trambus.noFTZ.MMLR.2.4)

#remove pedconnect
Melb.Trambus.noFTZ.MMLR.2.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.5)
Anova(Melb.Trambus.noFTZ.MMLR.2.5)

#remove PBN
Melb.Trambus.noFTZ.MMLR.2.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd2)
summary(Melb.Trambus.noFTZ.MMLR.2.6)
Anova(Melb.Trambus.noFTZ.MMLR.2.6)

#how can local access and employment density have negatvive correlation?
plot(lm(ln_Centroid_Mode ~ X10_PropComm+X12_LUEntropy+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd2))

#remove276-tram

plot(lm(ln_Bus ~ X10_PropComm+X12_LUEntropy+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd2))

#first, try rerunning the factor analysis, excluding free tram zone.
#did not yield strnog factor solutions
#remove276-tram, remove258

which(rownames(Melb.Trambus.noFTZ.rd2) == "276-tram") #47
which(rownames(Melb.Trambus.noFTZ.rd2) == "258-tram") #151

#remove influential outlier
Melb.Trambus.noFTZ.rd3 <- Melb.Trambus.noFTZ.rd2[-c(47, 151),]

#rd 3
Melb.Trambus.noFTZ.MMLR.3.1<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X16_ACDist+ X17_ACCount+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.1)
Anova(Melb.Trambus.noFTZ.MMLR.3.1)

#remove ACDist
Melb.Trambus.noFTZ.MMLR.3.2<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X17_ACCount+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.2)
Anova(Melb.Trambus.noFTZ.MMLR.3.2)

#remove AC Count
Melb.Trambus.noFTZ.MMLR.3.3<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE+ X35_censored_MeanSize, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.3)
Anova(Melb.Trambus.noFTZ.MMLR.3.3)

#remove mean size
Melb.Trambus.noFTZ.MMLR.3.4<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X13_PedConnect+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.4)
Anova(Melb.Trambus.noFTZ.MMLR.3.4)

#remove PEdConnect
Melb.Trambus.noFTZ.MMLR.3.5<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X14_PBN+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.5)
Anova(Melb.Trambus.noFTZ.MMLR.3.5)

#remove PBN
Melb.Trambus.noFTZ.MMLR.3.6<-lm(cbind(ln_Centroid_Mode, ln_Bus) ~ X10_PropComm+X12_LUEntropy+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd3)
summary(Melb.Trambus.noFTZ.MMLR.3.6)
Anova(Melb.Trambus.noFTZ.MMLR.3.6)

#diagnostics
par(mfrow=c(2,2))
plot(lm(ln_Centroid_Mode ~ X10_PropComm+X12_LUEntropy+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd3))

plot(lm(ln_Bus ~ X10_PropComm+X12_LUEntropy+ X20_PropUrban+ X22_EmpAccess+ X23_C_LOS+ X24_O_Bus_LOS+ X26_O_Train_LOS+ X29_PropOS+ X30_PropBach+ X31_LocalAccess_800+ X32_EmpDen_800+ X34_censored_PropFTE, data =Melb.Trambus.noFTZ.rd3))
