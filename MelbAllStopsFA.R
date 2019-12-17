# Title:                  Melbourne All Stops (cleaned version)
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script performs pretreatment (factor analysis) followed by multivariate multiple linear regression of built environment and sociodemographic variables on transit ridership for a sample of tram and bus locations in Melbourne. 
# Data:                   Ridership data includes average normal (school) weekday ridership 
#                         Train, Tram ridership, averaged for 2018, by Victorian Department of Transport. 
#                         Bus ridership, averaged for 4 months from August - November 2018, provided by Chris Loader from the bus planning team at the Victorian Department of Transport
#                         Built environment data is agrgegated to the transit walk catchment station area, and harvested from a variety of open data sources. For more information, refer to the publication related to this script. 
#                         Copyright statement: This script is the product of Laura Aston


#This is the script for the first run through of factor and cluster analysis, involving
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
