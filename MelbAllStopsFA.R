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

