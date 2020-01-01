# Title:                  Example Sampling script for holdout sample (not used)
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   Simple script to sample without replacement and then update the original dataset with an idetifier for the observations in the sample.
#                         Copyright statement: This script is the product of Laura Aston



SamplingBus<-sample(BusSample.400$OBJECTID_MODE, 4742, replace = FALSE, prob = NULL)
SamplingBus<-as.data.frame(SamplingBus)
SamplingBus<-mutate(SamplingBus,
                    CensusSample = "yes",
                    OBJECTID_MODE=SamplingBus)
BusSample.400<-BusSample.400%>%
  left_join(SamplingBus, by="OBJECTID_MODE")

SamplingTrain<-sample(TrainSample.800$OBJECTID_MODE, 106, replace = FALSE, prob = NULL)
SamplingTrain<-as.data.frame(SamplingTrain)
SamplingTrain<-mutate(SamplingTrain,
                      CensusSample = "yes",
                      OBJECTID_MODE=SamplingTrain)
TrainSample.800<-TrainSample.800%>%
  left_join(SamplingTrain, by="OBJECTID_MODE")