# Title:                  Cluster sampling script
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script is for random sampling from the multimodal clusters. Sampling is constrained by whichever mode has the least number of stops in each cluster. There are 12 clusters, of these 11 have all three modes (train, tram bus). 
#                         Copyright statement: This script is the product of Laura Aston

MMLR.data.inclcluster<- read.csv("Melb.AllStops.Data.18Dec19.csv", header=TRUE, sep=",")
row.names(MMLR.data.inclcluster) <- MMLR.data.inclcluster[,c(2)]


Sampledata.bus.400<- MMLR.data.inclcluster[which (MMLR.data.inclcluster$Mode=='bus'
                                                  & MMLR.data.inclcluster$Target.break == '400'),]
Sampledata.tram.600<- MMLR.data.inclcluster[which (MMLR.data.inclcluster$Mode=='tram'
                                                   & MMLR.data.inclcluster$Target.break == '600'),]
Sampledata.train.800<- MMLR.data.inclcluster[which (MMLR.data.inclcluster$Mode=='train'
                                                    & MMLR.data.inclcluster$Target.break == '800'),]

Sampledata.bus.1<- subset(Sampledata.bus.400,Cluster.key=='1')
Sampledata.tram.1<- subset(Sampledata.tram.600,Cluster.key=='1')
Sampledata.train.1<- subset(Sampledata.train.800,Cluster.key=='1')

Sampledata.bus.2<- subset(Sampledata.bus.400,Cluster.key=='2')
Sampledata.tram.2<- subset(Sampledata.tram.600,Cluster.key=='2')
Sampledata.train.2<- subset(Sampledata.train.800,Cluster.key=='2')

Sampledata.bus.3<- subset(Sampledata.bus.400,Cluster.key=='3')
Sampledata.tram.3<- subset(Sampledata.tram.600,Cluster.key=='3')
Sampledata.train.3<- subset(Sampledata.train.800,Cluster.key=='3')

Sampledata.bus.4<- subset(Sampledata.bus.400,Cluster.key=='4')
Sampledata.tram.4<- subset(Sampledata.tram.600,Cluster.key=='4')
Sampledata.train.4<- subset(Sampledata.train.800,Cluster.key=='4')

Sampledata.bus.5<- subset(Sampledata.bus.400,Cluster.key=='5')
Sampledata.tram.5<- subset(Sampledata.tram.600,Cluster.key=='5')
Sampledata.train.5<- subset(Sampledata.train.800,Cluster.key=='5')

Sampledata.bus.6<- subset(Sampledata.bus.400,Cluster.key=='6')
Sampledata.tram.6<- subset(Sampledata.tram.600,Cluster.key=='6')
Sampledata.train.6<- subset(Sampledata.train.800,Cluster.key=='6')

Sampledata.bus.7<- subset(Sampledata.bus.400,Cluster.key=='7')
Sampledata.tram.7<- subset(Sampledata.tram.600,Cluster.key=='7')
Sampledata.train.7<- subset(Sampledata.train.800,Cluster.key=='7')

Sampledata.bus.8<- subset(Sampledata.bus.400,Cluster.key=='8')
Sampledata.tram.8<- subset(Sampledata.tram.600,Cluster.key=='8')
Sampledata.train.8<- subset(Sampledata.train.800,Cluster.key=='8')

Sampledata.bus.9<- subset(Sampledata.bus.400,Cluster.key=='9')
Sampledata.tram.9<- subset(Sampledata.tram.600,Cluster.key=='9')
Sampledata.train.9<- subset(Sampledata.train.800,Cluster.key=='9')

Sampledata.bus.11<- subset(Sampledata.bus.400,Cluster.key=='11')
Sampledata.tram.11<- subset(Sampledata.tram.600,Cluster.key=='11')
Sampledata.train.11<- subset(Sampledata.train.800,Cluster.key=='11')

Sampledata.bus.12<- subset(Sampledata.bus.400,Cluster.key=='12')
Sampledata.tram.12<- subset(Sampledata.tram.600,Cluster.key=='12')
Sampledata.train.12<- subset(Sampledata.train.800,Cluster.key=='12')

##random sampling
bus1.sample<-sample(Sampledata.bus.1$OBJECTID_MODE, 4, replace = FALSE, prob = NULL)
bus1.sample<-as.data.frame(bus1.sample)
bus1.sample<-mutate(bus1.sample,
                    cluster.key=1,
)
write.csv(bus1.sample,file="bus1.sample.csv")

bus2.sample<-sample(Sampledata.bus.2$OBJECTID_MODE, 59, replace = FALSE, prob = NULL)
bus2.sample<-as.data.frame(bus2.sample)
bus2.sample<-mutate(bus2.sample,
                    cluster.key=2,
)
write.csv(bus2.sample,file="bus2.sample.csv")

bus3.sample<-sample(Sampledata.bus.3$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
bus3.sample<-as.data.frame(bus3.sample)
bus3.sample<-mutate(bus3.sample,
                    cluster.key=3,
)
write.csv(bus3.sample,file="bus3.sample.csv")

bus4.sample<-sample(Sampledata.bus.4$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
bus4.sample<-as.data.frame(bus4.sample)
bus4.sample<-mutate(bus4.sample,
                    cluster.key=4,
)
write.csv(bus4.sample,file="bus4.sample.csv")

bus5.sample<-sample(Sampledata.bus.5$OBJECTID_MODE, 1, replace = FALSE, prob = NULL)
bus5.sample<-as.data.frame(bus5.sample)
bus5.sample<-mutate(bus5.sample,
                    cluster.key=5,
)
write.csv(bus5.sample,file="bus5.sample.csv")

bus6.sample<-sample(Sampledata.bus.6$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
bus6.sample<-as.data.frame(bus6.sample)
bus6.sample<-mutate(bus6.sample,
                    cluster.key=6,
)
write.csv(bus6.sample,file="bus6.sample.csv")

bus7.sample<-sample(Sampledata.bus.7$OBJECTID_MODE, 55, replace = FALSE, prob = NULL)
bus7.sample<-as.data.frame(bus7.sample)
bus7.sample<-mutate(bus7.sample,
                    cluster.key=7,
)
write.csv(bus7.sample,file="bus7.sample.csv")

bus8.sample<-sample(Sampledata.bus.8$OBJECTID_MODE, 53, replace = FALSE, prob = NULL)
bus8.sample<-as.data.frame(bus8.sample)
bus8.sample<-mutate(bus8.sample,
                    cluster.key=8,
)
write.csv(bus8.sample,file="bus8.sample.csv")

bus9.sample<-sample(Sampledata.bus.9$OBJECTID_MODE, 30, replace = FALSE, prob = NULL)
bus9.sample<-as.data.frame(bus9.sample)
bus9.sample<-mutate(bus9.sample,
                    cluster.key=9,
)
write.csv(bus9.sample,file="bus9.sample.csv")

bus11.sample<-sample(Sampledata.bus.11$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
bus11.sample<-as.data.frame(bus11.sample)
bus11.sample<-mutate(bus11.sample,
                     cluster.key=11,
)
write.csv(bus11.sample,file="bus11.sample.csv")

bus12.sample<-sample(Sampledata.bus.12$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
bus12.sample<-as.data.frame(bus12.sample)
bus12.sample<-mutate(bus12.sample,
                     cluster.key=12,
)
write.csv(bus12.sample,file="bus12.sample.csv")

train1.sample<-sample(Sampledata.train.1$OBJECTID_MODE, 4, replace = FALSE, prob = NULL)
train1.sample<-as.data.frame(train1.sample)
train1.sample<-mutate(train1.sample,
                      cluster.key=1,
)
write.csv(train1.sample,file="train1.sample.csv")

train2.sample<-sample(Sampledata.train.2$OBJECTID_MODE, 59, replace = FALSE, prob = NULL)
train2.sample<-as.data.frame(train2.sample)
train2.sample<-mutate(train2.sample,
                      cluster.key=2,
)
write.csv(train2.sample,file="train2.sample.csv")

train3.sample<-sample(Sampledata.train.3$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
train3.sample<-as.data.frame(train3.sample)
train3.sample<-mutate(train3.sample,
                      cluster.key=3,
)
write.csv(train3.sample,file="train3.sample.csv")

train4.sample<-sample(Sampledata.train.4$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
train4.sample<-as.data.frame(train4.sample)
train4.sample<-mutate(train4.sample,
                      cluster.key=4,
)
write.csv(train4.sample,file="train4.sample.csv")

train5.sample<-sample(Sampledata.train.5$OBJECTID_MODE, 1, replace = FALSE, prob = NULL)
train5.sample<-as.data.frame(train5.sample)
train5.sample<-mutate(train5.sample,
                      cluster.key=5,
)
write.csv(train5.sample,file="train5.sample.csv")

train6.sample<-sample(Sampledata.train.6$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
train6.sample<-as.data.frame(train6.sample)
train6.sample<-mutate(train6.sample,
                      cluster.key=6,
)
write.csv(train6.sample,file="train6.sample.csv")

train7.sample<-sample(Sampledata.train.7$OBJECTID_MODE, 55, replace = FALSE, prob = NULL)
train7.sample<-as.data.frame(train7.sample)
train7.sample<-mutate(train7.sample,
                      cluster.key=7,
)
write.csv(train7.sample,file="train7.sample.csv")

train8.sample<-sample(Sampledata.train.8$OBJECTID_MODE, 53, replace = FALSE, prob = NULL)
train8.sample<-as.data.frame(train8.sample)
train8.sample<-mutate(train8.sample,
                      cluster.key=8,
)
write.csv(train8.sample,file="train8.sample.csv")

train9.sample<-sample(Sampledata.train.9$OBJECTID_MODE, 30, replace = FALSE, prob = NULL)
train9.sample<-as.data.frame(train9.sample)
train9.sample<-mutate(train9.sample,
                      cluster.key=9,
)
write.csv(train9.sample,file="train9.sample.csv")

train11.sample<-sample(Sampledata.train.11$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
train11.sample<-as.data.frame(train11.sample)
train11.sample<-mutate(train11.sample,
                       cluster.key=11,
)
write.csv(train11.sample,file="train11.sample.csv")

train12.sample<-sample(Sampledata.train.12$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
train12.sample<-as.data.frame(train12.sample)
train12.sample<-mutate(train12.sample,
                       cluster.key=12,
)
write.csv(train12.sample,file="train12.sample.csv")

tram1.sample<-sample(Sampledata.tram.1$OBJECTID_MODE, 4, replace = FALSE, prob = NULL)
tram1.sample<-as.data.frame(tram1.sample)
tram1.sample<-mutate(tram1.sample,
                     cluster.key=1,
)
write.csv(tram1.sample,file="tram1.sample.csv")

tram2.sample<-sample(Sampledata.tram.2$OBJECTID_MODE, 59, replace = FALSE, prob = NULL)
tram2.sample<-as.data.frame(tram2.sample)
tram2.sample<-mutate(tram2.sample,
                     cluster.key=2,
)
write.csv(tram2.sample,file="tram2.sample.csv")

tram3.sample<-sample(Sampledata.tram.3$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
tram3.sample<-as.data.frame(tram3.sample)
tram3.sample<-mutate(tram3.sample,
                     cluster.key=3,
)
write.csv(tram3.sample,file="tram3.sample.csv")

tram4.sample<-sample(Sampledata.tram.4$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
tram4.sample<-as.data.frame(tram4.sample)
tram4.sample<-mutate(tram4.sample,
                     cluster.key=4,
)
write.csv(tram4.sample,file="tram4.sample.csv")

tram5.sample<-sample(Sampledata.tram.5$OBJECTID_MODE, 1, replace = FALSE, prob = NULL)
tram5.sample<-as.data.frame(tram5.sample)
tram5.sample<-mutate(tram5.sample,
                     cluster.key=5,
)
write.csv(tram5.sample,file="tram5.sample.csv")

tram6.sample<-sample(Sampledata.tram.6$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
tram6.sample<-as.data.frame(tram6.sample)
tram6.sample<-mutate(tram6.sample,
                     cluster.key=6,
)
write.csv(tram6.sample,file="tram6.sample.csv")

tram7.sample<-sample(Sampledata.tram.7$OBJECTID_MODE, 55, replace = FALSE, prob = NULL)
tram7.sample<-as.data.frame(tram7.sample)
tram7.sample<-mutate(tram7.sample,
                     cluster.key=7,
)
write.csv(tram7.sample,file="tram7.sample.csv")

tram8.sample<-sample(Sampledata.tram.8$OBJECTID_MODE, 53, replace = FALSE, prob = NULL)
tram8.sample<-as.data.frame(tram8.sample)
tram8.sample<-mutate(tram8.sample,
                     cluster.key=8,
)
write.csv(tram8.sample,file="tram8.sample.csv")

tram9.sample<-sample(Sampledata.tram.9$OBJECTID_MODE, 30, replace = FALSE, prob = NULL)
tram9.sample<-as.data.frame(tram9.sample)
tram9.sample<-mutate(tram9.sample,
                     cluster.key=9,
)
write.csv(tram9.sample,file="tram9.sample.csv")

tram11.sample<-sample(Sampledata.tram.11$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
tram11.sample<-as.data.frame(tram11.sample)
tram11.sample<-mutate(tram11.sample,
                      cluster.key=11,
)
write.csv(tram11.sample,file="tram11.sample.csv")

tram12.sample<-sample(Sampledata.tram.12$OBJECTID_MODE, 2, replace = FALSE, prob = NULL)
tram12.sample<-as.data.frame(tram12.sample)
tram12.sample<-mutate(tram12.sample,
                      cluster.key=12,
)
write.csv(tram12.sample,file="tram12.sample.csv")