# Title:                  Cluster sampling script
# Author details:         Laura Aston
# Affiliation:            Public Transport Research Group, Monash University
# Contact details:        laura.aston@monash.edu
# Script and data info:   This script is for random sampling from the multimodal clusters. Sampling is constrained by whichever mode has the least number of stops in each cluster. There are 12 clusters, of these 11 have all three modes (train, tram bus). 
#                         Copyright statement: This script is the product of Laura Aston

Melb.data.inclcluster<- read.csv("BE-TR_AllStops_data.csv", header=TRUE, sep=",")
row.names(Melb.data.inclcluster) <- Melb.data.inclcluster[,c(2)]


Sampledata.bus.400<- Melb.data.inclcluster[which (Melb.data.inclcluster$Mode=='bus'
                                                  & Melb.data.inclcluster$Target.break == '400'),]
Sampledata.tram.600<- Melb.data.inclcluster[which (Melb.data.inclcluster$Mode=='tram'
                                                   & Melb.data.inclcluster$Target.break == '600'),]
Sampledata.train.800<- Melb.data.inclcluster[which (Melb.data.inclcluster$Mode=='train'
                                                    & Melb.data.inclcluster$Target.break == '800'),]

Sampledata.bus.1<- subset(Sampledata.bus.400,Cluster.membership=='1')
Sampledata.tram.1<- subset(Sampledata.tram.600,Cluster.membership=='1')
Sampledata.train.1<- subset(Sampledata.train.800,Cluster.membership=='1')

Sampledata.bus.2<- subset(Sampledata.bus.400,Cluster.membership=='2')
Sampledata.tram.2<- subset(Sampledata.tram.600,Cluster.membership=='2')
Sampledata.train.2<- subset(Sampledata.train.800,Cluster.membership=='2')

Sampledata.bus.3<- subset(Sampledata.bus.400,Cluster.membership=='3')
Sampledata.tram.3<- subset(Sampledata.tram.600,Cluster.membership=='3')
Sampledata.train.3<- subset(Sampledata.train.800,Cluster.membership=='3')

Sampledata.bus.4<- subset(Sampledata.bus.400,Cluster.membership=='4')
Sampledata.tram.4<- subset(Sampledata.tram.600,Cluster.membership=='4')
Sampledata.train.4<- subset(Sampledata.train.800,Cluster.membership=='4')

Sampledata.bus.5<- subset(Sampledata.bus.400,Cluster.membership=='5')
Sampledata.tram.5<- subset(Sampledata.tram.600,Cluster.membership=='5')
Sampledata.train.5<- subset(Sampledata.train.800,Cluster.membership=='5')


##random sampling
bus1.sample<-sample(Sampledata.bus.1$OBJECTID_MODE, 54, replace = FALSE, prob = NULL)
bus1.sample<-as.data.frame(bus1.sample)
bus1.sample<-mutate(bus1.sample,
                    Cluster.membership=1,
)
write.csv(bus1.sample,file="bus1.sample.csv")

bus2.sample<-sample(Sampledata.bus.2$OBJECTID_MODE, 84, replace = FALSE, prob = NULL)
bus2.sample<-as.data.frame(bus2.sample)
bus2.sample<-mutate(bus2.sample,
                    Cluster.membership=2,
)
write.csv(bus2.sample,file="bus2.sample.csv")

bus3.sample<-sample(Sampledata.bus.3$OBJECTID_MODE, 53, replace = FALSE, prob = NULL)
bus3.sample<-as.data.frame(bus3.sample)
bus3.sample<-mutate(bus3.sample,
                    Cluster.membership=3,
)
write.csv(bus3.sample,file="bus3.sample.csv")

bus4.sample<-sample(Sampledata.bus.4$OBJECTID_MODE, 3, replace = FALSE, prob = NULL)
bus4.sample<-as.data.frame(bus4.sample)
bus4.sample<-mutate(bus4.sample,
                    Cluster.membership=4,
)
write.csv(bus4.sample,file="bus4.sample.csv")

bus5.sample<-sample(Sampledata.bus.5$OBJECTID_MODE, 1, replace = FALSE, prob = NULL)
bus5.sample<-as.data.frame(bus5.sample)
bus5.sample<-mutate(bus5.sample,
                    Cluster.membership=5,
)
write.csv(bus5.sample,file="bus5.sample.csv")



train1.sample<-sample(Sampledata.train.1$OBJECTID_MODE, 54, replace = FALSE, prob = NULL)
train1.sample<-as.data.frame(train1.sample)
train1.sample<-mutate(train1.sample,
                      Cluster.membership=1,
)
write.csv(train1.sample,file="train1.sample.csv")

train2.sample<-sample(Sampledata.train.2$OBJECTID_MODE, 84, replace = FALSE, prob = NULL)
train2.sample<-as.data.frame(train2.sample)
train2.sample<-mutate(train2.sample,
                      Cluster.membership=2,
)
write.csv(train2.sample,file="train2.sample.csv")

train3.sample<-sample(Sampledata.train.3$OBJECTID_MODE, 53, replace = FALSE, prob = NULL)
train3.sample<-as.data.frame(train3.sample)
train3.sample<-mutate(train3.sample,
                      Cluster.membership=3,
)
write.csv(train3.sample,file="train3.sample.csv")

train4.sample<-sample(Sampledata.train.4$OBJECTID_MODE, 3, replace = FALSE, prob = NULL)
train4.sample<-as.data.frame(train4.sample)
train4.sample<-mutate(train4.sample,
                      Cluster.membership=4,
)
write.csv(train4.sample,file="train4.sample.csv")

train5.sample<-sample(Sampledata.train.5$OBJECTID_MODE, 1, replace = FALSE, prob = NULL)
train5.sample<-as.data.frame(train5.sample)
train5.sample<-mutate(train5.sample,
                      Cluster.membership=5,
)
write.csv(train5.sample,file="train5.sample.csv")

tram1.sample<-sample(Sampledata.tram.1$OBJECTID_MODE, 54, replace = FALSE, prob = NULL)
tram1.sample<-as.data.frame(tram1.sample)
tram1.sample<-mutate(tram1.sample,
                     Cluster.membership=1,
)
write.csv(tram1.sample,file="tram1.sample.csv")

tram2.sample<-sample(Sampledata.tram.2$OBJECTID_MODE, 84, replace = FALSE, prob = NULL)
tram2.sample<-as.data.frame(tram2.sample)
tram2.sample<-mutate(tram2.sample,
                     Cluster.membership=2,
)
write.csv(tram2.sample,file="tram2.sample.csv")

tram3.sample<-sample(Sampledata.tram.3$OBJECTID_MODE, 53, replace = FALSE, prob = NULL)
tram3.sample<-as.data.frame(tram3.sample)
tram3.sample<-mutate(tram3.sample,
                     Cluster.membership=3,
)
write.csv(tram3.sample,file="tram3.sample.csv")

tram4.sample<-sample(Sampledata.tram.4$OBJECTID_MODE, 3, replace = FALSE, prob = NULL)
tram4.sample<-as.data.frame(tram4.sample)
tram4.sample<-mutate(tram4.sample,
                     Cluster.membership=4,
)
write.csv(tram4.sample,file="tram4.sample.csv")

tram5.sample<-sample(Sampledata.tram.5$OBJECTID_MODE, 1, replace = FALSE, prob = NULL)
tram5.sample<-as.data.frame(tram5.sample)
tram5.sample<-mutate(tram5.sample,
                     Cluster.membership=5,
)
write.csv(tram5.sample,file="tram5.sample.csv")

