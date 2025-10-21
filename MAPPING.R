#data
dat=read.csv(file.choose())
input <- dat[,3:13]

#normalization
library(clusterSim)
input=data.Normalization(input,type="n1",normalization="column")
head(input)
m=apply(input,2,mean)
s=apply(input,2,sd)
input=scale(input,m,s)



#FACTOR ANALYSIS
fact=factanal(input,3,rotation="varimax",scores="regression")


#euclidean distance
disance=dist(dat)

#heirarchical clustering
hc.c=hclust(disance)
clust=cutree(hc.c,k=6)
new=as.data.frame(cbind(fact$scores,clust))
attach(dat)
ne=data.frame(Country,clust)
-----------------------------------------------
#FACTORS
scores=data.frame(fact$scores)
attach(scores)
scores$New_Factor=Factor3*(-1)
attach(scores)
scores=data.frame(Factor1,Factor2,New_Factor)
attach(dat)
attach(scores)
fact1=data.frame(Country,Factor1)
names(fact1)[2]="Career_Driven"
head(fact1)

fact2=data.frame(Country,Factor2)
names(fact2)[2]="Socio_Economic"


fact3=data.frame(Country,New_Factor)
names(fact3)[2]="Eff_Planning"


---------------------------------------------
#MAPPING
intall.packages("dplyr")
install.packages("maps")
library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
theme_set(
  theme_void()
  )
#FACTOR1
fact1$region <-(fact1$Country)
head(fact1)


world_map <- map_data("world")

clust.map <- left_join(fact1, world_map, by = "region")
head(clust.map)
ggplot(clust.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Career_Driven ), color = "white")+
  scale_fill_viridis_c(option = "plasma")


#FACTOR2
fact2$region <-(fact2$Country)
head(fact2)


world_map <- map_data("world")

clust.map <- left_join(fact2, world_map, by = "region")
head(clust.map)
ggplot(clust.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Socio_Economic ), color = "white")+
  scale_fill_viridis_c(option = "cividis")



#FACTOR3
fact3$region <-(fact3$Country)
head(fact3)


world_map <- map_data("world")

clust.map <- left_join(fact3, world_map, by = "region")
head(clust.map)
ggplot(clust.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Eff_Planning ), color = "white")+
  scale_fill_viridis_c(option = "D")

-----------------------------------------------------------------------------------------------
#New Factor

scores=data.frame(fact$scores)
attach(scores)
scores$New_Factor=Factor3*(-1)
attach(scores)
scores=data.frame(Factor1,Factor2,New_Factor)


#normalization
library(clusterSim)
data=data.Normalization(scores,type="n4",normalization="column")
head



avg=data.frame(dat$Country, Means=rowMeans(data[,1:3]))
View(avg)

#FACTOR3
avg$region <-(avg$dat.Country)
head(avg)


world_map <- map_data("world")

clust.map <- left_join(avg, world_map, by = "region")
head(clust.map)
ggplot(clust.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Means), color = "white")+
  scale_fill_viridis_c(option = "E")























fact1$region <-(fact1$Country)
head(fact1)


world_map <- map_data("world")

clust.map <- left_join(fact1, world_map, by = "region")
head(clust.map)
ggplot(clust.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Career_Driven ), color = "white")+
  scale_fill_viridis_c(option = "plasma")


#FACTOR2
fact2$region <-(fact2$Country)
head(fact2)


world_map <- map_data("world")

clust.map <- left_join(fact2, world_map, by = "region")
head(clust.map)
ggplot(clust.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Socio_Economic ), color = "white")+
  scale_fill_viridis_c(option = "cividis")



#FACTOR3
fact3$region <-(fact3$Country)
head(fact3)


world_map <- map_data("world")

clust.map <- left_join(fact3, world_map, by = "region")
head(clust.map)
ggplot(clust.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Eff_Planning ), color = "white")+
  scale_fill_viridis_c(option = "D")

-----------------------------------------------------------------------------------------------
#New Factor

scores=data.frame(fact$scores)
attach(scores)
scores$New_Factor=Factor3*(-1)
attach(scores)
scores=data.frame(Factor1,Factor2,New_Factor)


#normalization
library(clusterSim)
data=data.Normalization(scores,type="n4",normalization="column")
head



avg=data.frame(dat$Country, Means=rowMeans(data[,1:3]))
View(avg)

#FACTOR3
avg$region <-(avg$dat.Country)
head(avg)


world_map <- map_data("world")

clust.map <- left_join(avg, world_map, by = "region")
head(clust.map)
ggplot(clust.map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Means), color = "white")+
  scale_fill_viridis_c(option = "E")


