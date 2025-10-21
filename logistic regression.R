d1=read.csv(file.choose(),header=T,sep=",")
head(d2)
library(mice)
library(lattice)
impute=mice(d1,method="cart",m=1)
impute_new=complete(impute)
d2=data.frame(impute_new)
write.csv(d2,"C:\\Users\\SALVIA ANNA JOSEPH\\Desktop\\final project\\testdist.csv")

fix(d1)



data=read.csv(file.choose(),header=T,sep=",")
str(data)
data$airports=as.numeric(data$airports)
data1=scale(data[,-c(1,2,7)])
data2=cbind(data1,data$reported)
d=data.frame(data2)
head(d)
colnames(d)=c("gdp", "arrivals","departures","population","airports","ch_pop","distance_km","reported")
names(d)
model=glm(reported~gdp+arrivals+departures+population+ch_pop+distance_km+airports,data=d,family="binomial")
summary(model)

#multicollinearity

install.packages("car")
install.packages("caret")
model=lm(reported~arrivals+ch_pop+distance_km,data=test)
summary(model)
test=read.csv(file.choose())
head(test)
ols_eigen_cindex(model)
library(car)
vif(model)

