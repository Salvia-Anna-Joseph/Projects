mydata=read.table(file.choose(),header=T,sep=",")
mydata
mymodel=lm(charges ~ age + bmi,data=mydata)
summary(mymodel)

