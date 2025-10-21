

data1=read.csv(file.choose(),header=T,sep=",")
head(data1)
library(mice)
impute=mice(data1,method="cart",m=1)
impute1=complete(impute)
head(impute1)
data2=data.frame(impute1)
write.csv(data2,"C:\\Users\\SALVIA ANNA JOSEPH\\Desktop\\test_data.csv",row.names=F)
