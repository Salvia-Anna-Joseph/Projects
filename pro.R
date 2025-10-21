#IMPUTING MISSING VALUES
data=read.table(file.choose(),header=T,sep=",")
head(data)
library(VIM)
library(lattice)
install.packages("VIM")
install.packages("lattice")
library(mice)
md.pattern(data)
a=md.pairs(data)
impute=mice(data,m=1,method="cart")
im=complete(impute)
head(im)
impute$im$hos_beds
impute$im$ch_pop
write.csv(im="C:\\Users\\SALVIA ANNA JOSEPH\\Desktop\\final project\\corona",row.names=F)

#CORRELATION#
data=read.table(file.choose(),header=T,sep",")
cor(data[,2:14])