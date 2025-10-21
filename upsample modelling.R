data=read.csv(file.choose())
head(data)
install.packages("ISLR")
library(ISLR)
sample_size=floor(0.80*nrow(data))
set.seed(500)
train=sample(seq_len(nrow(data)),size=sample_size)
train_data=data[train,]
test_data=data[-train,]
head(train_data)
head(test_data)
nrow(test_data)
nrow(train_data)

write.csv(test_data,"C:\\Users\\SALVIA ANNA JOSEPH\\Desktop\\upsample_test.csv",row.names=F)
write.csv(train_data,"C:\\Users\\SALVIA ANNA JOSEPH\\Desktop\\upsample_train.csv",row.names=F)



train_data=read.csv(file.choose())
head(data)
model_train=lm(spread_rate~pop.kmsq+population+hos_beds+climate+temp+arrivals+departures+ch_pop+gdp,data=test_data)
summary(model_train)
test_data=read.csv(file.choose())
validation=predict(model_train,data=test_data)
head(validation)