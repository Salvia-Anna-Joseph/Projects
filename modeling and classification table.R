#train and test data#
data=read.csv(file.choose())
head(data)
install.packages("ISLR")
library(ISLR)
sample_size=floor(0.80*nrow(data))
set.seed(200)
train=sample(seq_len(nrow(data)),size=sample_size)
train_data=data[train,]
test_data=data[-train,]
head(train_data)
head(test_data)
nrow(test_data)
nrow(train_data)

write.csv(data1_score,"C:\\Users\\SALVIA ANNA JOSEPH\\Desktop\\new_0.9.csv",row.names=F)
write.csv(train_data,"C:\\Users\\SALVIA ANNA JOSEPH\\Desktop\\final project\\sub_data.csv",row.names=F)

data1=read.csv(file.choose())
head(data1)
distance=scale(data1$distance_km)
cbind(data1,distance)
as.numeric(data1$airports)
model1=glm(reported~arrivals+distance+ch_pop,data=data1,family="binomial")
summary(model1)
install.packages("ROCR")
score=predict(model1,data=data1,type="response")
head(score)

data1_score = cbind(data1,score) 
head(data1_score) 

data1_score$pred = ifelse(data1_score$score<0.5,0,1) 
cls_table=table(data1_score$reported,data1_score$pred) 
cls_table 
rownames(cls_table)=c("No","Yes") 
colnames(cls_table)=c("No","Yes") 
cls_table 
prop.table(cls_table,1)
overall_perf=(sum(prop.table(cls_table,1)[1,1],prop.table(cls_table,1)[2,2]))/2 
overall_perf
error_rate=(sum(prop.table(cls_table,1)[1,2],prop.table(cls_table,1)[2,1]))/2 
# install.packages("gmodels") 
library(gmodels) 
CrossTable(data1_score$reported,data1_score$pred,prop.chisq=T) 


cutpoints = seq(0,1,0.01) 
length(cutpoints)
 sensitivity = seq(1,101,1)
 specificity = seq(1,101,1) 
cutpoint_performance = cbind(cutpoints,sensitivity,specificity) 
table(data1_score$reported)
for(i in 1:101)
{ 
data1_score$pred= ifelse(data1_score$score<cutpoint_performance[i,1],0,1)
data1_score$sumed = data1_score$pred+data1_score$reported 
data1_score$pred1_1 = ifelse(data1_score$sumed==2,1,0)
correct1_1 = sum(data1_score$pred1_1) 
data1_score$pred0_0 = ifelse(data1_score$sumed==0,1,0) 
correct0_0 = sum(data1_score$pred0_0) 
cutpoint_performance[i,2] = correct1_1/11 
cutpoint_performance[i,3] = correct0_0/21 
}
cutvalue_table = data.frame(cutpoint_performance) 

cutpoint_performance=data.frame(cutpoint_performance)
plot(cutpoint_performance$cutpoints,cutpoint_performance$sensitivity,"o",pch=8,col=6,main="Chart for sensitivity and specificity")
lines(cutpoint_performance$cutpoints,cutpoint_performance$specificity,"o",pch=13,col="blue") 

cutvalue_table$diff = abs(cutvalue_table$sensitivity-cutvalue_table$specificity)
head(cutvalue_table) 
optimum_value=subset(cutvalue_table,diff==min(diff))
optimum_value 
data1_score$pre_class=ifelse(data1_score$score<0.9,0,1)
head(data1_score)
  
attach(data1_score)
tble=table(reported,pre_class) 
tble 

library(pROC)
roc(data1_score$reported,model1$fitted.values,plot=T)