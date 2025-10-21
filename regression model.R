mycsv=read.table("",header=T,sep=",")
mycsv
mydata1=read.table(file.choose(),header=T,sep=",")
mydata1=read.table("C:\\Users\\user\\Downloads",header=T,sep=",")
mydata1
mymodel1=lm(Revenue ~ TV + NEWS,data=mydata1)
summary(mymodel1)
mydata=read.table(file.choose(),header=T,sep=",")
mydata
mydata2=mydata
mydata2$Smoker_new <-ifelse(mydata2$Smoker==c("Yes"),1,0)
mydata2$Diabetes_new <-ifelse(mydata2$Diabetes==c("Yes"),1,0)
mydata2$Fam_his_new <-ifelse(mydata2$Fam_his==c("Yes"),1,0)
head(mydata2)
mymodel2=lm(risk ~ Age + Pressure + Smoker_new + Diabetes_new + Fam_his_new,data=mydata2)
summary(mymodel2)

mymodel2=step(lm(risk ~ Age + Pressure + Smoker_new + Diabetes_new + Fam_his_new,data=mydata2))
summary(mymodel2)
attach(mydata2)
mydata2$predicted_value=41.227+23.661*Smoker_new+13.061*Diabetes_new+19.607*Fam_his_new
head(mydata2)
C:\Users\user\Downloads

mydata3=read.table("C:\\Users\\user\\Downloads\\Regression modelling.csv",header=T,sep=",")
mydata3
mydata3=read.table(file.choose(),header=T,sep=",")
mydata3
mymodel3=lm(EF ~ Age + BMI,data=mydata3)
summary(mymodel3)
# Stepwise method to Build Model
# ----------------------------------------------------------------
mymodel3=step(lm(EF ~ Age + BMI,data=mydata3))
summary(mymodel3)
#-----------------------------------------------------------------------------------------------------------
# PREDICTED Y BASED ON MODEL EQUATION
#-----------------------------------------------------------------------------------------------------------
mydata3$Predicted=predict(mymodel3,data=mydata3)
head(mydata3)
#-----------------------------------------------------------------------------------------------------------
# DETRMINING MEAN ABSOLUTE ERROR [MAE]
#-----------------------------------------------------------------------------------------------------------
mydata3$abs_error <- abs(mydata3$EF - mydata3$Predicted)
head(mydata3)
mae <- mean(mydata3$abs_error)
mae
#--------------------------------------------------------------------------
# DETRMINING MEAN ABSOLUTE PERCENTAGE ERROR [MAPE]
#--------------------------------------------------------------------------
mydata3$per_abs_error <- abs((mydata3$EF - mydata3$Predicted)/ mydata3$EF)
mydata3
mape <- mean(mydata3$per_abs_error)*100
mape
===========================================================================











\