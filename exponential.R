
countobs=function(r,l,u)
{obs=ifelse(r>=l & r<u ,1,0)
sum(obs)
}
freq=c(countobs(r,0,50),countobs(r,50,100),countobs(r,100,150),countobs(r,150,200),countobs(r,200,250))
freq
class=c("0-50","50-100","100-150","150-200","200-250")
class
data.frame(class,freq)





#----random sample from uniform distribution----#
h<-runif(2000)
#---specifying value of theta--#
theta<-3
#----determining value taken by exponential variate---#
exp_sample<- log(1-h)*(-theta)
#----frequency table---#
exp_sample
r=round(exp_sample)
r
r1=table(round(exp_sample))
data.frame(r1)
hist(r1)


#----random sample from uniform distribution----#
h<-runif(2000)
#---specifying value of theta--#
theta<-8
#----determining value taken by exponential variate---#
exp_sample<- log(1-h)*(-theta)
#----frequency table---#
r=round(exp_sample)
r2=table(round(exp_sample))
freq=c(countobs(r,0,10),countobs(r,10,20),countobs(r,20,30),countobs(r,30,40),countobs(r,40,50),countobs(r,50,60))
freq
class=c("0-10","10-20","20-30","30-40","40-50","50-60")
class
data.frame(class,freq)
hist(r2)


#----random sample from uniform distribution----#
h<-runif(2000)
#---specifying value of theta--#
theta<-25
#----determining value taken by exponential variate---#
exp_sample<- log(1-h)*(-theta)
#----frequency table---#
r=round(exp_sample)
r3=table(round(exp_sample))
data.frame(r3)
freq=c(countobs(r,0,50),countobs(r,50,100),countobs(r,100,150),countobs(r,150,200),countobs(r,200,250))
freq
class=c("0-50","50-100","100-150","150-200","200-250")
class
data.frame(class,freq)
hist(r3)




#----random sample from uniform distribution----#
h<-runif(2000)
#---specifying value of theta--#
theta<-12
#----determining value taken by exponential variate---#
exp_sample<- log(1-h)*(-theta)
#----frequency table---#
r=round(exp_sample)
r4=table(round(exp_sample))
data.frame(r4)
freq=c(countobs(r,0,20),countobs(r,20,40),countobs(r,40,60),countobs(r,60,80),countobs(r,80,100),countobs(r,100,120))
freq
class=c("0-20","20-40","40-60","60-80","80-100","100-120")
class
data.frame(class,freq)
hist(r4)

layout(matrix(c(1,2,3,4),2,2))
hist(r1)
hist(r2)
hist(r3)
hist(r4)


h<-runif(5000)
theta<-3
exp_sample<-log(1-h)*(-theta)
r=round(exp_sample)
k=exp_sample^(1/3.6)
k1=table(round(k))
data.frame(k1)