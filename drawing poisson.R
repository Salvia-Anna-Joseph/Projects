drawing samples from poisson distribution
theta<-6
set.seed(1)
x<-runif(10)
exp_sample<-log(1-x)/(-theta)
cu<-cumsum(exp_sample)
sa<-subset(cu,cu<=1)
sample=length(sa)





theta<-5
sample1=1:1000
for(i in 1: 1000)
{
set.seed(i+29)
x<-runif(1000)
exp_sample1<-log(1-x)/(-theta)
cu1<-cumsum(exp_sample1)
sa1<-subset(cu1,cu1<=1)
sample1[i]=length(sa1)
}
sample1


theta<-2
sample2=1:1000
for(i in 1: 1000)
{
set.seed(i+29)
x<-runif(1000)
exp_sample2<-log(1-x)/(-theta)
cu2<-cumsum(exp_sample2)
sa2<-subset(cu2,cu2<=1)
sample2[i]=length(sa2)
}
sample2


theta<-4
sample3=1:1000
for(i in 1: 1000)
{
set.seed(i+29)
x<-runif(1000)
exp_sample3<-log(1-x)/(-theta)
cu3<-cumsum(exp_sample3)
sa3<-subset(cu3,cu3<=1)
sample3[i]=length(sa3)
}
sample3

layout(matrix(1,2,3,4)2,2))
plot()