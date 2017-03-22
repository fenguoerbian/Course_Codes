load("DOE_Lecture.rda")

alpha<-0.05
str(fabric)
a<-4 # randomly choose 4 levels of loom
n<-16/a # 16 observations in total, each level of loom have 16/4=4 replications 
fit<-aov(strength~loom,data=fabric)
out<-summary(fit)[[1]]
# In this assumption, we test whether the variance is zero, so no need to test model.tables nor TukeyHSD #


###### One-way random effects model: Method of moments estimation ######
sigma2<-c(out[2,3], (out[1,3]-out[2,3])/n)


###### The intra-class correlation: confidence interval ######
MS<-out[1,3]/out[2,3]
LR<-(MS/qf(c(1-alpha/2,alpha/2),a-1,a*(n-1))-1)/n
LR<-LR/(1+LR) # this is the confidence interval
rho<-sigma2[2]/sum(sigma2) # this is the intra-class correlation



###### Two-way random effects model ######
str(gauge)
head(gauge)
n<-2
a<-20
b<-3
fit<-aov(measure~part*operator,data=gauge)
( out<-summary(fit)[[1]] )
# modify the summary table. Firtst the F-value of part and operator #
out[c(1,2),4]<-out[1:2,3]/out[3,3]
# modify the summary table. Second the p-value of part and operator #
out[1:2,5]<-pf(out[1:2,4],out[1:2,1],out[3,1],lower.tail = F) 
out
sigma2<-c(noise=out[4,3],AB=(out[3,3]-out[4,3])/n,A=(out[1,3]-out[3,3])/b/n,B=(out[2,3]-out[3,3])/a/n)



###### Two-way random effects model, no-interaction ######
fit<-aov(measure~part+operator,data=gauge)
(out<-summary(fit)[[1]])
( sigma2<-c(noise=out[3,3], A=(out[1,3]-out[3,3])/b/n, B=(out[2,3]-out[3,3])/a/n) )



###### Two-way mixed effects model ######
