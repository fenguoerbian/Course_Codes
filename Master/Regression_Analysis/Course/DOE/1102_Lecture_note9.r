require(combinat)
load("DOE_Lecture.rda")



###### 
cement
boxplot(cement)

###### Inference given two samples, Two sample t-test ######
with(cement,t.test(modified,unmodified,var.equal = T))

### the two sample t-test is equal in linear regression ###
x<-rep(c(0,1),each=10)
y<-c(cement$unmodified,cement$modified)
temp<-lm(y~x)
summary(temp)
confint(temp)



###### Inference given two samples, the randomization test ######
x<-combn(20,10)
y<-with(cement,c(modified,unmodified))
tstat<- function(a,b)
{
  (mean(a)-mean(b))/sqrt((var(a)+var(b))/length(a))
}
allt<-apply(x,2,function(u) tstat(y[u],y[-u]))
(t0<-tstat(y[1:10],y[-(1:10)]))
count<-sum(allt<= -abs(t0)) + sum(allt>=abs(t0))
(pvalue<-count/length(allt))



###### Paired comparison ######
str(hardness)
with(hardness, t.test(tip1,tip2,paired = T))
