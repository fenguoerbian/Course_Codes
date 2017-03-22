load("DOE_Lecture.rda")

###### Choice of sample size, illustration ######
alpha<-0.05
n<-c(4,5,7,10,15,20,30,50,100)
d<-seq(0,3,length.out=101)

p<-outer(d,n, function(d,n,alpha) 
  {
  ncp<-sqrt(n)*d
  t0<-qt(1-alpha/2,df=n-1)
  return(pt(t0,df=n-1,ncp=ncp,lower.tail = F)+pt(-t0,df=n-1,ncp=ncp))
},alpha)

matplot(d,p,type="l",ylim=c(0,1),xlab="d",ylab="power")
legend(2.2,0.7,legend=n,lty=1:length(n),col=1:length(n))

# choose from the top-right region
abline(v=1,h=0.8,lty=2)


###### Choice of sample size ######
x<-power.t.test(n=2:10,delta=0.5,sd=0.25)
names(x$power)=2:10
round(x$power,3)



###### An example ######
Data<-fiber
boxplot(Data$strength~Data$weight)



###### Analysis of variance ######
fiber.aov<-aov(strength~weight,data=fiber)

### these 2 commands below are the same for aov object ###
summary(fiber.aov)
anova(fiber.aov)

### Regression analysis, model checking ###
# diagnostic plot #
par(mfrow=c(2,2))
plot(fiber.aov)
par(mfrow=c(1,1))

###### Analysis of variance, parameter estimation ###
model.tables(fiber.aov,se=TRUE,type="means")
model.tables(fiber.aov,se=TRUE,type="effect")




###### Regression analysis, the design matrix ######
### There are 2 ways to generate different design matrix ###

# 1.generate the desigh matrix using globa option #
options(contrasts=c("contr.sum","contr.poly"))
fiber.aov2<-aov(strength~weight,data=fiber)
model.matrix(fiber.aov2)      # this only takes effect if the model is fitted after we adjusted the options parameter
model.matrix(fiber.aov)       # the design matrix of fiber.aov doesn't take effect after

# 2.generate the design matrix using parameter in model.matrix #
# the formula does not depend on response #
X1<-model.matrix(~weight,data=fiber,contrasts=list(weight="contr.sum"))
X2<-model.matrix(~weight,data=fiber,contrasts=list(weight="contr.poly"))
X3<-model.matrix(~weight,data=fiber,contrasts=list(weight="contr.treatment"))

