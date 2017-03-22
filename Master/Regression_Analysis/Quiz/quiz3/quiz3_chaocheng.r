###### !!! Note: I have changed my laptop. 
###        Found that pdf() could not save the figure properly(letters all disappear in the figure).
###        So I used cario_pdf() instead.


require(xtable)
require(MASS)


###### 1 ######
rm(list=ls())
Data<-read.table("dat1.txt",header=T)
str(Data)

### scatter plot ###
cairo_pdf("1_scatter.pdf")
with(Data,plot(x,y,type="n"))
with(Data[Data$set==1,],points(x,y,pch=2,col=2))
with(Data[Data$set==2,],points(x,y,pch=3,col=3))
legend(0,6,c("set=1","set=2"),pch=2:3,col=2:3)
dev.off()
### linear model ###
Data.lm<-lm(y~x,data=Data)
xtable(summary(Data.lm))
xtable(anova(Data.lm))

### confidence band ###
new.x<-seq(min(Data$x),max(Data$x),length.out=10)
plot.y<-predict(Data.lm,data.frame(x=new.x),interval="confidence",level=0.99)
matlines(new.x,plot.y,col=c(1,4,4),lty=c(1,2,2))

### dummy variable ###
Data.lm2<-lm(y~x+set+set:x,data=Data)
xtable(anova(Data.lm,Data.lm2))



###### 2 ######
rm(list=ls())
Data<-read.table("dat2.txt",header=T)
str(Data)

### linear ###
with(Data,plot(x,y))
Data.lm<-lm(y~x+I(x^2),data=Data)
xtable(summary(Data.lm))

new.x<-seq(min(Data$x),max(Data$x),length.out=20)
plot.y<-predict(Data.lm,data.frame(x=new.x))
lines(new.x,plot.y)


plot(Data.lm)

### boxcox ###
lambda<-boxcox(Data.lm,lambda=seq(0,1,0.05))
lambda$x[which.max(lambda$y)]

### appropiate lambda ###
l0<-0.5
n<-length(Data$y)
ydot<-prod(Data$y)^(1/n)
v<-ydot^(1-l0)*(Data$y^l0-1)/l0
Data.lm2<-lm(v~x,data=Data)
xtable(summary(Data.lm2))
anova(Data.lm2)

plot(Data$x,v,xlab="x",ylab="transformed response")
abline(Data.lm2)

res<-resid(Data.lm2)
res_t<-rstudent(Data.lm2)
res_s<-rstandard(Data.lm2)
res_c<-cooks.distance(Data.lm2)

cairo_pdf("test2.pdf",width=28,height=14)
par(mfrow=c(2,4))
plot(Data.lm2)
plot(abs(res),type="h",ylab="residual")
plot(abs(res_t),type="h",ylab="studentized residual")
plot(abs(res_s),type="h",ylab="standardized residual")
plot(res_c,type="h",ylab="Cook's distance")
par(mfrow=c(1,1))
dev.off()
