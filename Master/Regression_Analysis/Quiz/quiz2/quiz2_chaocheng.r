require(xtable)

### weighted
rm(list=ls())
Data<-read.table("wls.txt",header=T)
str(Data)
with(Data,plot(x,y))


Data.lm<-lm(y~x,data=Data,weights=w)
abline(Data.lm)

pdf("W_scatter.pdf")
with(Data,plot(x,y))
abline(Data.lm)
dev.off()


summary(Data.lm)
anova(Data.lm)

xtable(summary(Data.lm))
xtable(anova(Data.lm))

new.x<-seq(min(Data$x),max(Data$x),length.out=100)
plot.y<-predict(Data.lm,newdata=data.frame(x=new.x),interval="confidence",level=0.99)

pdf("W_CI.pdf")
with(Data,plot(x,y))
matlines(new.x,plot.y,col=c(1,2,2),lty=c(1,2,2),lwd=2)
legend(1.5,7,c("fitted line","99% confidence band"),lty=1:2,col=1:2,lwd=2)
dev.off()

###### plolynomial ######
rm(list=ls())
Data<-read.table("poly.txt",header=T)
str(Data)
Data.lm<-lm(y~x,data=Data)
summary(Data.lm)
xtable(summary(Data.lm))

## quadratic
Data.lm2<-lm(y~x+I(x^2),data=Data)
summary(Data.lm2)
xtable(summary(Data.lm2))

## cubic
Data.lm3<-lm(y~x+I(x^2)+I(x^3),data=Data)
summary(Data.lm3)
xtable(summary(Data.lm3))

### anova
anova(Data.lm)
anova(Data.lm2)
anova(Data.lm3)

xtable(anova(Data.lm))
xtable(anova(Data.lm2))
xtable(anova(Data.lm3))

### scatter plot
pdf("P_CI.pdf")
with(Data,plot(x,y))
abline(Data.lm,col=2,lwd=2)
new.x<-seq(min(Data$x),max(Data$x),length.out=100)
y1<-predict(Data.lm2,newdata=data.frame(x=new.x))
y2<-predict(Data.lm3,newdata=data.frame(x=new.x))
matlines(new.x,cbind(y1,y2),lty=1,col=3:4,lwd=2)
legend(-3,17,c("linear","quadratic","cubic"),lty=1,col=2:4,lwd=2)
dev.off()
### model choose###
anova(Data.lm,Data.lm2)
anova(Data.lm2,Data.lm3)

xtable(anova(Data.lm,Data.lm2))
xtable(anova(Data.lm2,Data.lm3))

