require(xtable)
Data<-read.table("dat.txt",header=T)
str(Data)
fit.lm<-lm(Y~X,data=Data)

summary(fit.lm)
xtable(summary(fit.lm))

new.x<-data.frame(X=c(0,-5,5))
conf.Y<-predict(fit.lm,newdata = new.x,interval = "confidence",level = 0.99)
xtable(conf.Y)

anova(fit.lm)
xtable(anova(fit.lm))

pdf("scatterplot.pdf")
with(Data,plot(X,Y))
abline(fit.lm)
points(Data$X,fit.lm$fitted.values,pch=16,cex=0.75,col="red")
dev.off()

pdf("residual.pdf")
plot(fit.lm$fitted.values,fit.lm$residuals,xlab="fitted values",ylab="residulas")
abline(h=0,lty=2)
dev.off()

plot(fit.lm,which=1)
