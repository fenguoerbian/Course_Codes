load("RA_Lecture.rda")
str(steamI)
with(steamI,plot(temperature,steam))

fit.lm<-lm(steam~temperature,data=steamI)
abline(fit.lm)
par(mfrow=c(2,2))
plot(fit.lm)
par(mfrow=c(1,1))

summary(fit.lm)
anova(fit.lm)
confint(fit.lm)

min.x<-min(steamI$temperature)
max.x<-max(steamI$temperature)
new.x<-data.frame(temperature=seq(from=min.x,to=max.x,length.out=100))
conf.y<-predict(fit.lm,new.x,interval="confidence")
pred.y<-predict(fit.lm,new.x,interval="predict")

matplot(new.x,conf.y,type="l",lty=c(1,2,2),col=c(1,2,2))
matlines(new.x,pred.y,lty=c(1,3,3),col=c(1,3,3))



