load("RA_Lecture.rda")

require(scatterplot3d)
require(ellipse)


steamI.lm<-lm(steam~temperature,data=steamI)
steamII.lm<-lm(steam~temperature+day,data=steamII)

steamI.summary<-summary(steamI.lm)
steamI.anova<-anova(steamI.lm)




###### scatterplot3d for steamII ######
tmp<-scatterplot3d(steamII,type="h",color="blue",box=F,angle=50)
k=c(7,19)
tmp$points3d(steamII[k,],col=2,type="h")

tmp=scatterplot3d(steamII,color="blue",type="h",box=F,angle=50)
tmp$plane3d(steamII.lm)
tmp$points3d(steamII[k,],col=2,type="h")




###### confidence region for steamI ######
alpha<-0.05

### joint confidence region ###
tmp<-ellipse(steamI.lm,level=1-alpha)
plot(tmp,type="l")
polygon(tmp,col="yellow",border="red")
points(coef(steamI.lm)[1],coef(steamI.lm)[2],pch=16,col="red")

### individual confidence region ###
xpos<-coef(steamI.lm)[1]+qt(1-alpha/2,df=steamI.summary$df[2])*c(-1,1)*coef(steamI.summary)[1,2]
ypos<-coef(steamI.lm)[2]+qt(1-alpha/2,df=steamI.summary$df[2])*c(-1,1)*coef(steamI.summary)[2,2]
rect(xpos[1],ypos[1],xpos[2],ypos[2],border="green",lty=2)

### bonferroni confidence region ###
xpos<-coef(steamI.lm)[1]+qt(1-alpha/4,df=steamI.summary$df[2])*c(-1,1)*coef(steamI.summary)[1,2]
ypos<-coef(steamI.lm)[2]+qt(1-alpha/4,df=steamI.summary$df[2])*c(-1,1)*coef(steamI.summary)[2,2]
rect(xpos[1],ypos[1],xpos[2],ypos[2],border="blue",lty=1)



###### 2D confidence region for steamII ######
par(mfrow=c(2,2))
k<-c(1,2)
tmp<-ellipse(steamII.lm,which=k)
plot(tmp,type="l")
polygon(tmp,col="yellow",border="red")
points(t(coef(steamII.lm)[k]),pch=16,col="red")

k<-c(1,3)
tmp<-ellipse(steamII.lm,which=k)
plot(tmp,type="l")
polygon(tmp,col="yellow",border="red")
points(t(coef(steamII.lm)[k]),pch=16,col="red")

k<-c(2,3)
tmp<-ellipse(steamII.lm,which=k)
plot(tmp,type="l")
polygon(tmp,col="yellow",border="red")
points(t(coef(steamII.lm)[k]),pch=16,col="red")

par(mfrow=c(1,1))



###### Dose the order matter? ######
steamII.lm2<-lm(steam~day+temperature,data=steamII)
summary(steamII.lm)
summary(steamII.lm2)

anova(steamII.lm)
anova(steamII.lm2)



###### the plot of response: steamII ######
plot(steamII$steam,type="l",lty=1,col="red",xlab="month",ylab="steam")
points(steamII$steam,pch=1,col="red")
lines(fitted(steamII.lm),type="l",lty=2,col="green")
points(fitted(steamII.lm),pch=16,col="green")
legend(16,12,c("observed","fitted"),lty=c(1,2),pch=c(1,16),col=c("red","green"))




###### The residual plots: steamII ######
par(mfrow=c(2,2))

error<-residuals(steamII.lm)
qqnorm(error)
qqline(error)

plot(fitted(steamII.lm),error,xlab="fitted value",ylab="residuals")
abline(h=0,lty=3,col="red")


plot(steamII$temperature,error,xlab="temperature",ylab="residual")
abline(h=0,lty=3,col="red")

plot(steamII$day,error,xlab="day",ylab="residual")
abline(h=0,lty=3,col="red")

par(mfrow=c(1,1))


###### The 3D residual plot: steamII ######
tmp<-scatterplot3d(x=steamII$temperature,y=steamII$day,z=error,box=F,pch=16,color="blue",cex.symbols = 0.5,xlab="temperature",ylab="day",zlab="residual")
tmp$plane3d(lm(error~steamII$temperature+steamII$day))



###### The residuals: steam II ######
par(mfrow=c(2,2))
plot(residuals(steamII.lm),xlab="",ylab="residual",main="residulas",pch=1)

plot(rstandard(steamII.lm),xlab="",ylab="residual",main="standardized residuals",pch=3,col="red")

plot(rstudent(steamII.lm),xlab="",ylab="residual",main="studentized residuals",col="green",pch=16)

matplot(cbind(residuals(steamII.lm),rstandard(steamII.lm),rstudent(steamII.lm)),pch=c(1,3,16),col=c("black","red","green"),xlab="",ylab="residual",main="comparison")
par(mfrow=c(1,1))



###### Influential observations ######
s_i<-rstandard(steamII.lm)
h_i<-hatvalues(steamII.lm)
D_i<-cooks.distance(steamII.lm)

par(mfrow=c(2,2))

plot(abs(s_i),type="h",ylab=expression(paste("|",s[i],"|",sep="")))
k<-sort.list(abs(s_i),decreasing=T)[1:3]
points(k,abs(s_i[k]),pch=16,col=2)
text(k+1,abs(s_i[k]),k,col=4)

plot(abs(h_i),type="h",ylab="hat value",xlab="")
k<-sort.list(abs(h_i),decreasing=T)[1:3]
points(k,abs(h_i)[k],pch=16,col=2)
text(k+1,abs(h_i)[k],k,col=4)

plot(D_i,type="h",ylab="Cook's distance",xlab="")
k<-sort.list(D_i,decreasing=T)[1:3]
points(k,D_i[k],pch=16,col=2)
text(k+1,D_i[k],k,col=4)

steamII.lm3<-lm(steam~temperature+day,data=steamII[-19,])
new.fit<-predict(steamII.lm3,newdata=steamII[,-3])
plot(fitted(steamII.lm)-new.fit,ylab="Difference in fit",xlab="")

par(mfrow=c(1,1))



###### the scatter plot: score ######
plot(score)
k<-c(2,18,19)
points(score[k,],pch=16,col=2)
text(score[k[-2],1]+1,score[k[-2],2],k[-2],col=4)
text(score[k[2],1]-1,score[k[2],2],k[2],col=4)

score.lm<-lm(Y~X,data=score)
score.lm2<-lm(Y~X,data=score[-18,])
abline(score.lm,col=4)
abline(score.lm2,col=2,lty=2)




###### The cook's distance: score ######
cd<-cooks.distance(score.lm)
plot(cd,type="h",xlab="",ylab="cook's distance")
k<-sort.list(cd,decreasing=T)[1]
points(k,cd[k],pch=16,col=2)
text(k+1,cd[k],k,col=4)
