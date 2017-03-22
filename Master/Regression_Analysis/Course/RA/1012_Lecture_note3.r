load("RA_Lecture.rda")



###### anonymous ######
w<-c(rep(1,22),0.25)
fit.lm<-lm(Y~X,data=anonymous)
fit.wlm<-lm(Y~X,data=anonymous,weights = w)
with(anonymous,plot(X,Y))
abline(fit.lm,col=4,lty=1,lwd=0.5)
abline(fit.wlm,col=2,lty=2,lwd=0.5)




###### killrat ######
str(killrat)

### How to get wei and var?
# Method 1, use split and sapply
tmp<-split(killrat$Dose,killrat$Rate)
wei<-sapply(tmp,var)
len<-sapply(tmp,length)

# Method 2, use tapply
wei<-tapply(killrat$Dose,killrat$Rate,var)
len<-tapply(killrat$Dose,killrat$Rate,length)

# Method 3, use aggregate, the default usage
wei<-aggregate(killrat$Dose,by=list(Rate=killrat$Rate),FUN=var)$x
len<-aggregate(killrat$Dose,by=list(Rate=killrat$Rate),FUN=length)$x

#Method4, use aggregate, the fomula usage
wei<-aggregate(Dose~Rate,data=killrat,FUN=var)$x
len<-aggregate(Dose~Rate,data=killrat,FUN=length)$x

######
#  MAPPLY
#
#  For wei and len that are not data frames, unlist could be used to paste all the list components
#
#  M<-mapply(rep,wei,len,SIMPLIFY=F)
#  M<-unlist(M,use.name=F)  
#  
#  or, you can transform them to data frames
#
######

wei<-data.frame(wei)
len<-data.frame(len)
W<-mapply(rep,wei,len)
W<-1/W

fit.lm<-lm(Dose~Rate,data=killrat)
fit.wlm<-lm(Dose~Rate,data=killrat,weights=W)

with(killrat,plot(Rate,Dose,type="n"))

########
# previously I used this:
#
# with(killrat,points(Rate[Rate==1],Dose[Rate==1],pch=1,col=1))
# with(killrat,points(Rate[Rate==2],Dose[Rate==2],pch=2,col=2))
# with(killrat,points(Rate[Rate==4],Dose[Rate==4],pch=3,col=3))
# with(killrat,points(Rate[Rate==8],Dose[Rate==8],pch=4,col=4))
#
# Now I use this
########

k<-1:4
trash<-with(killrat,sapply(1:4,FUN=function(k) points(Rate[Rate==2^(k-1)],Dose[Rate==2^(k-1)],pch=k,col=k)))
rm(trash)

legend(4.5,25,paste("weight=",t(1/wei),sep=""),pch=k,col=k)

abline(fit.lm,col=2)
abline(fit.wlm,col=4,lty=2)





###### weightloss ######
str(weightloss)

### The quadratic polynomial fit: wtloss ###
plot(weightloss,cex=0.5)
abline(lm(Weight~Days,data=weightloss),col=4,lty=2)

wt.plm<-lm(Weight~Days+I(Days^2),data=weightloss)
new.x<-seq(min(weightloss$Days),max(weightloss$Days),length.out=100)
plot.x<-data.frame(Days=new.x)
new.y<-predict(wt.plm,newdata=plot.x)
lines(new.x,new.y,col=2)



### residual analysis: wtloss ###
wt.lm<-lm(Weight~Days,data=weightloss)
par(mfrow=c(2,2))

# simple linear regression, residual qq-plot
qqnorm(residuals(wt.lm))
qqline(residuals(wt.lm),col=2)

# quadratic polynomial regression, residual qq-plot
qqnorm(residuals(wt.plm))
qqline(residuals(wt.plm),col=2)

# simple linear regression, residual vs. fitted values
wt.lm.res<-residuals(wt.lm)
wt.lm.fit<-fitted(wt.lm)
plot(wt.lm.fit,wt.lm.res,xlab="fitted values",ylab="residuals")
abline(h=0,lty=2,col=4)

# quadratic polynomial regression, residua vs. fitted values
wt.plm.res<-residuals(wt.plm)
wt.plm.fit<-fitted(wt.plm)
plot(wt.plm.fit,wt.plm.res,xlab="fitted values",ylab="residuals")
abline(h=0,lty=2,col=4)

par(mfrow=c(1,1))

