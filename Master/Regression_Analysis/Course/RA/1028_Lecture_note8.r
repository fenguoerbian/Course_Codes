require(car)
require(leaps)
require(MASS)
require(xtable)
require(lattice)
require(rgl)



load("RA_Lecture.rda")

###### An analysis with all predictors #####
str(hald)
hald.lm<-lm(Y~X1+X2+X3+X4,data=hald)
summary(hald.lm)
vif(hald.lm)



###### The pairwise scatter plots of inputs: hald ######
pairs(hald[,1:4],col=4)



###### Exhaustive search or best subset ######
hald.r2<-leaps(hald[,1:4],hald[,5],method="r2")
hald.adj2<-leaps(hald[,1:4],hald[,5],method="adjr2")
hald.cp<-leaps(hald[,1:4],hald[,5],method="Cp")
str(hald.r2)
hald.r2

### try to compute MSE ###
# p: includes the intercept #
# C_p = SSE / s^2 - (n-2p) #
# s^2 is the sigma^2 of the full model #
# MSE = SSE/(n-p) #

n<-nrow(hald)
hald.full<-lm(Y~X1+X2+X3+X4,data=hald)
s2<-summary(hald.full)$sigma^2
hald.MSE=(hald.cp$Cp+(n-2*hald.cp$size))*s2/(n-hald.cp$size)

hald.tab<-matrix(0,ncol=9,nrow=nrow(hald.cp$which))
hald.tab[,1:4]<-cbind(hald.r2$r2,hald.adj2$adjr2,hald.cp$Cp,hald.MSE)
hald.tab[,5]<-hald.r2$size
hald.tab[,6:9]<-hald.cp$which
colnames(hald.tab)<-c("R2","AdjR2","Cp","MSE","Size","X1","X2","X3","X4")
xtable(hald.tab)



###### The criteria against the model size ######
par(mfrow=c(2,2))
plot(hald.r2$size,hald.r2$r2,xlab="size",ylab="R2")
plot(hald.adj2$size,hald.adj2$adjr2,xlab="size",ylab="AdjR2")
plot(hald.cp$size,hald.cp$Cp,xlab="size",ylab="Cp")
plot(hald.cp$size,hald.MSE,xlab="size",ylab="MSE")
par(mfrow=c(1,1))



###### A close-up of the C_p plot of those models with C_p \leq 6 ######
plot(hald.cp$size,hald.cp$Cp,xlab="Size",ylab=expression(C[p]),xlim=c(0,5),ylim=c(0,6),pch=16,col=4)
abline(a=0,b=1,lty=1,col=3)




###### The post-processing of a selected model ######
stepAIC(hald.full,direction = "both")
stepAIC(hald.full,direction = "backward")
stepAIC(lm(Y~X1,data=hald),scope="Y~X1+X2+X3+X4",direction = "forward")
# this k=log(n) is BIC
stepAIC(lm(Y~X1,data=hald),scope = "Y~X1+X2+X3+X4",direction = "forward",k=log(n))
# we save one result for later use #
# by dropterm(), we can select model via other tests #
hald.AIC<-stepAIC(hald.full)
dropterm(hald.AIC,test="F")


###### The backward selection applied to hald ######
hald.back<-stepAIC(hald.full,direction="backward")


###### The minimum AIC model in hald ######
summary(hald.back)



###### Post-processing in hald #####
dropterm(lm(Y~X1+X2+X4,data=hald),test="F")
dropterm(lm(Y~X1+X2,data=hald),test="F")



###### The response surface: hald ######
newx1<-seq(min(hald$X1),max(hald$X1),length.out=50)
newx2<-seq(min(hald$X2),max(hald$X2),length.out=50)
newx<-data.frame(X1=newx1,X2=newx2)
hald.lm<-lm(Y~X1+X2,data=hald)
ploty<-outer(newx1,newx2,function(a,b) predict(hald.lm,data.frame(X1=a,X2=b)))
persp(newx1,newx2,ploty,col=3,theta=120,phi=15,xlab="X1",ylab="X2",zlab="Y")
# an interactive 3-D plot
persp3d(newx1,newx2,ploty,col=3)




###### ridge ######
example("lm.ridge")
