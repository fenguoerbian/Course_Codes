###### Polynomial regression: chemical example ######
require(lattice)
load("RA_Lecture.rda")


chemical.lm<-lm(Y~(X1+X2+X3)^2+I(X1^2)+I(X2^2)+I(X3^2),data=chemical)

###### The estimates in the quadratic model: chemical ######
summary(chemical.lm)
anova(chemical.lm)

###### Some diagnostic plot ######
res.res<-residuals(chemical.lm)
res.sta<-rstandard(chemical.lm)
res.stu<-rstudent(chemical.lm)
chemical.cook<-cooks.distance(chemical.lm)
chemical.fit<-fitted(chemical.lm)

par(mfrow=c(2,2))

plot(chemical.lm)


plot(chemical.fit,res.res,xlab="fitted values",ylab="residuals")
abline(h=0,lty=2,col=4)

plot(chemical.fit,res.sta,xlab="fitted values",ylab="standardized residuals")
abline(h=0,lty=2,col=4)

plot(chemical.fit,res.stu,xlab="fitted values",ylab="studentized residuals")
abline(h=0,lty=2,col=4)

plot(chemical.cook,ylab="Cook's distance",type="h")
k<-sort.list(chemical.cook,decreasing=T)[1:3]
points(k,chemical.cook[k],pch=16,col=2)

par(mfrow=c(1,1))



###### Variable selection: Do we need X2? ######
chemical.lm.x2<-lm(Y~(X1+X3)^2+I(X1^2)+I(X3^2),data=chemical)
anova(chemical.lm.x2,chemical.lm)



###### Variable selection: Do we need quadratic terms associated with X1? ######
tmp<-Y~(X1+X3)^2+I(X3^2)-X1:X3
chemical.lm.x2.x1<-lm(tmp,data=chemical)
anova(chemical.lm.x2.x1,chemical.lm.x2)



###### The final model: chemical ######
summary(chemical.lm.x2.x1)
anova(chemical.lm.x2.x1)



###### The response surface: chemical ######
n<-20
x1<-seq(min(chemical$X1),max(chemical$X1),length.out=n)
x3<-seq(min(chemical$X3),max(chemical$X3),length.out=n)
funtmp<-function(x,y) predict(chemical.lm.x2.x1,newdata=data.frame(X1=x,X3=y))
Y<-matrix(mapply(funtmp,rep(x1,each=n),x3),n,n)

############
#
# after some test, we can varify that the structure of Y is:
#
#           X1
#     ---------------
#     |             |
#     |             |
#     |             |
# X3  |      Y      |
#     |             |
#     |             |
#     |             |
#     ---------------
#
############


wireframe(Y)
cloud(Y,shade=T)

### for those who don't have the package lattice
persp(Y,theta=-45,phi=30)

### a beautiful version
print(wireframe(Y, shade = TRUE,par.box=list(col="green",lty=1),
                zlab="Y",xlab=expression(X[3]),ylab=expression(X[1]),
                drape=TRUE, distance=0.5, aspect = c(61/87, 0.4)))

### an interactive version
require(rgl)
persp3d(x1,x3,Y,col=3)



###### Dummy variable, regression with a categorical variable: turkey example ######
load("RA_Lecture.rda")


### The scatter plot: turkey
with(turkey,plot(Age,Weight,type="n"))
with(turkey,points(Age[Z1==1],Weight[Z1==1],pch=1,col=2))
with(turkey,points(Age[Z2==1],Weight[Z2==1],pch=3,col=3))
with(turkey,points(Age[(Z1+Z2)==0],Weight[(Z1+Z2)==0],pch=16,col=4))

legendword<-c("Georgia","Virginia","Wisconsin")
legend(28,11,legendword,pch=c(1,3,16),col=2:4)



### The residual plot fitted with one single line: turkey ###
turkey.lm.nodummy<-lm(Weight~Age,data=turkey)
dotchart(resid(turkey.lm.nodummy),groups=turkey$Origin)
abline(v=0,lty=2)



### The plot fitted with three lines: turkey ###
turkey.lm.G<-lm(Weight~Age,data=turkey,subset=(Origin=="Georgia"))
turkey.lm.V<-lm(Weight[Z2==1]~Age[Z2==1],data=turkey)
turkey.lm.W<-lm(Weight~Age,data=turkey[turkey$Origin=="Wisconsin",])

with(turkey,plot(Age,Weight,type="n"))
with(turkey,points(Age[Z1==1],Weight[Z1==1],pch=1,col=2))
with(turkey,points(Age[Z2==1],Weight[Z2==1],pch=3,col=3))
with(turkey,points(Age[(Z1+Z2)==0],Weight[(Z1+Z2)==0],pch=16,col=4))

legendword<-c("Georgia","Virginia","Wisconsin")
legend(28,11,legendword,pch=c(1,3,16),col=2:4,lty=1:3)

abline(turkey.lm.G,lty=1,col=2)
abline(turkey.lm.V,lty=2,col=3)
abline(turkey.lm.W,lty=3,col=4)



###### main effects and interactions ######
turkey.lm<-lm(Weight~Age+Z1+Z2+Age:Z1+Age:Z2,data=turkey)


###### Submodels and hypothesis testing ######
### Do we need the dummy variable? ###
anova(turkey.lm.nodummy,turkey.lm)

### Are the slopes the same? ###
turkey.lm.sameslope<-lm(Weight~Age+Z1+Z2,data=turkey)
anova(turkey.lm.sameslope,turkey.lm)

### Are the first two intercepts identical? ###
turkey.lm.intercept<-lm(Weight~Age+I(Z1+Z2),data=turkey)
anova(turkey.lm.intercept,turkey.lm.sameslope)

### Fitted results and submodel selection: turkey ###
summary(turkey.lm)

anova(turkey.lm.nodummy,turkey.lm)
anova(turkey.lm.sameslope,turkey.lm)
anova(turkey.lm.intercept,turkey.lm)



