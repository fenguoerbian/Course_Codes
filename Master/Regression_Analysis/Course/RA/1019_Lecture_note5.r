load("RA_Lecture.rda")


###### Various models: turkey ######
### full model ###
turkey.lm1<-lm(Weight~Age*(Z1+Z2),data=turkey)

### common slope ###
turkey.lm2<-lm(Weight~Age+Z1+Z2,data=turkey)

### common slope and common intercept for Georgia and Virginia ###
turkey.lm3<-lm(Weight~Age+I(Z1+Z2),data=turkey)

### no dummy variable ###
turkey.lm4<-lm(Weight~Age,data=turkey)

### prepare the data for ploting the figures when using predict ###
new.x<-rep(seq(18,34,length.out=10),3)
plot.x<-data.frame(Age=new.x,Z1=c(rep(1,10),rep(0,20)),Z2=c(rep(0,10),rep(1,10),rep(0,10)))

### plot the 4 figures ###
# full model #
with(turkey,plot(Age,Weight,type="n"))
with(turkey,points(Age[Z1==1],Weight[Z1==1],pch=1,col=2))
with(turkey,points(Age[Z2==1],Weight[Z2==1],pch=3,col=3))
with(turkey,points(Age[(Z1+Z2)==0],Weight[(Z1+Z2)==0],pch=16,col=4))
legendword<-c("Georgia","Virginia","Wisconsin")
legend(28,11,legendword,pch=c(1,3,16),col=2:4,lty=1:3,lwd=1.5)
# using abline to draw lines
CV<-coef(turkey.lm1)
abline(a=CV[1]+CV[3],b=CV[2]+CV[5],lty=1,col=2,lwd=1.5)
abline(a=CV[1]+CV[4],b=CV[2]+CV[6],lty=2,col=3,lwd=1.5)
abline(a=CV[1],b=CV[2],lty=3,col=4,lwd=1.5)
# using predict to draw lines, same result, but simpler than abline
plot.y<-predict(turkey.lm1,plot.x)
k<-1:3
sapply(k,FUN=function(k) lines(new.x[1:10+(k-1)*10],plot.y[1:10+(k-1)*10],lty=k,col=k+1,lwd=1.5))


# common slope #
with(turkey,plot(Age,Weight,type="n"))
with(turkey,points(Age[Z1==1],Weight[Z1==1],pch=1,col=2))
with(turkey,points(Age[Z2==1],Weight[Z2==1],pch=3,col=3))
with(turkey,points(Age[(Z1+Z2)==0],Weight[(Z1+Z2)==0],pch=16,col=4))
legendword<-c("Georgia","Virginia","Wisconsin")
legend(28,11,legendword,pch=c(1,3,16),col=2:4,lty=1:3,lwd=1.5)
# using abline to draw lines
CV<-coef(turkey.lm2)
abline(a=CV[1]+CV[3],b=CV[2],lty=1,col=2,lwd=1.5)
abline(a=CV[1]+CV[4],b=CV[2],lty=2,col=3,lwd=1.5)
abline(a=CV[1],b=CV[2],lty=3,col=4,lwd=1.5)
# using predict to draw lines, same result, but simpler than abline
plot.y<-predict(turkey.lm2,plot.x)
k<-1:3
sapply(k,FUN=function(k) lines(new.x[1:10+(k-1)*10],plot.y[1:10+(k-1)*10],lty=k,col=k+1,lwd=1.5))


# common slope and common intercept for Georgia and Virginia #
with(turkey,plot(Age,Weight,type="n"))
with(turkey,points(Age[(Z1+Z2)==1],Weight[(Z1+Z2)==1],pch=1,col=2))
with(turkey,points(Age[(Z1+Z2)==0],Weight[(Z1+Z2)==0],pch=16,col=4))
legendword<-c("Georgia and Virginia","Wisconsin")
legend(28,11,legendword,pch=c(1,16),col=c(2,4),lty=c(1,3),lwd=1.5)
# using abline to draw lines 
CV<-coef(turkey.lm3)
abline(a=CV[1]+CV[3],b=CV[2],lty=1,col=2,lwd=1.5)
abline(a=CV[1],b=CV[2],lty=3,col=4,lwd=1.5)
# using predict to draw lines, same result, but simpler than abline
plot.y<-predict(turkey.lm3,plot.x)
k<-c(1,3)
sapply(k,FUN=function(k) lines(new.x[1:10+(k-1)*10],plot.y[1:10+(k-1)*10],lty=k,col=k+1,lwd=1.5))

# no dummy variable #
with(turkey,plot(Age,Weight,type="n"))
with(turkey,points(Age[Z1==1],Weight[Z1==1],pch=1,col=2))
with(turkey,points(Age[Z2==1],Weight[Z2==1],pch=3,col=3))
with(turkey,points(Age[(Z1+Z2)==0],Weight[(Z1+Z2)==0],pch=16,col=4))
legendword<-c("Georgia","Virginia","Wisconsin")
legend(28,11,legendword,pch=c(1,3,16),col=2:4)
abline(turkey.lm4,lwd=1.5)





###### Broken-line regression: saving example ######
str(savings)
savings$z<-savings$pop15>35 # bool varibale would be fine
savings.lm<-lm(sr~pop15+I(z*(pop15-35)),data=savings)
savings.lm1<-lm(sr~pop15,data=savings,subset=(pop15<35))
savings.lm2<-lm(sr~pop15,data=savings[savings$pop15>35,])
CV<-coef(savings.lm)
CV1<-coef(savings.lm1)
CV2<-coef(savings.lm2)

with(savings,plot(pop15,sr,xlab="pop15",ylab="rate"))
rect(20,-1,35,30,col="lightgrey")
rect(35,-1,50,30,col="moccasin")
with(savings,points(pop15,sr,pch=16,col="violet"))
abline(v=35,lwd=2)
# seperate lines
segments(0,CV1[1],35,CV1[1]+35*CV1[2],lty=2,lwd=2,col=2)
segments(35,CV2[1]+35*CV2[2],50,CV2[1]+50*CV2[2],lty=2,lwd=2,col=2)
# one continuous line
new.x<-seq(0,50,length.out=100)
plot.y<-predict(savings.lm,data.frame(pop15=new.x,z=new.x>35))
lines(new.x,plot.y,lty=1,lwd=2,col=4)



