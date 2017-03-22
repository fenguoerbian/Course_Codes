# This program is for the 2d kernel density estimate for the "faithful" data

require(rgl)
require(MASS)
M<-kde2d(faithful$waiting,faithful$eruptions,n=100)
contour(M,xlab="waiting",ylab="eruptions")
persp3d(M$x,M$y,M$z,col="green",xlab="waiting",ylab="eruptions",zlab="density")
