load("DOE_Lecture.rda")



###### Randomized complete block design ######
# coupon is the block #
# tip is the factor of interest #
str(hardness)
fit<-aov(reading~tip+coupon,data=hardness)
model.tables(fit,se=T)
( X<-model.matrix(~tip+coupon,data=hardness,contrasts=list(tip=contr.sum,coupon=contr.sum)) )
crossprod(X)
summary(fit)
tukey.tip<-TukeyHSD(fit,which="tip")
plot(tukey.tip)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))




###### paired T-test, using aov ######
rm(list=ls())
load("ch2dat.rda")
read<-c(hardness$tip1,hardness$tip2)
tip<-c(rep(1,10),rep(2,10))
hardness2<-data.frame(read=read,tip=factor(tip))
hardness2$block<-factor(rep(1:10,2))
str(hardness2)
fit2<-aov(read~tip+block,data=hardness2)
model.tables(fit2,se=T)
summary(fit2)
t.test(hardness$tip1,hardness$tip2,paired = T)


###### latinsquare design ######
latinsquare <- function(n){
  starttime <- Sys.time()
  require(combinat)
  perms <- permn(n)
  perms <- sample(perms)
  # combinations package
  # build all permuations
  # randomize the order
  # start building the table row by row
  SET = 1:n
  SQUARE = NULL
  SQUARE = rbind(rep(NA,n),sample(SET))
  TRY = 1
  for(i in 1:(length(SET)-1)){
    test = FALSE
    while(test == FALSE){
      X <- perms[[TRY]]
      errors = NULL
      for(i in 1:n) errors <- c(errors,X[i] %in% SQUARE[,i])
      TRY = TRY + 1
      if(sum(errors) != 0) test = FALSE else test <- TRUE
    }
    SQUARE <- rbind(SQUARE,X)
  }
  SQUARE <- SQUARE[-1,]
  row.names(SQUARE) <- 1:n
  cat("Time elapsed",difftime(Sys.time(),starttime,units="secs"),"seconds\n")
  return(data.frame(SQUARE))
}




###### latinsuqare design ######
load("DOE_Lecture.rda")
str(propellant)
fit<-aov(rate~row+col+trt,data=propellant)
summary(fit)
model.tables(fit,se=T)
tukey.trt<-TukeyHSD(fit,which="trt")
plot(tukey.trt)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))

