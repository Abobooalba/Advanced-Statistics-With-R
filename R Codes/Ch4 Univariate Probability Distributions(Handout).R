## ----setup,include=FALSE-------------------------------------------------
#options(formatR.arrow=TRUE,tikzDefaultEngine="xetex")
opts_chunk$set(
#dev = 'pdf',
#dev='tikz',
fig.align='center', 
size='footnotesize', 
#results='asis',
out.width='.8\\linewidth',
fig.path='figure/', 	
fig.keep='high', 	
#fig.show='asis', 
cache=TRUE,	
warning=FALSE,
message=FALSE,
par=TRUE,
prompt=TRUE
)
knit_hooks$set(par=function(before, options, envir){
if (before && options$fig.show!='none')
par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
})

## ----Binomial,echo=FALSE,fig.height=6------------------------------------
P<-c(0.05, 0.25, 0.5, 0.75)  
x <- 0 : 20 
par(mfrow = c(2, 2)) 
for(i in 1 : 4){ 
density <- dbinom(x,size=20,prob=P[i]) 
xlab <- bquote(italic(p) == .(P[i])) 
plot(x, density, type = 'h', lwd = 2, xlab = xlab, ylab = "Probability") 
}

## ----Binom2Norm,echo=FALSE,fig.height=6----------------------------------
size<-c(10,50,100,300) 
p<-0.20 
N=100000 
par(mfrow = c(2, 2)) 
for(i in 1:4){   
n<-size[i]   
y<-rbinom(N,n,p)   
maxy<-max(y)   
miny<-min(y)   
xlab<-bquote(italic(n) == .(size[i]))   
hist(y,freq=FALSE,main="",breaks=maxy-miny,xlim=c(miny,maxy),xlab=xlab,col="lightblue")   
curve(dnorm(x, mean=mean(y), sd=sd(y)), add=TRUE, col="darkblue", lwd=3)   
}

## ----Poisson1,fig.height=4-----------------------------------------------
N<-10000; lambda<-5; x<-rpois(N,lambda)   
hist(x, probability=T, col='lightblue',main="")

## ----Poisson4,fig.height=4-----------------------------------------------
opar<-par(no.readonly=TRUE)      
lambda<-c(4,7,10,13);x<-0:20;par(mfrow=c(2,2))    
for(i in 1 : 4){d<-dpois(x,lambda[i])     
                xlab <- bquote(lambda == .(lambda[i]))     
plot(x,d,type='h',lwd=2,xlab=xlab,ylab="Probability")
points(x,d)};par(opar)

## ------------------------------------------------------------------------
library("PASWR2")
head(SOCCER)
xtabs(~goals, data = SOCCER)

## ------------------------------------------------------------------------
LAG<-sapply(1:5,function(x){
    SOCCER$goals[x:(x + 227)]})
round(cor(LAG), 3)

## ------------------------------------------------------------------------
OBS = xtabs(~goals, data = SOCCER)
Empir = round(OBS/sum(OBS), 3) 
TheoP=round(dpois(0:(length(OBS)-1),mean(SOCCER$goals,na.rm=TRUE)),3)
EXP = round(TheoP * 232, 0)
ANS = cbind(OBS, EXP, Empir, TheoP)
ANS

## ----fig.height=4.5------------------------------------------------------
u<-runif(1000);x<-u^(1/3)
hist(x,prob=TRUE,main="")
curve(3*x^2,from=0,to=1,add=TRUE,lwd=3,col="red")
text(0.5,2,expression({X}*"~"*{f(x)==3*x^2}))

## ----Normal,echo=FALSE,fig.height=6--------------------------------------
x<-seq(-3,5,length=501) 
plot(x,dnorm(x),type='l',ylim=c(0,1),xlab='x',ylab='density',col="blue",lwd=2) 
lines(x,dnorm(x,2,.5),,col="red",lwd=2) 
text(0,.44,expression(paste(italic(mu[X])==0,',',italic(sigma[X])== 1))) 
text(2,.84,expression(paste(italic(mu[Y])==2,',',italic(sigma[Y])== 0.5)))

## ------------------------------------------------------------------------
inter.times<-with(data=SOCCER,cgt[2:575]-cgt[1:574]) 
MEAN <- mean(inter.times) 
SD <- sd(inter.times) 
c(MEAN = MEAN, SD = SD)

## ----echo=FALSE----------------------------------------------------------
rate <- (575/232)*(1/90) # rate = lambda*t 
nit <- sum(!is.na(inter.times)) # number of inter.times 
OBS <- xtabs(~cut(inter.times, breaks = c(seq(0, 130, 10), 310))) 
EmpiP <- round(OBS/nit, 3) 
TheoP <- round(c((pexp(seq(10, 130, 10),rate) - 
          pexp(seq(0, 120, 10), rate)),(1-pexp(130,rate))),3) 
EXP<-round(TheoP*nit,0)
ANS<-cbind(OBS,EXP,EmpiP,TheoP)
ANS

## ----hist_and_exp,fig.height=4-------------------------------------------
hist(inter.times,breaks=seq(0,310,10),col="pink",xlim=c(0,140),
     prob=TRUE,main="")
xt<-seq(0,140,0.01);ft<-dexp(xt,rate);lines(xt,ft,type="l")

## ----Cauchy_and_Normal,fig.height=4.5------------------------------------
x<-seq(-6,6,length=501) 
plot(x,dnorm(x),ylab='density',type='l',lwd=2,col='red') 
lines(x,dcauchy(x),lwd=2,col='blue'); abline(h=0) 
legend("topright",legend=c('Normal','Cauchy'),
       col=c('red','blue'),lty=1,lwd=2)

