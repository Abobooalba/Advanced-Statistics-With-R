## ----setup,include=FALSE-------------------------------------------------
options(formatR.arrow=TRUE,tikzDefaultEngine="xetex")
opts_chunk$set(
dev = 'pdf',
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
par(mar=c(4,4,.01,.01),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
})

## ------------------------------------------------------------------------
prod(5:(5-3+1))
factorial(5)/factorial(5-3)

## ------------------------------------------------------------------------
choose(n=8,k=3)

## ------------------------------------------------------------------------
S<-c(1,2,3,4,5,6); A<-c(1,2); B<-c(1,2,3); C<-c(2,4,6) 
intersect(A,B) 
intersect(intersect(A,B),C) #works pairwise 
union(A,B) 
union(union(A,B),C) #works pairwise 
(complementofA<-setdiff(S,A)) #complement of A 
intersect(A,complementofA)

## ------------------------------------------------------------------------
m<-seq(10,50,5)
P.E<-function(m){
     c(Students=m,ProbAtL2SB=1-prod((365:(365-m+1)/365)))}
round(sapply(m,P.E),2)

## ------------------------------------------------------------------------
set.seed(2) # done for reproducibility 
actual <- sample(x = 1:3, size = 10000, replace = TRUE) 
aguess <- sample(x = 1:3, size = 10000, replace = TRUE)
equals <- (actual == aguess) 
PNoSwitch <- sum(equals)/10000 
not.eq <- (actual != aguess) 
PSwitch <- sum(not.eq)/10000 
Probs <- c(PNoSwitch, PSwitch)
names(Probs) <- c("P(Win no Switch)", "P(Win Switch)") 
Probs

## ------------------------------------------------------------------------
x<-c(0,1,2,3) 
f<-c(1/8,3/8,3/8,1/8)
mu<-sum(x*f)
sigma2<-sum((x-mu)^2*f)
sigma<-sqrt(sigma2)
mu;sigma2;sigma

## ------------------------------------------------------------------------
fx <- function(x) {
      3/4 - 3/4 * x^2 } # define function fx 
integrate(fx,lower=-0.5,upper=1) # gives value and tolerance

