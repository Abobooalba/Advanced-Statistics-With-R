## ----setup,include=FALSE-------------------------------------------------
opts_chunk$set(
#dev = 'pdf',
fig.align='center', 
size='footnotesize', 
#results='asis',
out.width='.8\\linewidth',
fig.path='figure/', 	
fig.keep='high', 	
fig.show='asis', 
#cache=TRUE,	
#warning=FALSE,
prompt=TRUE
)

## ----margin,include=FALSE------------------------------------------------
knit_hooks$set(
 margin=function(before,options,envir){ 
  if(before) 
    par(mar=c(4,4,0.1,0.1)) 
  else NULL 
} 
)

## ------------------------------------------------------------------------
3^2+sqrt(4);factorial(5);log(10)
exp(2); pi; sin(pi/3)
print("Hello world")
date()

## ------------------------------------------------------------------------
x<-(1:12)
x
seq(12)
seq(4, 6, 0.25)

## ------------------------------------------------------------------------
rep(10, 3) 
rep(c(1:3), 3) 
rep(c(3.14, 2.71), 3) 

## ------------------------------------------------------------------------
dbinom(2, 5, 0.60)

## ------------------------------------------------------------------------
pnorm(1.96,0,1)-pnorm(-1.96,0,1)

## ------------------------------------------------------------------------
dpois(2,1) 

## ------------------------------------------------------------------------
sample(1:50,5);sample(1:50,5);sample(1:50,5,replace=T) 
set.seed(1234);sample(1:50,5)
set.seed(1234);sample(1:50,5)

## ----fig.height=3.5------------------------------------------------------
height<-rnorm(1000,mean=156,sd=4.6)
hist(height,main="") 

## ------------------------------------------------------------------------
x <- 1:5 
length(x)
sum(x)
x1 <- seq(1, 5, by = 1)
x2 <- seq(1, 5, length = 5) 
x1==x2
x <- letters[1:5] 
x == c("b")

## ------------------------------------------------------------------------
a<-2
A<-matrix(c(1,2,3,4),nrow=2,ncol=2) # Real matrix
A 
B<-matrix(c(1,2,2,7),nrow=2,ncol=2) # Symmetric real matrix.
B

## ------------------------------------------------------------------------
A%*%B

## ------------------------------------------------------------------------
crossprod(A,B) # t(A)%*%B

## ------------------------------------------------------------------------
A*B

## ------------------------------------------------------------------------
A/B

## ------------------------------------------------------------------------
solve(A)

## ------------------------------------------------------------------------
solve(A,B) #Identical to: solve(A)%*%B
solve(A)%*%B

## ------------------------------------------------------------------------
5>4;5>=4;5==4;5!=4
5 >= 4 & 4 <= 5 # AND 
5 >= 4 | 4 == 5 # OR 

## ------------------------------------------------------------------------
f <- function(x){exp(-x^2/2)/sqrt(2*pi)} 
f(1)
f(1) == dnorm(1) # built-in-function 
class(f)
integrate(f,-2,2)

## ----fig.height=4--------------------------------------------------------
f<-function(x){exp(-x^2/2)/sqrt(2*pi)}   
plot(f,xlim=c(-3,3),col="blue",lwd = 2)
abline(h=0)

## ------------------------------------------------------------------------
x<-0:9
x
mean(x);median(x);mode(x)
var(x);sd(x)

## ------------------------------------------------------------------------
x<-rnorm(20,1,2);x
summary(x)

## ----results='markup'----------------------------------------------------
x<-0:19; y<-x+rnorm(20,0,1);out<-lm(y~x)
summary(out)

## ----results='asis'------------------------------------------------------
library(xtable)
xtable(out)
xtable(anova(out))

## ----fig.width=5,fig.height=3.5,margin=TRUE------------------------------
plot(x,y) 
abline(lm(y~x),col='blue')

## ----fig.width=5,fig.height=3.1,margin=TRUE------------------------------
x<-seq(-3,3,length=101) 
y<-dnorm(x) # assign standard normal values to y 
plot(x,y,type='l')  # 'l' stands for line

## ----fig.width=6.1,fig.height=3.8,margin=TRUE----------------------------
x<-seq(-10,10,length=30)
y<-x
f<-function(x,y){
  r<-sqrt(x^2 + y^2)
  10*sin(r)/r }
z<-outer(x,y,f)
z[is.na(z)]<-1
persp(x,y,z,theta=30,phi=30,expand=0.5,col="lightblue")

## ----message=FALSE,fig.width=5.2,fig.height=3.1,margin=TRUE--------------
library(ggplot2)
qplot(wt, mpg, data=mtcars, color=factor(cyl), geom=c("point", "smooth"))

