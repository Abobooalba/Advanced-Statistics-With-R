## ----setup,include=FALSE-------------------------------------------------
#options(formatR.arrow=TRUE,tikzDefaultEngine="xetex")
opts_chunk$set(
dev = 'pdf',
#dev='tikz',
fig.align='center', 
size='footnotesize', 
#results='asis',
out.width='.85\\linewidth',
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
par(mar=c(4,4,1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
})

## ------------------------------------------------------------------------
library(quantmod) 
getSymbols('GOOG', from = "2005-01-01") 
# Data from SPY, the ETF tracks SP500
getSymbols('SPY', from = "2005-01-01")
x <- SPY$SPY.Close
y <- GOOG$GOOG.Close
n <- length(y) 
ret <- diff(log(y))
ret <- ret[-1]
SP500ret <- diff(log(x)) 
SP500ret <- SP500ret[-1]

## ----plotgoogprice,fig.height=4.5, echo=FALSE, echo=2, results='hide'----
Sys.setlocale("LC_TIME", "English")
plot(y,type="l",col=20,main="Google",xlab="Price",ylab="$")

## ----plotgoogmke,fig.height=5.5, echo=FALSE------------------------------
SP500ret <- as.vector(SP500ret) 
ret <- as.vector(ret)
plot(SP500ret, ret, xlab = "Google", ylab = "SP500", pch=20, col = 'light grey', bty='n') 
abline(lm(ret ~ SP500ret),col='red',lwd=2) 
title("Google Market Model")

## ------------------------------------------------------------------------
googlemkt<-lm(ret~SP500ret); summary(googlemkt)

## ------------------------------------------------------------------------
new <- data.frame(SP500ret = c(1, 1.2))  
predict.lm(googlemkt,new,interval="prediction") 
predict.lm(googlemkt,new,interval="confidence") 

## ------------------------------------------------------------------------
library("datasets")
states<-as.data.frame(state.x77[,c("Murder","Population",
                      "Illiteracy","Income","Frost")])
round(cor(states),3)

## ----statematrixplot,fig.height=5.5--------------------------------------
library(car); scatterplotMatrix(states)

## ------------------------------------------------------------------------
fit<-lm(Murder~.,data = states); summary(fit)

## ------------------------------------------------------------------------
anova(fit)

## ----results='asis'------------------------------------------------------
library("xtable")
xtable(summary(fit))

## ------------------------------------------------------------------------
fit2<-lm(Murder~Population+Illiteracy, data = states)
summary(fit2)

## ------------------------------------------------------------------------
fit<-lm(Murder~., data=states) 
step(fit,direction="backward")

## ------------------------------------------------------------------------
fit1 <- lm(Murder ~ . , data=states)  
fit2 <- lm(Murder~Population+Illiteracy,data=states)  
anova(fit2,fit1)

## ------------------------------------------------------------------------
AIC(fit2,fit1)
BIC(fit2,fit1)

## ------------------------------------------------------------------------
library(leaps)
fitsub<-regsubsets(Murder~ . , data=states) 
summary(fitsub)

## ----statessubsetleaps,fig.height=5.5------------------------------------
plot(fitsub,scale="bic")

