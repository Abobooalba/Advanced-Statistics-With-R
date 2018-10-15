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
#cache=TRUE,	
warning=FALSE,
message=FALSE,
par=TRUE,
prompt=TRUE
)
knit_hooks$set(par=function(before, options, envir){
if (before && options$fig.show!='none')
par(mar=c(4,4,2,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
})

## ----Anscombe_data-------------------------------------------------------
ansc<-read.csv("R/anscombe.csv", header=TRUE); ansc

## ----set1_lm-------------------------------------------------------------
fit1<-lm(y1~x1, data=ansc)
coef(fit1)
anova(fit1)

## ----fit2_lm-------------------------------------------------------------
fit2<-lm(y2~x2,data=ansc)
coef(fit2)
anova(fit2)

## ----lm_fit3-------------------------------------------------------------
fit3<-lm(y3~x3, data=ansc)
coef(fit3)
anova(fit3)

## ----lm_fit4-------------------------------------------------------------
fit4<-lm(y4~x4, data=ansc)
coef(fit4)
anova(fit4)

## ----ScatterPlotAnscombe, echo=FALSE, fig.height=6-----------------------
attach(ansc)
par(mfrow=c(2,2))
plot1<-plot(x1,y1,main='Set 1',col=34,pch=20)
abline(fit1)
plot2<-plot(x2,y2,main='Set 2',col=24,pch=20)
abline(fit2)
plot3<-plot(x3,y3,main='Set 3',col=20,pch=20)
abline(fit3)
plot4<-plot(x4,y4,main='Set 4',col=3,pch=20)
abline(fit4)
detach(ansc)

## ----plot_fit1, fig.height=5---------------------------------------------
par(mfrow=c(2,2)); plot(fit1)

## ----plot_fit2, fig.height=5---------------------------------------------
par(mfrow=c(2,2)); plot(fit2)

## ----plot-fit3, fig.height=5---------------------------------------------
par(mfrow=c(2,2)); plot(fit3)

## ----plot_fit4, fig.height=5---------------------------------------------
par(mfrow=c(2,2)); plot(fit4)

## ----smokedata-----------------------------------------------------------
smoke<-read.table("R/smoking.txt",header=TRUE)
colnames(smoke)<-c("Country", "Cancer","Consumption")
attach(smoke); smoke

## ------------------------------------------------------------------------
cor(cbind(Cancer,Consumption))

## ----smokingplot,echo=FALSE, fig.height=5--------------------------------
plot(Consumption,Cancer,pch=20,col=34,main="Cancer vs Smoking consumption") 
fitsmoke<-lm(Cancer~Consumption) 
abline(fitsmoke,col=2,lty=2,lwd=2) 
text(1145,465,"UK",pos=4) 
text(1280,190,"USA",pos=3) 
text(1115,350,"Finland",pos=4)
fitsmoke2 = lm(Cancer[-7]~Consumption[-7]) 
abline(fitsmoke2,col=3,lty=3,lwd=3)
legend("topleft", c("Full sample","Removed U.S."), col=2:3, lty=2:3, lwd=2:3, bty="n") 

## ----smoke4in1, fig.height=5---------------------------------------------
par(mfrow=c(2,2)); plot(fitsmoke)

## ----smokecoef-----------------------------------------------------------
coef(fitsmoke) 
coef(fitsmoke2)
summary(fitsmoke)$adj.r.squared
summary(fitsmoke2)$adj.r.squared

## ----smokeqqPlot,echo=2, fig.height=5------------------------------------
library(car)
qqPlot(fitsmoke,simulate=TRUE,main="Q-Q Plot")

## ----smokecrPlot,fig.height=4,echo=TRUE----------------------------------
crPlots(fitsmoke)

## ----spreadLevelPlot,results='markup',fig.height=4,echo=TRUE-------------
spreadLevelPlot(fitsmoke,main="")
detach(smoke)

## ----simulationncv,echo=1:4,results='markup',tidy=TRUE-------------------
x<-1:50 
y=1+2*x+rnorm(x)*(1+x/2)
fm<-lm(y~x) 
summary(fm) 
rdata<-data.frame(x,y) 
attach(rdata)

## ----simulscatter,echo=TRUE,results='markup',tidy=TRUE,fig.height=4.5----
plot(x,y,data=rdata) 
abline(1,2,lty=3) 
abline(coef(fm),col="blue") 
segments(x,fitted(fm),x,y,lty=4,col="red")

## ----simul4in1,fig.height=5----------------------------------------------
par(mfrow=c(2,2)); plot(fm) 

## ----stateshomotest,results='markup',tidy=TRUE,digits=4,echo=TRUE--------
ncvTest(fm) 

## ----statespreadLevelPlot,fig.height=4-----------------------------------
spreadLevelPlot(fm,main="")
detach(rdata)

## ----hatPlot, echo=FALSE,,fig.height=5-----------------------------------
#############
## hat.plot 
#############
hat.plot<-function(fit){ 
p<-length(coefficients(fit)) 
n<-length(fitted(fit))
plot(hatvalues(fit),main="Index Plot of Hat Values") 
abline(h=c(2,3)*p/n,col="red",lty=2) 
#identify(1:n,hatvalues(fit),names(hatvalues(fit))) 
}
hat.plot(fitsmoke)
text(7,0.4,"USA",pos=4) 

## ----influencePlot,results='markup',fig.height=4-------------------------
influencePlot(fitsmoke,main="Influence Plot",sub="Circle size is proportional to Cooks distance")
detach(smoke)

## ----mammals-------------------------------------------------------------
mam<-read.table("R/mammals.txt",header=TRUE)
head(mam)

## ----mamalresid,fig.height=4---------------------------------------------
attach(mam)
par(mfrow = c(1, 2)); fitmam<-lm(Body~Brain)
plot(Brain,Body,main="Fitted Model")
abline(fitmam,col="red"); residuals<-rstudent(fitmam)
plot(Brain,residuals, main="Residuals Model",col="red")

## ----mamal4in1,fig.height=5----------------------------------------------
par( mfrow=c( 2, 2) ); plot(fitmam)

## ------------------------------------------------------------------------
summary(powerTransform(Body)) 

## ----hisBody, fig.height=4-----------------------------------------------
par(mfrow = c(1, 2))
hist(Body); hist(log(Body))

## ------------------------------------------------------------------------
boxTidwell(Body ~ Brain) 

## ----mamalln,echo=FALSE,fig.height=5-------------------------------------
par(mfrow = c(1, 2)) 
logbody<-log(Body); logbrain<-log(Brain)
fitmamln<-lm(logbody~logbrain)
plot(logbrain,logbody,xlab="log(Body)", ylab="log(Brain)", main="Fitted Model")
abline(fitmamln,col="red"); residln<-rstudent(fitmamln)
plot(logbrain,residln, xlab="log(Body)", ylab="Residuals", main="Residuals Model",col="red")

## ----mamallogplot,fig.height=4-------------------------------------------
par(mfrow=c(2,2))
fitmamln<-lm(logbody~logbrain)
plot(fitmamln)

## ----mamallogsum---------------------------------------------------------
summary(fitmamln)

## ------------------------------------------------------------------------
rstudent(fitmamln)[rstudent(fitmamln)>2]

## ----maminflu,results='hide', fig.height=4-------------------------------
influencePlot(fitmamln,main="Influence Plot", 
sub="Circle size is proportional to Cooks distance")
text(0.038,3,"Man",pos=4)
detach(mam)

