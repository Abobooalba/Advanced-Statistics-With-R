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

## ----echo=FALSE,fig.height=6---------------------------------------------
library(ggplot2) 
mu <- seq(30, 50, 0.01) 
power9 <- 1 - pnorm(44, mu, 6/sqrt(9)) + pnorm(36, mu, 6/sqrt(9)) 
power36 <- 1 - pnorm(42, mu, 6/sqrt(36)) + pnorm(38, mu, 6/sqrt(36)) 
library(grid) 
DF <- data.frame(x = mu, P9 = power9, P36 = power36) 
p <- ggplot(data = DF, aes(x = mu, y = P9))
p + geom_line(color = "lightblue") + geom_line(aes(x = mu, y = P36), lty = "dashed", color = "blue") + theme_bw() +    
annotate("segment", x = 32, xend = 37, y = 0.35, yend = 0.78, arrow = arrow(length = unit(0.2, "cm"))) +   
annotate("segment", x = 32, xend = 34.2, y = 0.6, yend = 0.78, arrow = arrow(length = unit(0.2, "cm"))) + 
annotate("segment", x = 40, xend = 40, y = 0.65, yend = 0.06, arrow = arrow(length = unit(0.2, "cm"))) + 
annotate("text", parse=TRUE, x = 32, y = 0.33, label = "n == 36") +  
annotate("text", x = 32, y = 0.58, parse=TRUE, label = "n == 9") + 
annotate("text", x = 40, y = 0.7, parse=TRUE, label ="alpha == 0.045") +  
labs( x = expression(mu), y = expression("Power(" *mu*")")) 

## ----echo=FALSE,fig.height=5.5-------------------------------------------
curve(dnorm(x, 1, 1), -2, 5, axes = FALSE, ann = FALSE, n = 500) 
curve(dnorm(x, 2, 1), -2, 5, add = TRUE, n = 500) 
BETA <- pnorm(2.0364, 2, 1)
ALPHA <- pnorm(2.0364, 1, 1, lower = FALSE) 
x <- seq(-2, qnorm(BETA, 2, 1), length = 200) 
y <- dnorm(x, 2, 1) 
xs <- c(-2, x, qnorm(BETA, 2, 1)) 
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue3") 
x <- seq(qnorm(1 - ALPHA, 1 , 1), 5, length = 200)
y <- dnorm(x, 1, 1)
xs <- c(qnorm(1 - ALPHA, 1, 1), x, 5) 
ys <- c(0, y, 0)
polygon(xs, ys, col = "lightskyblue1")
# Retrace now 
curve(dnorm(x, 1, 1), -2, 5, add = TRUE, n = 500, lwd = 2, col = "red") 
curve(dnorm(x, 2, 1), -2, 5, add = TRUE, n = 500, lwd = 2) 
# Highlight x-axis 
segments(-2, 0, 5, 0, lwd = 3)
# 
segments(1, 0, 1, dnorm(1, 1, 1), lwd = 2) 
segments(qnorm(1 - ALPHA, 1 ,1), 0, qnorm(1 - ALPHA, 1 ,1), dnorm(qnorm(1 - ALPHA, 1 ,1), 2, 1), lwd = 2)
segments(2, 0, 2, dnorm(2, 2, 1), lwd = 2) 
### 
arrows(-1.5, .40, 0, .06, length = .1)
mtext(expression("P(type II error)" == 0.5145), side = 3, line = 0, at = - 1) 
arrows(4.5, .40, 3, .06, length = .1)
mtext(expression("P(type I error)" == 0.1500), side = 3, line = 0, at = 4) 
### labels now 
axis(side = 1, at = c(-2, -1, 0, 1, 2.0364, 3, 4, 5), line = -0.4)

## ------------------------------------------------------------------------
xbar=57 
mu0=60 
s=10 
n=26 
alpha <- 0.001
se=s/sqrt(n) 
tobs=(xbar-mu0)/se  # t statistic
ct <- qt(alpha, df = n-1) # critical value
pvalue<-pt(tobs,n-1) #p-value,one-sided
c(ct = ct, tobs = tobs, pvalue = pvalue)

## ------------------------------------------------------------------------
prop.test(90,900,0.2,alternative ="less")

## ------------------------------------------------------------------------
library("PASWR2")
cv <- qchisq(0.95,19) # Critical Value
s2 <- var(WASHER$diameter)
n <- sum(!is.na(WASHER$diameter))
Chi2Obs <- (n - 1)*s2 / 0.004 #Standardized Test Statistic's Value 
pvalue <- pchisq(Chi2Obs, n-1, lower = FALSE)     
 c(n = n, CriticalValue = cv, Chi2Obs = Chi2Obs, pvalue = pvalue) 

## ------------------------------------------------------------------------
library("quantmod")
AAPL <- getSymbols("AAPL", from = "2016-01-01", to = Sys.Date(), 
                   auto.assign = FALSE)
BUFF <- getSymbols("BRK-A", from = "2016-01-01", to = Sys.Date(), 
                   auto.assign = FALSE)
head(AAPL)

## ----chartAPPL,fig.height=5,echo=FALSE,echo=2,results='hide'-------------
Sys.setlocale("LC_TIME", "English")
chartSeries(AAPL,theme=chartTheme('white'),TA="addVo();addBBands()")

## ----chartBUFF,fig.height=5,echo=FALSE,echo=2,results='hide'-------------
Sys.setlocale("LC_TIME", "English")
chartSeries(BUFF,theme=chartTheme('white'),TA="addVo();addBBands()")

## ------------------------------------------------------------------------
aapl <- diff(log(AAPL$AAPL.Adjusted)) # log returns 
aapl <- aapl[-1] # delete the first NA 
buff <- diff(log(BUFF$'BRK-A.Adjusted')) 
buff <- buff[-1] 
aapl.ret <- as.vector(aapl) # Why? aapl is a xts
buff.ret = as.vector(buff) 
# moments of returns 
library(moments)
mean(aapl.ret); sd(aapl) 
skewness(aapl.ret); kurtosis(aapl.ret)

## ----histAPPL,fig.height=4-----------------------------------------------
hist(aapl.ret,breaks=50,main = "", xlab="daily return",
     col='lightblue', freq = FALSE) 
y<-seq(-0.08,0.08,length=200) 
lines(y,dnorm(y,mean(aapl.ret),sd(aapl.ret)),type='l',col='red',lwd=2)

## ----boxAP-BU,fig.height=5-----------------------------------------------
boxplot(aapl.ret, buff.ret, names=c("AAPL","Buffet")) 

## ------------------------------------------------------------------------
 t.test(aapl.ret, buff.ret, var.equal = F)

## ------------------------------------------------------------------------
prop.test(x=c(500,505),n=c(44425,44405),alternative="two.sided")

## ------------------------------------------------------------------------
var.test(aapl.ret, buff.ret) 

## ----QQplotSchool,fig.height=4-------------------------------------------
ggplot(data = STSCHOOL, aes(sample = satisfaction, shape = school, 
              color = school)) +    theme_bw() + stat_qq()   

## ----boxSchool,fig.height=4----------------------------------------------
ggplot(data=STSCHOOL,aes(x=school,y=satisfaction,fill=school)) +   
geom_boxplot() + theme_bw()  

## ------------------------------------------------------------------------
var.test(STSCHOOL$x, STSCHOOL$y) 

## ------------------------------------------------------------------------
 t.test(STSCHOOL$x, STSCHOOL$y, var.equal = T)

