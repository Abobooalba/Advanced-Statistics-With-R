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
par(mar=c(4,4,.8,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
})

## ----stnorm,echo=FALSE,fig.height=4--------------------------------------
curve(dnorm(x, 0, 1), -3.5, 3.5, axes = FALSE, ann = FALSE, n = 500) 
alpha <- 0.05 
x <- seq(-3.5, qnorm(alpha/2), length = 100) 
y <- dnorm(x, 0, 1) 
xs <- c(-3.5, x, qnorm(alpha/2)) 
ys <- c(0, y, 0) 
polygon(xs, ys, col = "skyblue3") 
x <- seq(qnorm(1 - alpha/2), 3.5, length = 100)
y <- dnorm(x, 0, 1) 
xs <- c(qnorm(1 - alpha/2), x, 3.5) 
ys <- c(0, y, 0) 
polygon(xs, ys, col = "skyblue3")
segments(-3.5, 0, 3.5, 0, lwd = 3) 
curve(dnorm(x, 0, 1), -3.5, 3.5, axes = FALSE, ann = FALSE, add = TRUE, lwd = 2) 
segments(qnorm(alpha/2), 0, qnorm(alpha/2), dnorm(qnorm(alpha/2)), lwd = 2)
segments(qnorm(1 - alpha/2), 0, qnorm(1 - alpha/2), dnorm(qnorm(1 - alpha/2)), lwd = 2) 
arrows(-3, 0.08, -2.5, 0.02, length = .06) 
arrows(3, 0.08, 2.5, 0.02, length = .06) 
text(-3, 0.1, expression(alpha/2)) 
text(3, 0.1, expression(alpha/2)) 
text(0, 0.17, expression(1 - alpha)) 
mtext(expression(-z[1 - alpha/2] == z[alpha/2]), side = 1, line = 0, at = qnorm(alpha/2)) 
mtext(expression(z[1 - alpha/2]), side = 1, line = 0, at = qnorm(1 - alpha/2))  

## ----QQ_GROCERY, fig.height=4--------------------------------------------
library("PASWR2")
with(data = GROCERY, qqnorm(amount))  
with(data = GROCERY, qqline(amount))

## ------------------------------------------------------------------------
xbar <- mean(GROCERY$amount); z <- qnorm(0.985) 
CI1 <- xbar + c(-1, 1) * z * sqrt(900)/sqrt(30); CI1 
CI2 <-  z.test(GROCERY$amount, sigma.x = sqrt(900), n.x = 30,
              conf.level = .97)$conf
CI2  

## ------------------------------------------------------------------------
z.test(GROCERY$amount, sigma.x = sqrt(900), n.x = 30, 
              conf.level = .99, alternative = "greater")$conf 

## ------------------------------------------------------------------------
ceo<-read.csv("R/ceo.txt") 
mean(ceo$Total)
t.test(ceo$Total)$conf

## ------------------------------------------------------------------------
attach(ceo)
Salary_ratio<-Salary/Total; t.test(Salary_ratio)$conf.int
Bonus_ratio<-Bonus/Total; t.test(Bonus_ratio)$conf.int
Stock_ratio<-Stock/Total; t.test(Stock_ratio)$conf.int
detach(ceo)

## ------------------------------------------------------------------------
str(CALCULUS)
MEANS <- tapply(CALCULUS$score, CALCULUS$calculus, mean) 
MEANS 
pe <- MEANS[2] - MEANS[1] 
z <- qnorm(0.975) 
pe + c(-1, 1)*z*sqrt(25/18 + 144/18) 

## ------------------------------------------------------------------------
ScoreYesCalc <- subset(CALCULUS, select = score, 
                subset = calculus == "Yes", drop = TRUE) 
ScoreNoCalc <- subset(CALCULUS, select = score, 
                subset = calculus == "No", drop = TRUE) 
CI <- z.test(x = ScoreYesCalc, y = ScoreNoCalc, sigma.x = sqrt(25), 
                sigma.y = sqrt(144), conf.level = 0.95)$conf
CI

## ------------------------------------------------------------------------
t.test(score ~ calculus,data=CALCULUS,var.equal=TRUE)$conf 
ScoreYesCalc <- subset(CALCULUS, select = score, 
                       subset = calculus == "Yes", drop = TRUE)  
ScoreNoCalc <- subset(CALCULUS, select = score, 
                       subset = calculus == "No", drop = TRUE) 
t.test(x = ScoreYesCalc, y = ScoreNoCalc, var.equal=TRUE)$conf

## ------------------------------------------------------------------------
CALCULUS$calculus<-factor(CALCULUS$calculus,levels=c("Yes","No"))
t.test(score ~ calculus,data=CALCULUS)$conf 
t.test(score ~ calculus,data=CALCULUS) # Two sample t-test

## ------------------------------------------------------------------------
ScoreYesCalc <- subset(CALCULUS, select = score, 
                      subset = calculus == "Yes", drop = TRUE)  
n1 <- length(ScoreYesCalc); var1 <- var(ScoreYesCalc) 
L1 <- round((n1-1)*var1/qchisq(0.975, n1),2) 
U1 <- round((n1-1)*var1/qchisq(0.025, n1),2)
paste("CI for variance of YesCalc: (",L1,",",U1,")")
ScoreNoCalc <- subset(CALCULUS, select = score, 
                     subset = calculus == "No", drop = TRUE) 
n2 <- length(ScoreNoCalc); var2 <- var(ScoreNoCalc) 
L2 <- round((n2-1)*var2/qchisq(0.975, n2),2) 
U2 <- round((n2-1)*var2/qchisq(0.025, n2),2) 
paste("CI for variance of NoCalc: (",L2,",",U2,")")

## ------------------------------------------------------------------------
VR <- var1/var2 
CI <- c(qf(0.025, n1-1, n2-1) * VR, qf(0.975, n1-1, n2-1) * VR) 
CI

## ------------------------------------------------------------------------
prop.test(45,75)$conf

## ------------------------------------------------------------------------
prop.test(x=c(11,77),n=c(1500000,6000000),alternative="greater",
conf.level=.95)$conf

## ------------------------------------------------------------------------
n <- sum(!is.na(SOCCER$game)) # number of games 
xbar <- mean(SOCCER$goals, na.rm = TRUE)
z <- qnorm(0.95) # z_{0.95} 
CI <- xbar + c(-1, 1) * z * sqrt(xbar/n) 
CI

