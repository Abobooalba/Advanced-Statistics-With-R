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

## ----fig.height=4--------------------------------------------------------
curve(dchisq(x,df=3),from=0,to=20,ylab="y")
ind<-c(3,5,7,10,15)
for (i in ind){
curve(dchisq(x,df=i),0,20,col=i,lwd=2,add=TRUE)}
legend("topright",inset=.05,title="df",as.character(ind),fill=ind)

## ----student-t,echo=FALSE,fig.height=5-----------------------------------
x<-seq(-6,6,length=201)
plot(x,dnorm(x),xlab=expression(paste(italic(z),' or ',italic(t))),ylab='density',type='l', 
lwd = 2, col='red')
df <- c(1, 3, 12) 
for (i in df) 
lines(x, dt(x, i)) 
labels <- c('normal', expression(italic(t[1])), expression(italic(t[3])))
atx <- c(2, 0, 0)
aty <- c(0.35, 0.26, 0.34)
text(atx, aty, labels = labels)

