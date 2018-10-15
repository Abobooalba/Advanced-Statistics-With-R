## ----setup,include=FALSE-------------------------------------------------
#options(formatR.arrow=TRUE,tikzDefaultEngine="xetex")
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
par(mar=c(4,4,.02,.02),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)
})

## ------------------------------------------------------------------------
f1 <- function(x, y, p = 0){  
exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2))} 
f2 <- function(x, y, p = 0.4){ 
exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2))} 
f3 <- function(x, y, p = 0.8){  
exp( (x^2 - 2*p*x*y + y^2) / (-2*(1 - p^2)) ) / (2*pi*sqrt(1 - p^2))} 

## ----plotN2_persp,fig.height=3-------------------------------------------
opar <- par(no.readonly = TRUE) # copy of current settings 
par(mfrow = c(1, 3), mar = c(1.1, 1.1, 1.1, 1.1), pty = "s") 
x <- seq(-3, 3, length = 40); y <- x 
persp(x, y, outer(x, y, f1), zlab = "z", main = expression(rho == 0),     
       theta = -25, expand = 0.65, phi = 25, shade = 0.4)
persp(x, y, outer(x, y, f2), zlab = "z", main = expression(rho == 0.4),      
    theta = -25, expand = 0.65, phi = 25, shade = 0.4) 
persp(x, y, outer(x, y, f3), zlab = "z", main = expression(rho == 0.8),     
    theta = -25, expand = 0.65, phi = 25, shade = 0.4) 
par(opar)

## ----plotN2_contour,fig.height=3-----------------------------------------
opar <- par(no.readonly = TRUE) # copy of current settings
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 4.1, 1.1), pty = "s")
x <- seq(-3, 3, length = 50); y <- x 
contour(x, y, outer(x, y, f1), nlevels = 10, xlab = "x", ylab = "y",       
        main = expression(rho == 0)) 
contour(x, y, outer(x, y, f2), nlevels = 10, xlab = "x", ylab = "y",        
        main = expression(rho == 0.4)) 
contour(x, y, outer(x, y, f3), nlevels = 10, xlab = "x", ylab = "y",        
        main = expression(rho == 0.8)) 
par(opar)

## ----plotN2_image,fig.height=3-------------------------------------------
opar <- par(no.readonly = TRUE) # copy of current settings 
par(mfrow = c(1, 3), mar = c(4.1, 4.1, 4.1, 1.1), pty = "s") 
x <- seq(-3, 3, length = 50); y <- x 
image(x, y, outer(x, y, f1), col  = gray((0:100)/100), xlab = "x",     
      ylab = "y", main = expression(rho == 0))
image(x, y, outer(x, y, f2), col  = gray((0:100)/100), xlab = "x",      
     ylab = "y", main = expression(rho == 0.4)) 
image(x, y, outer(x, y, f3), col  = gray((0:100)/100), xlab = "x",      
     ylab = "y", main = expression(rho == 0.8)) 
par(opar)

## ------------------------------------------------------------------------
x <- seq(-3, 3, length = 40) 
y <- x 
z1 <- outer(x, y, f1) 
z2 <- outer(x, y, f2) 
z3 <- outer(x, y, f3) 
Grid <- expand.grid(x = x, y = y) 
zp <- c(expression(rho == 0.0), expression(rho == 0.4),        
        expression(rho == 0.8))

## ----plotN2_wireframe,fig.height=4---------------------------------------
library("lattice")
wireframe( z1 + z2 + z3 ~ x * y, data = Grid, xlab = "x", ylab = "y",          
zlab = "z", outer = TRUE, layout = c(3, 1),          
strip = strip.custom(factor.levels = zp) )

## ----plotN2_contureplot,fig.height=4-------------------------------------
contourplot(z1 + z2 + z3 ~ x * y, data = Grid, xlab = "x", ylab = "y",              
           outer = TRUE, layout = c(3, 1), aspect = "xy",             
           cuts = 11, strip = strip.custom(factor.levels = zp))

## ----plotN2_levelplot,fig.height=4---------------------------------------
levelplot(z1 + z2 + z3 ~ x * y, data = Grid, xlab = "x", ylab = "y", 
          outer = TRUE, layout = c(3, 1), aspect = "xy", 
          strip = strip.custom(factor.levels = zp))

## ------------------------------------------------------------------------
x <- seq(-3, 3, length = 50)
y <- x 
z1 <- outer(x, y, f1)
z2 <- outer(x, y, f2)
z3 <- outer(x, y, f3)
Grid <- expand.grid(x = x, y = y)
DF1 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z1))
DF2 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z2))
DF3 <- data.frame(x = Grid$x, y = Grid$y, z = as.vector(z3))
DF1$r = "rho == 0.0" 
DF2$r = "rho == 0.4"
DF3$r = "rho == 0.8" 
BDF <- rbind(DF1, DF2, DF3)

## ----plotN2_ggplot2_contour,fig.height=4---------------------------------
library("ggplot2")
p <- ggplot(data = BDF, aes(x = x, y = y, z = z)) 
p + stat_contour(aes(colour = ..level..)) + theme_bw() +  
    scale_colour_gradient(low = "gray10", high = "gray90") +  
    labs(colour = "Density", x = "x", y = "y") +  
    facet_grid(. ~ r, labeller = label_parsed) +  
    coord_fixed(ratio = 1)

## ----plotN2_ggplot2_heat,fig.height=4------------------------------------
p <- ggplot(data = BDF, aes(x = x, y = y, fill = z))
p + geom_raster() + theme_bw() +  
    scale_fill_gradient(low = "gray10", high = "gray90") +  
    labs(fill = "Density", x = "x", y = "y") + 
    facet_grid(. ~ r, labeller = label_parsed) +  
    coord_fixed(ratio = 1)

