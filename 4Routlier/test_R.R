
#
n=1000;sdout=3
x=rnorm(n,mean=0,sd=1)
y=rnorm(n,mean=5,sd=2)
plot(x,y,pch=19,cex=1)
oindx=abs(x-0)>sdout*1;oindy=abs(y-5)>sdout*2
oindxy=oindx | oindy  
points(x[oindxy],y[oindxy],pch='+',col='red',cex=2)

ovalx=x[oindx]
hist(x,xlim=range(x))
lines(sort(x),dnorm(sort(x))*450)
points(ovalx,dnorm(ovalx),pch='+',col='red',cex=2)

#
boxplot(x,y)
bx=boxplot.stats(x)
#for (i in bx$out) points(i,pch='+',col='red',cex=1)
points(rep(1,length(bx$out)),bx$out,pch='x',col='red',cex=1)
points(rep(1,5),bx$stats,pch='x',col='blue',cex=1)

bx$stats
fivenum(x)
fivenum
quantile(x,c(25,50,75)/100)

#http://r.789695.n4.nabble.com/Whiskers-on-the-default-boxplot-graphics-td2195503.html
quantile(x,75/100)-quantile(x,25/100)
quantile(x,75/100)+1.5*IQR(x)
sort( x[x>2    & (x< (quantile(x,75/100)+1.5*IQR(x)))] )
sort( x[x<(-2) & (x> (quantile(x,25/100)-1.5*IQR(x)))] )

#### 图有偏移
x2=c(rnorm(n,mean=0,sd=1),rnorm(n/3,mean=8,sd=2))
xh=hist(x2,breaks=seq(-10,20,0.5))
lines(range(xh$mids[xh$density<0.01 & xh$mids <5 & xh$mids>1]),c(2,2),col='red',lwd=10 )
par(new=TRUE)
boxplot(x2,horizontal = TRUE,col='grey')

# https://en.wikipedia.org/wiki/Local_outlier_factor
library(DMwR)
y2=c(rnorm(n,mean=0,sd=4),rnorm(n/3,mean=6,sd=3))
dfxy=data.frame(x2=x2,y2=y2)
plot(dfxy)
#interesting when change k
score_dfxy=lofactor(dfxy, k = 5)
out_dfxy=order(score_dfxy, decreasing = T)[1:20]
points(dfxy[out_dfxy,],col='red',pch='x',cex=1)

#
library(quantmod)
options("getSymbols.warning4.0"=FALSE)
setSymbolLookup(TL=list(name="000025.sz",src="yahoo"))
getSymbols("TL")
candleChart(TL,theme='white', type='candles') 

