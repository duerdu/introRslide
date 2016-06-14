
# 随机数据 生成
n=1000;sdout=3
x=rnorm(n,mean=0,sd=1)
y=rnorm(n,mean=5,sd=2)

par(mfrow = c(1, 2), ann = FALSE)
options(repr.plot.width=8, repr.plot.height=4)
# 二维布局 
plot(x,y,pch=19,cex=1)
oindx=abs(x-0)>sdout*1;oindy=abs(y-5)>sdout*2
oindxy=oindx | oindy  
points(x[oindxy],y[oindxy],pch='+',col='red',cex=2)
# 一维分布
ovalx=x[oindx]
hist(x,xlim=range(x))
lines(sort(x),dnorm(sort(x))*450)
points(ovalx,dnorm(ovalx),pch='+',col='red',cex=2)

# 箱图 （中位数，四分位数，异常值）
options(repr.plot.width=6, repr.plot.height=5)
boxplot(x,y)
bx=boxplot.stats(x)
#for (i in bx$out) points(i,pch='+',col='red',cex=1)
points(rep(1,length(bx$out)),bx$out,pch='x',col='red',cex=1)
points(rep(1,5),bx$stats,pch='x',col='blue',cex=1)

#上下边框
bx$stats
fivenum(x)
t(quantile(x,c(25,50,75)/100))
fivenum


# 上下边界  （help(boxplot.stats),看coef参数）
#http://r.789695.n4.nabble.com/Whiskers-on-the-default-boxplot-graphics-td2195503.html
IQR(x)
quantile(x,75/100)-quantile(x,25/100)
quantile(x,75/100)+1.5*IQR(x)
bx$stats
sort( x[x>2    & (x< (quantile(x,75/100)+1.5*IQR(x)))] )
sort( x[x<(-2) & (x> (quantile(x,25/100)-1.5*IQR(x)))] )


options(repr.plot.width=8, repr.plot.height=4)
par(mfrow = c(1, 2), ann = FALSE)
#
x2=c(rnorm(n,mean=0,sd=1),rnorm(n/3,mean=8,sd=2))
xh=hist(x2,breaks=seq(range(x2)[1]-1,range(x2)[2]+1,0.5))
#
xh=hist(x2,breaks=seq(range(x2)[1]-1,range(x2)[2]+1,0.5))
lines(range(xh$mids[xh$density<0.01 & xh$mids <5 & xh$mids>1]),c(2,2),col='red',lwd=10 )
par(new=TRUE)
boxplot(x2,horizontal = TRUE,col='grey')

options(repr.plot.width=8, repr.plot.height=4)
par(mfrow = c(1, 2), ann = FALSE)
library(DMwR)
#
y2=c(rnorm(n,mean=0,sd=4),rnorm(n/3,mean=6,sd=3))
dfxy=data.frame(x2=x2,y2=y2)
plot(dfxy,pch=19,cex=1)
#interesting when change k
plot(dfxy,pch=19,cex=1)
score_dfxy=lofactor(dfxy, k = 5)
out_dfxy=order(score_dfxy, decreasing = T)[1:10]
points(dfxy[out_dfxy,],col='red',pch='x',cex=1.2)

dfxy$cc=NULL

options(repr.plot.width=6, repr.plot.height=5)
nc=5
cc=kmeans(dfxy,nc)
ccout=fitted(cc)
plot(y2~x2,col=rownames(ccout),dfxy,cex=2,pch = ".")
#centers
points(cc$centers, col = 1:nc, pch = "o", cex = 2)  
#outliers
centers=cc$centers[cc$cluster,]
dists=sqrt(rowSums((dfxy-centers)^2))
outers=order(dists,decreasing = TRUE)[1:20]
points(dfxy[outers,c('x2','y2')], col = 9, pch = "+", cex = 1)  

options(repr.plot.width=8, repr.plot.height=4)
par(mfrow = c(1, 2), ann = FALSE)
hc = hclust(dist(dfxy), "ave")
plot(hc)
ccout = cutree(hc, k = 5)
plot(y2~x2,col=ccout,dfxy)

library(quantmod)
options("getSymbols.warning4.0"=FALSE)
setSymbolLookup(TL=list(name="000025.sz",src="yahoo"))
getSymbols("TL")
candleChart(TL,theme='white', type='candles') 
