
## ----artificial.eg, results='asis', tidy=FALSE---------------------------
require(mplot,quietly=TRUE)
op = options(gvis.plot.tag = "chart")
n = 100
set.seed(11)
e = rnorm(n)
x0 = 1
x1 = rnorm(n)
x2 = rnorm(n)
x3 = x1^2
x4 = x2^2
x5 = x1*x2
y = x0 + x1 + x2 + e
dat = data.frame(y,x1,x2,x3,x4,x5)
af1 = af(y~., data=dat, n.cores=3)


## ------------------------------------------------------------------------
summary(af1)


## ----af1plot, results='asis', tidy=FALSE---------------------------------
plot(af1)


## ----bdat.full.plot, results='asis', tidy=FALSE--------------------------
bfat.full = af(Bodyfat~.,data=bodyfat,n.cores=3, n.c=100,initial.stepwise=FALSE)
plot(bfat.full)


## ----bfat.stepwise.plot, results='asis', tidy=FALSE----------------------
bfat.stepwise = af(Bodyfat~.,data=bodyfat,n.cores=3, n.c=50,initial.stepwise=TRUE)
plot(bfat.stepwise)


## ------------------------------------------------------------------------
summary(bfat.stepwise)


## ----binom.eg, results='asis', tidy=FALSE--------------------------------
require(mvtnorm)
n=200
set.seed(11)
p = 6
cor.mat = diag(p)
cor.mat[1,2] = cor.mat[2,1] = 0.7
cor.mat[5,6] = cor.mat[6,5] = 0.7
X = rmvnorm(n,sigma=cor.mat)
beta = c(1,2.1,0,1.5,0,0.9,1.1)
z = cbind(1,X)%*%beta
pr = 1/(1+exp(-z))
y = rbinom(n,1,pr) 
glm.df = data.frame(y,X)
af.glm = af(fixed=y~., data=glm.df, family="binomial", n.cores=4)


## ----af.glm.plot, results='asis', tidy=FALSE-----------------------------
plot(af.glm)


## ----lvp1.eg, results='asis', tidy=FALSE, warning=FALSE------------------
lvp1 = lvp(fixed=y~.,data=glm.df,family="binomial",B=50)


## ----msc, results='asis', tidy=FALSE-------------------------------------
plot(lvp1,which="msc",highlight="X1")


## ----boot, results='asis', tidy=FALSE------------------------------------
plot(lvp1,which="boot",highlight="X1")


## ----vip, results='asis', tidy=FALSE-------------------------------------
plot(lvp1,which="vip")


