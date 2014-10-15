## ----artificial.eg, tidy=FALSE-------------------------------------------
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
lm1 = lm(y~., data=dat)
af1 = af(lm1, n.cores=4)
summary(af1)

## ----af1plot, results='asis', tidy=FALSE---------------------------------
plot(af1)

## ----af.stepwise, tidy=FALSE---------------------------------------------
af1.step = af(lm1, n.cores=4, initial.stepwise=TRUE)
summary(af1.step)

## ----af1.step.plot, results='asis', tidy=FALSE---------------------------
plot(af1.step)

## ----afeg2---------------------------------------------------------------
n = 100
set.seed(11)
e = rnorm(n)
x0 = 1
x1 = rnorm(n)
x2 = rnorm(n)
x6 = rep(c("A","B","C","D"),n/4)
x3 = x1^2
x4 = x2^2
x5 = x1*x2
gp = as.numeric(factor(x6)) 
yI = x0 + x1 + x2 + gp + e
datI = data.frame(yI,x1,x2,x3,x4,x5,x6)
lmI = lm(yI~., data=datI)
af2 = af(lmI, n.cores=4, initial.stepwise=TRUE)
summary(af2)

## ----af2plot, results='asis', tidy=FALSE---------------------------------
plot(af2)

## ----binom.eg, results='hide', tidy=FALSE, eval=FALSE--------------------
#  require(mvtnorm)
#  n=200
#  set.seed(11)
#  p = 4
#  cor.mat = diag(p)
#  cor.mat[1,2] = cor.mat[2,1] = 0.7
#  X = rmvnorm(n,sigma=cor.mat)
#  beta = c(1,2.1,0,0.9,1.1)
#  z = cbind(1,X)%*%beta
#  pr = 1/(1+exp(-z))
#  y = rbinom(n,1,pr)
#  glm.df = data.frame(y,X)
#  glm1 = glm(y~.,data=glm.df,family="binomial")
#  af.glm = af(glm1, n.cores=4, c.max=14, n.c=40, B = 100)
#  summary(af.glm)
#  plot(af.glm)

## ----bdat.full.plot, results='asis', tidy=FALSE--------------------------
data(bodyfat)
bfat.lm = lm(Bodyfat~.,data=subset(bodyfat,select=-Id))
bfat.full = af(bfat.lm, n.cores=4, initial.stepwise=FALSE)
plot(bfat.full)

## ----bfat.stepwise.plot, results='asis', tidy=FALSE----------------------
bfat.rest = af(bfat.lm, n.cores=4, n.c=50, c.max = 20)
plot(bfat.rest)

## ------------------------------------------------------------------------
summary(bfat.rest)

## ----vis1.eg, results='asis', tidy=FALSE---------------------------------
vis1 = vis(lmI,n.cores=4,B=50)

## ----msc, results='asis', tidy=FALSE-------------------------------------
plot(vis1,which="lvk",highlight="x1")

## ----boot, results='asis', tidy=FALSE------------------------------------
plot(vis1,which="boot",highlight="x1")

## ----vip, results='asis', tidy=FALSE-------------------------------------
plot(vis1,which="vip")

## ----vis, results='hide', tidy=FALSE-------------------------------------
data(bodyfat)
vis.bf = vis(bfat.lm, n.cores=4, nvmax = 4)

## ----, lvk.bf, results='asis'--------------------------------------------
plot(vis.bf, which="lvk",highlight = "Abdo")

## ----, boot2, results='asis'---------------------------------------------
plot(vis.bf, which="boot", highlight = "Abdo")

## ----, vip2, results='asis'----------------------------------------------
plot(vis.bf, which="vip")

