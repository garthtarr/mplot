## ------------------------------------------------------------------------
n=50
set.seed(8)
x1 = rnorm(n,0.22,2)
x7 = 0.5*x1 + rnorm(n,0,sd=2)
x6 = -0.75*x1 + rnorm(n,0,3)
x3 = -0.5-0.5*x6 + rnorm(n,0,2)
x9 = rnorm(n,0.6,3.5)
x4 = 0.5*x9 + rnorm(n,0,sd=3)
x2 = -0.5 + 0.5*x9 + rnorm(n,0,sd=2)
x5 = -0.5*x2+0.5*x3+0.5*x6-0.5*x9+rnorm(n,0,1.5)
x8 = x1 + x2 -2*x3 - 0.3*x4 + x5 - 1.6*x6 - 1*x7 + x9 +rnorm(n,0,0.5)
y = 0.6*x8 + rnorm(n,0,2)
df = round(data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,y),1)

## ------------------------------------------------------------------------
round(cor(df),1)
plot(df)

## ------------------------------------------------------------------------
mf = lm(y~.,data=df)
summary(mf)
mf.step=step(mf)
summary(mf.step)

## ----, results='asis', tidy=FALSE----------------------------------------
require(mplot)
af1 = af(mf)
op = options(gvis.plot.tag = "chart")
plot(af1)

## ----vip, results='asis', tidy=FALSE-------------------------------------
vis1 = vis(mf)
plot(vis1,which="vip")

## ----lvk, results='asis', tidy=FALSE-------------------------------------
plot(vis1,which="lvk")

## ----boot, results='asis', tidy=FALSE------------------------------------
plot(vis1,which="boot")

## ----, eval=FALSE--------------------------------------------------------
#  mplot(mf, vis=vis1, af=af1)

