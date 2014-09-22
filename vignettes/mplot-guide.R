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
lm1 = lm(y~., data=dat)
af1 = af(lm1, n.cores=3)

## ------------------------------------------------------------------------
summary(af1)

## ----af1plot, results='asis', tidy=FALSE---------------------------------
plot(af1)

