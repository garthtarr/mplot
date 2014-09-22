# https://www.ma.utexas.edu/users/mks/statmistakes/stepwise.html

require(MPV)
pathoeg
round(cor(pathoeg),1)
summary(step(lm(y~.,data=pathoeg)),k=log(n))

# Designing my own pathological example 
# To be sure that y really was generated
# by a simple linear regression (y~x8).
n=50
set.seed(8) # a seed of 2 also works
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
round(cor(df),1)
mf=lm(y~.,data=df)
m0=lm(y~1,data=df)
summary(step(mf))


step(m0,scope=list(lower=m0,upper=mf),direction="forward",k=log(n))
step(m0,scope=list(lower=m0,upper=mf),direction="forward",k=2)


af1 = af(mf)
#summary(lm(y~x8,data=df))