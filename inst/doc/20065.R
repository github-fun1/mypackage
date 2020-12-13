## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(mypackage)

## -----------------------------------------------------------------------------
plot(x=1:100,y=1:100,type='l')

## -----------------------------------------------------------------------------
plot(mtcars$wt, mtcars$mpg)

## -----------------------------------------------------------------------------
head(cars)

## -----------------------------------------------------------------------------
library(knitr)
kable(head(cars))

## -----------------------------------------------------------------------------
set.seed(20065) #set a seed for the random variable generator to get a reproducible result
n=1000
u=runif(1000,0,1)
x=2*(1-u)^(-0.5)

## -----------------------------------------------------------------------------
hist(x,freq = FALSE,
     breaks = 24,
     xlab = "the value of x",
     main="density histogram with theoretical density curve")
xx=seq(0,150,len=1000)
lines(xx,8/xx^3,col="red")
legend("topright",
       legend = 'theoretical density-line',
       lty=1,
       col="red")
box()

## -----------------------------------------------------------------------------
myfunction1<-function(n){
  #generate random variables x from population X ~ f_e
  set.seed(20065) #set a seed for the random variable generator to get a reproducible result
  x=0 #initialization
  for (i in 1:n) {
    #generate n variables by a for loop
    u1=runif(1,min=-1,max=1)
    u2=runif(1,min=-1,max=1)
    u3=runif(1,min=-1,max=1)
    if(abs(u3)>=abs(u2) && abs(u3)>=abs(u1)){
      x[i]=u2
    } 
    else
      x[i]=u3
  }
  return(x) #return n variables as a vector
}


## -----------------------------------------------------------------------------
sampledata=myfunction1(10000)
hist(sampledata,
     freq = FALSE,
     xlim = c(-1,1),
     xlab = "the value of X",
     main="density histogram")
xx=seq(-1,1,len=10000)
lines(xx,0.75*(1-xx^2),col="blue",lty=2)
legend("topright",
       legend = 'f_e(x)',
       lty=2,
       col="blue")
box()

## -----------------------------------------------------------------------------
set.seed(20065) #set a seed for the random variable generator to get a reproducible result
n=1000
u=runif(1000,0,1)
x=2*(1-u)^(-0.25)-2
hist(x,freq = FALSE,
     breaks = 24,
     xlab = "the value of x",
     main="density histogram with theoretical density curve")
xx=seq(0,9,len=1000)
lines(xx,64/(2+xx)^5,col="red")
legend("topright",
       legend = 'theoretical density-line',
       lty=1,
       col="red")
box()

## -----------------------------------------------------------------------------
n=100000
u=runif(n,min=0,max=pi/3)
f<-function(x){pi/3*sin(x)}
y=f(u)
theta.hat=mean(y)
print(list(theta=0.5,theta.hat=theta.hat))

## -----------------------------------------------------------------------------
m=10000000
n=m/2
u=runif(m)
v=u[0:n]
f=function(x){exp(x)}
g=function(y){0.5*(exp(y)+exp(1-y))}
T1=f(u)
T2=g(v)
print(list(theta.theoretical=exp(1)-1,theta.hat.simpleMC=mean(T1),theta.hat.antitheticMC=mean(T2),variance.reduction.empirical=1-var(T2)/var(T1),variance.reduction.theoretical=(0.5*exp(2)-exp(1)-0.5)/(-exp(2)+4*exp(1)-3)))

## -----------------------------------------------------------------------------
theta.theoretical=1+1/(sqrt(2*pi*exp(1)))-pnorm(1)
theta.hat=estimator.var=0
n=1e5
g<-function(x){x^2/sqrt(2*pi)*exp(-0.5*x^2)*(x>=1)}
f1<-function(x){1/sqrt(2*pi)*exp(-0.5*x^2)}
f2<-function(x){x*exp((1-x^2)/2)}

x=rnorm(n) #using f1
fg=g(x)/f1(x)
theta.hat[1]=mean(fg)
estimator.var[1]=var(fg)

y=sqrt(1-2*log(1-runif(n))) #using f2, inverse transform method
fg=g(y)/f2(y)
theta.hat[2]=mean(fg)
estimator.var[2]=var(fg)

f.compare=rbind(theta.hat,estimator.var)
colnames(f.compare)=c("f1","f2")
f.compare.var.percent=1-estimator.var[2]/estimator.var[1]
print(list(theta.theoretical=theta.theoretical,importance.function.compare=f.compare,f2_VS_f1_variance.reduction=f.compare.var.percent))

## -----------------------------------------------------------------------------
n=1e7
k=5
g<-function(x){exp(-x-log(1+x^2))*(x>0)*(x<1)}
theta.hat=se=0

#in Example5.10
f<-function(x){exp(-x)/(1-exp(-1))}
set.seed(20065)
x= -log(1-runif(n)*(1-exp(-1)))
fg=g(x)/f(x)
theta.hat[1]=mean(fg)
se[1]=sd(fg)

#in Example5.13
fj<-function(x){5*exp(-x)/(1-exp(-1))}
theta.hat.j=var.j=0
s.i=c(0,log(5/(4+exp(-1))),log(5/(3+2*exp(-1))),log(5/(2+3*exp(-1))),log(5/(1+4*exp(-1))),1)
set.seed(20065)

for(i in 1:k)
{
  u=runif(n/k,(i-1)/k,i/k)
  x=-log(1-u*(1-exp(-1)))
  #fg=g(x)/fj(x)
  gj<-function(x){exp(-x-log(1+x^2))*(x>0)*(x<1)*(x>s.i[i])*(x<s.i[i+1])}
  fg=gj(x)/fj(x)
  theta.hat.j[i]=mean(fg)
  var.j[i]=var(fg)
}
theta.hat[2]=sum(theta.hat.j)
se[2]=sqrt(mean(var.j))

result=rbind(theta.hat,se)
colnames(result)=c("original","stratified")
se.reduction=1-se[2]/se[1]
print(list(compare_between_2_methods=result,standard_error_reduction=se.reduction))

## -----------------------------------------------------------------------------
n=20
m=1e4
alpha=0.05
a=b=0
set.seed(20065)
x=0
for(i in 1:m){
  x=rnorm(n)
  a[i]=mean(x)-qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
  b[i]=mean(x)+qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
}
interval=cbind(a,b)

cl.empirical=sum((a<0)&(b>0))/m
print(list(average.interval=apply(interval,2,mean),cl.empirical=cl.empirical))

## -----------------------------------------------------------------------------
n=20
m=1e4
alpha=0.05
a=b=0
set.seed(20065)
x=0
for(i in 1:m){
  x=rchisq(n,2)
  a[i]=mean(x)-qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
  b[i]=mean(x)+qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
}
interval=cbind(a,b)
cl.empirical=sum((a<2)&(b>2))/m
print(list(average.interval=apply(interval,2,mean),cl.empirical=cl.empirical))


## ----warning=FALSE------------------------------------------------------------
sk<-function(x){
  #computes the sample skewness coefficient
  xbar=mean(x)
  m3=mean((x-xbar)^3)
  m2=mean((x-xbar)^2)
  return(m3/m2^1.5)
}
n=50 #sample size
m=10000 #replicate times
alp=0.1 #significance level
cv=qnorm(1-alp/2,0,sqrt(6*(n-2)/((n+1)*(n+3))))
alpha=c(seq(1,10,1),seq(20,200,20))
nu=c(seq(3,12,1),seq(20,200,20))
N1=length(alpha)
N2=length(nu)
pwr.beta=numeric(N1)
pwr.t=numeric(N2)
for(j in 1:N1){
  a=alpha[j]
  sktests=numeric(m)
  for(i in 1:m){
    x=rbeta(n,a,a)
    sktests[i]=as.integer(abs(sk(x))>=cv)
  }
  pwr.beta[j]=mean(sktests)
}
for(k in 1:N2){
  nu0=nu[k]
  sktests=numeric(m)
  for(i in 1:m){
    x=rt(n,nu0)
    sktests[i]=as.integer(abs(sk(x))>=cv)
  }
  pwr.t[k]=mean(sktests)
}

mydat=data.frame(alpha,pwr.beta,nu,pwr.t)
library('ggplot2')
ggplot(mydat,aes(x=parameter,y=power))+
  coord_cartesian(xlim = c(0,200),ylim = c(0,1))+
  geom_point(data=mydat,aes(x=alpha,y=pwr.beta),color="green")+
  geom_point(data = mydat,aes(x=nu,y=pwr.t),color="red")+
  geom_hline(aes(yintercept=0.1),linetype='dashed')

## -----------------------------------------------------------------------------
sigma1=1
sigma2=1.5
m=1e4
n1=n2=c(10,20,50,100,200,500,1000)
alpha=0.055 #significance level
power=matrix(0,nrow=length(n1),ncol=2)
#count five test
count5test<-function(x,y){
  X=x-mean(x)
  Y=y-mean(y)
  outx=sum(X>max(Y))+sum(X<min(Y))
  outy=sum(Y>max(X))+sum(Y<min(X))
  return(as.integer(max(c(outx,outy))>5))
}
#F test
ftest<-function(x,y){
  X=x-mean(x)
  Y=y-mean(y)
  cv=c(qf(1-alpha/2,length(Y)-1,length(X)-1),qf(alpha/2,length(Y)-1,length(X)-1))
  f.stat=var(Y)/var(X)
  return(as.integer((f.stat>=cv[1])||(f.stat<=cv[2])))
}

#compute power
for(i in 1:length(n1)){
  test1=test2=0
  testNormal=0
  for(j in 1:m){
    x=rnorm(n1[i],0,sigma1)
    y=rnorm(n2[i],0,sigma2)
    test1[j]=count5test(x,y)
    test2[j]=ftest(x,y)
  }
  power[i,]=c(mean(test1),mean(test2))
}
power.count5test=power[,1]
power.ftest=power[,2]
mydat2=data.frame(n1,power.count5test,power.ftest)
mydat2
ggplot(mydat2,aes(x=n,y=power))+
  coord_cartesian(xlim = c(0,1000),ylim = c(0,1))+
  geom_point(data=mydat2,aes(x=n1,y=power.count5test),color="green")+
  geom_line(data=mydat2,aes(x=n1,y=power.count5test),color="green",linetype='dashed')+
  geom_point(data = mydat2,aes(x=n1,y=power.ftest),color="red")+
  geom_line(data = mydat2,aes(x=n1,y=power.ftest),color="red",linetype='dashed')

## -----------------------------------------------------------------------------
library(MASS)
Mardia<-function(mydata){
  n=nrow(mydata)
  c=ncol(mydata)
  central<-mydata
  for(i in 1:c){
    central[,i]<-mydata[,i]-mean(mydata[,i])
  }
  sigmah<-t(central)%*%central/n
  a<-central%*%solve(sigmah)%*%t(central)
  b<-sum(colSums(a^{3}))/(n*n)
  test<-n*b/6
  chi<-qchisq(0.95,c*(c+1)*(c+2)/6)
  as.integer(test>chi)
}

set.seed(1234)
mu <- c(0,0,0)
sigma <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
m=1000
n<-c(10, 20, 30, 50, 100, 500)
#m: number of replicates; n: sample size
a=numeric(length(n))
for(i in 1:length(n)){
  a[i]=mean(replicate(m, expr={
    mydata <- mvrnorm(n[i],mu,sigma) 
    Mardia(mydata)
  }))
}

## -----------------------------------------------------------------------------
print(a)

## -----------------------------------------------------------------------------
library(MASS)
set.seed(7912)
set.seed(7912)
mu1 <- mu2 <- c(0,0,0)
sigma1 <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
sigma2 <- matrix(c(100,0,0,0,100,0,0,0,100),nrow=3,ncol=3)
sigma=list(sigma1,sigma2)
m=1000
n=50
#m: number of replicates; n: sample size
epsilon <- c(seq(0, .06, .01), seq(.1, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
for (j in 1:N) { #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
  for (i in 1:m) { #for each replicate
    index=sample(c(1, 2), replace = TRUE, size = n, prob = c(1-e, e))
    mydata<-matrix(0,nrow=n,ncol=3)
    for(t in 1:n){
      if(index[t]==1) mydata[t,]=mvrnorm(1,mu1,sigma1) 
      else mydata[t,]=mvrnorm(1,mu2,sigma2)
    }
    sktests[i] <- Mardia(mydata)
  }
  pwr[j] <- mean(sktests)
}
plot(epsilon, pwr, type = "b",
     xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .05, lty = 3)
se <- sqrt(pwr * (1-pwr) / m) #add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
library(bootstrap)
n=nrow(law)
theta.hat=cor(law$LSAT,law$GPA)
theta.jack=numeric(n)
for(i in 1:n){
  theta.jack[i]=cor(law$LSAT[-i],law$GPA[-i])
}
bias.jack=(n-1)*(mean(theta.jack)-theta.hat)
se.jack=sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))
print(list(theta.hat=theta.hat,bias.jack=bias.jack,se.jack=se.jack))

## -----------------------------------------------------------------------------
library("boot")
data(aircondit,package = "boot")
theta.hat=1/mean(aircondit$hours)

boot.obj<-boot(aircondit$hours,R=2000,
               statistic = function(x,i){1/mean(x[i])})
boot.obj
boot.ci(boot.obj,type = c("norm","basic","perc","bca"))

## -----------------------------------------------------------------------------
library(boot)
library(bootstrap)
eigens=eigen(cov(scor))$values
theta.hat=eigens[1]/sum(eigens)
n=nrow(scor)

#jackknife
theta.jack=numeric(n)
for(i in 1:n)
{
  eigens=eigen(cov(scor[-i,]))$values
  theta.jack[i]=eigens[1]/sum(eigens)
}
bias.jack=(n-1)*(mean(theta.jack)-theta.hat)
se.jack=(n-1)*sqrt(var(theta.jack)/n)
print(list(theta.hat=theta.hat, bias.jack=bias.jack, se.jack=se.jack))

## -----------------------------------------------------------------------------
data("ironslag",package = "DAAG")
magnetic<-ironslag$magnetic
chemical<-ironslag$chemical
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3<-e4<- matrix(0,nrow = n*(n-1)/2,ncol=2)
# for n-fold cross validation
# fit models on leave-two-out samples
ii=1
for (k in 1:(n-1)) {
  for(l in (k+1):n){
    y <- magnetic[c(-k,-l)]
    x <- chemical[c(-k,-l)]

    J1 <- lm(y ~ x)
    yhat1.k <- J1$coef[1] + J1$coef[2] * chemical[k]
    yhat1.l <- J1$coef[1] + J1$coef[2] * chemical[l]
    e1[ii,] <- magnetic[c(k,l)] - c(yhat1.k,yhat1.l)

    J2 <- lm(y ~ x + I(x^2))
    yhat2.k <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
    yhat2.l <- J2$coef[1] + J2$coef[2] * chemical[l] + J2$coef[3] * chemical[l]^2
    e2[ii,] <- magnetic[c(k,l)] - c(yhat2.k,yhat2.l)

    J3 <- lm(log(y) ~ x)
    yhat3.k <-exp(J3$coef[1] + J3$coef[2] * chemical[k])
    yhat3.l <-exp(J3$coef[1] + J3$coef[2] * chemical[l])
    e3[ii,] <- magnetic[c(k,l)] - c(yhat3.k,yhat3.l)

    J4 <- lm(log(y) ~ log(x))
    yhat4.k<-exp(J4$coef[1] + J4$coef[2] * log(chemical[k]))
    yhat4.l<-exp(J4$coef[1] + J4$coef[2] * log(chemical[l]))
    e4[ii,] <- magnetic[c(k,l)] - c(yhat4.k,yhat4.l)
    ii=ii+1
  }
}
print(list(MSE.linear=mean(e1^2),MSE.quadratic=mean(e2^2),MSE.exponential=mean(e3^2),MSE.loglog=mean(e4^2)))
lm(magnetic~chemical+I(chemical^2))

## -----------------------------------------------------------------------------
set.seed(12345)
# Count Five test
count5test = function(x, y) {
X = x - mean(x)
Y = y - mean(y)
outx = sum(X > max(Y)) + sum(X < min(Y))
outy = sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
return(as.integer(max(c(outx, outy)) > 5))
}
# Count Five test permutation
count5test_permutation = function(z) {

n = length(z)
x = z[1:(n/2)]
y = z[-(1:(n/2))]
X = x - mean(x)
Y = y - mean(y)
outx = sum(X > max(Y)) + sum(X < min(Y)) 
outy = sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0) 
return(as.integer(max(c(outx, outy)) > 5))
}
permutation = function(z,R) {
  n = length(z)
  out = numeric(R)
  for (r in 1: R){
      p = sample(1:n ,n ,replace = FALSE)
      out[r] = count5test_permutation(z[p])
  }
  sum(out)/R
}              

n1 = 20
n2 = 50
mu1 = mu2 = 0
sigma1 = sigma2 = 1
m = 1e3

alphahat1 = mean(replicate(m, expr={
x = rnorm(n1, mu1, sigma1)
y = rnorm(n2, mu2, sigma2)
x = x - mean(x) #centered by sample mean
y = y - mean(y)
count5test(x, y)
}))
alphahat2 = mean(replicate(m, expr={
x = rnorm(n1, mu1, sigma1)
y = rnorm(n2, mu2, sigma2)
x = x - mean(x) #centered by sample mean 
y = y - mean(y)
z = c(x,y)
permutation(z,1000) 
})<0.05)

round(c(count5test=alphahat1,count5test_permutation=alphahat2),4)

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
library(MASS)

Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5)
  (i1 + i2) / (k * n)
}

eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R, sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}

## -----------------------------------------------------------------------------
mu1 <- c(0,0,0)
sigma1 <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
mu2 <- c(0,0,0)
sigma2 <- matrix(c(2,0,0,0,3,0,0,0,4),nrow=3,ncol=3)
n1=n2=20
n <- n1+n2 
N = c(n1,n2)
k=3
R=999
m=100
set.seed(1234)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  mydata1 <- mvrnorm(n1,mu1,sigma1)
  mydata2 <- mvrnorm(n2,mu2,sigma2)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
mu1 <- c(0,0,0)
sigma1 <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
mu2 <- c(0.5,-0.5,0.5)
sigma2 <- matrix(c(2,0,0,0,2,0,0,0,2),nrow=3,ncol=3)
n1=n2=20
n <- n1+n2 
N = c(n1,n2)
k=3
R=999
m=100
set.seed(1234)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  mydata1 <- mvrnorm(n1,mu1,sigma1)
  mydata2 <- mvrnorm(n2,mu2,sigma2)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
n1=n2=20
n <- n1+n2 
N = c(n1,n2)
k=3
R=999
m=100
set.seed(1234)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  mydata1 <- as.matrix(rt(n1,1,2),ncol=1)
  mydata2 <- as.matrix(rt(n2,2,5),ncol=1)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
n1=n2=20
n <- n1+n2 
N = c(n1,n2)
k=3
R=999
m=100
set.seed(1234)
p.values <- matrix(NA,m,3)
rbimodel<-function(n,mu1,mu2,sd1,sd2){
  index=sample(1:2,n,replace=TRUE)
  x=numeric(n)
  index1<-which(index==1)
  x[index1]<-rnorm(length(index1), mu1, sd1)
  index2<-which(index==2)
  x[index2]<-rnorm(length(index2), mu2, sd2)
  return(x)
}
for(i in 1:m){
  mydata1 <- as.matrix(rbimodel(n1,0,0,1,2),ncol=1)
  mydata2 <- as.matrix(rbimodel(n2,1,1,4,3),ncol=1)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
mu1 <- c(0,0,0)
sigma1 <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
mu2 <- c(0.5,-0.5,0.5)
sigma2 <- matrix(c(2,0,0,0,2,0,0,0,2),nrow=3,ncol=3)
n1=10
n2=100
n <- n1+n2 
N = c(n1,n2)
k=3
R=999
m=100
set.seed(1234)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  mydata1 <- mvrnorm(n1,mu1,sigma1)
  mydata2 <- mvrnorm(n2,mu2,sigma2)
  mydata <- rbind(mydata1,mydata2)
  p.values[i,1] <- eqdist.nn(mydata,N,k)$p.value
  p.values[i,2] <- eqdist.etest(mydata,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=mydata1,y=mydata2,num.permutations=R,seed=i*2846)$p.value
}
alpha <- 0.05;
pow <- colMeans(p.values<alpha)
pow

## -----------------------------------------------------------------------------
set.seed(3000)
lap_f = function(x) exp(-abs(x))

rw.Metropolis = function(sigma, x0, N){
 x = numeric(N)
 x[1] = x0
 u = runif(N)
 k = 0
 for (i in 2:N) {
  y = rnorm(1, x[i-1], sigma)
  if (u[i] <= (lap_f(y) / lap_f(x[i-1]))) x[i] = y 
  else {
  x[i] = x[i-1]
  k = k+1
  }
 }
 return(list(x = x, k = k))
}

N = 2000
sigma = c(.05, .5, 2, 16)
x0 = 25
rw1 = rw.Metropolis(sigma[1],x0,N)
rw2 = rw.Metropolis(sigma[2],x0,N)
rw3 = rw.Metropolis(sigma[3],x0,N)
rw4 = rw.Metropolis(sigma[4],x0,N)
#number of candidate points rejected
Rej = cbind(rw1$k, rw2$k, rw3$k, rw4$k)
Acc = round((N-Rej)/N,4)
rownames(Acc) = "Accept rates"
colnames(Acc) = paste("sigma",sigma)
knitr::kable(Acc)
#plot
par(mfrow=c(2,2))  #display 4 graphs together
    rw = cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
    for (j in 1:4) {
        plot(rw[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rw[,j]))
    }
    

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
# psi[i,j] is the statistic psi(X[i,1:j])
# for chain in i-th row of X
psi <- as.matrix(psi)
n <- ncol(psi)
k <- nrow(psi)
psi.means <- rowMeans(psi) #row means
B <- n * var(psi.means) #between variance est.
psi.w <- apply(psi, 1, "var") #within variances
W <- mean(psi.w) #within est.
v.hat <- W*(n-1)/n + (B/n) #upper variance est.
r.hat <- v.hat / W #G-R statistic
return(r.hat)
}

k <- 4    # four chains
x0 <- c(-10,-5,5,10)    # overdispersed initial values
N <- 10000    # length of chains
b <- 200    # burn-in length

par(mfrow=c(2,2))

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(0.5,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (1000+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[(1000+1):N], type="l", xlab="sigma=0.5", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(1,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (500+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x2 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(500+1):N], type="l", xlab="sigma=1", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(4,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (b+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x3 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(b+1):N], type="l", xlab="sigma=4", ylab="R_hat")
abline(h=1.2, lty=2)

X <- matrix(nrow=k,ncol=N)
for (i in 1:k)
  X[i,] <- rw.Metropolis(16,x0[i],N)$x
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
psi[i,] <- psi[i,] / (1:ncol(psi))
rhat <- rep(0, N)
for (j in (b+1):N)
rhat[j] <- Gelman.Rubin(psi[,1:j])
x4 <- min(which(rhat>0 & rhat<1.2))
plot(rhat[(b+1):N], type="l", xlab="sigma=16", ylab="R_hat")
abline(h=1.2, lty=2)

c(x2,x3,x4)

## -----------------------------------------------------------------------------
k = c(4:25,100,500,1000)
S = function(a,k){
 ck = sqrt(a^2*k/(k+1-a^2))
 pt(ck,df=k,lower.tail=FALSE)
}

f = function(a,k){S(a,k)-S(a,k-1)}
#curve(f(x),xlim = c(0,sqrt(k)))
a <- seq(0, 4, by=0.01)
plot(a, f(a, k[23]), lty=1, col=1, type="l", xlim=c(0, 4), xlab="a", ylab="f(a|k)", main="f(a) with different k")
lines(a, f(a, k[24]), xlim = c(0, 4), lty=2, col=2)
lines(a, f(a, k[25]), xlim = c(0, 4), lty=3, col=3)
legend("topright", legend=c("k=100", "k=500", "k=1000"), col=1:3,lty=1:3)
# So the lower and upper bound in function uniroot should be 1 and 2 respectively

solve = function(k){
  output = uniroot(function(a){S(a,k)-S(a,k-1)},lower=1,upper=2)
  output$root
}

root = matrix(0,2,length(k))

for (i in 1:length(k)){
  root[2,i]=round(solve(k[i]),4)
}

root[1,] = k
rownames(root) = c('k','A(k)')
root

## -----------------------------------------------------------------------------
ABO.em<-function(p.ini,n.obs){
  M=1e4 #maximum ierations
  tol=.Machine$double.eps #when to converge

  n=sum(n.obs)
  nA.=n.obs[1]
  nB.=n.obs[2]
  nOO=n.obs[3]
  nAB=n.obs[4]
  
  p=q=r=numeric(0)
  p[1]=p.ini[1]
  q[1]=p.ini[2]
  r[1]=1-p[1]-q[1]
  iter=1
  
  for(i in 2:M){
    p.old=p[i-1]
    q.old=q[i-1]
    r.old=r[i-1]
    
    nAA.t=nA.*p.old^2/(p.old^2+2*p.old*r.old)
    nAO.t=nA.*2*p.old*r.old/(p.old^2+2*p.old*r.old)
    nBB.t=nB.*q.old^2/(q.old^2+2*q.old*r.old)
    nBO.t=nB.*2*q.old*r.old/(q.old^2+2*q.old*r.old)
    nOO.t=nOO
    nAB.t=nAB
    
    p[i]=(2*nAA.t+nAO.t+nAB.t)/2/n
    q[i]=(2*nBB.t+nBO.t+nAB.t)/2/n
    r[i]=(2*nOO.t+nAO.t+nBO.t)/2/n
    iter=iter+1
    
    U=abs((p[i]-p.old)/p.old)<=tol
    V=abs((q[i]-q.old)/q.old)<=tol
    W=abs((r[i]-r.old)/r.old)<=tol
    if(U&&V&&W)
      break
  }
  list(p.mle.em=p[iter],q.mle.em=q[iter],r.mle.em=r[iter],iter=iter)
}
nObs=c(444,132,361,63)
pInitial=c(1/3,1/3) #initial p,q value
em.result<-ABO.em(p.ini=pInitial,n.obs=nObs)
print(em.result)

## -----------------------------------------------------------------------------
ABO.em.trend<-function(p.ini,n.obs){
  M=1e4 #maximum ierations
  tol=.Machine$double.eps #when to converge

  n=sum(n.obs)
  nA.=n.obs[1]
  nB.=n.obs[2]
  nOO=n.obs[3]
  nAB=n.obs[4]
  
  p=q=r=numeric(0)
  loglikelihood=numeric(0)
  p[1]=p.ini[1]
  q[1]=p.ini[2]
  r[1]=1-p[1]-q[1]
  loglikelihood[1]=0
  iter=1
  
  for(i in 2:M){
    p.old=p[i-1]
    q.old=q[i-1]
    r.old=r[i-1]
    
    nAA.t=nA.*p.old^2/(p.old^2+2*p.old*r.old)
    nAO.t=nA.*2*p.old*r.old/(p.old^2+2*p.old*r.old)
    nBB.t=nB.*q.old^2/(q.old^2+2*q.old*r.old)
    nBO.t=nB.*2*q.old*r.old/(q.old^2+2*q.old*r.old)
    nOO.t=nOO
    nAB.t=nAB
    
    p[i]=(2*nAA.t+nAO.t+nAB.t)/2/n
    q[i]=(2*nBB.t+nBO.t+nAB.t)/2/n
    r[i]=(2*nOO.t+nAO.t+nBO.t)/2/n
    iter=iter+1
    
    loglikelihood[i]=nAA.t*2*log(p[i])+nAO.t*log(2*p[i]*r[i])+nBB.t*2*log(q[i])+nBO.t*log(q[i]*r[i])+nOO.t*2*log(r[i])+nAB.t*log(2*p[i]*q[i])
    
    U=abs((p[i]-p.old)/p.old)<=tol
    V=abs((q[i]-q.old)/q.old)<=tol
    W=abs((r[i]-r.old)/r.old)<=tol
    if(U&&V&&W)
      break
  }
  list(p.mle.em=p[iter],q.mle.em=q[iter],r.mle.em=r[iter],iter=iter,p.mle.all=p,q.mle.all=q,loglikelihoods=loglikelihood)
}
nObs=c(444,132,361,63)
pInitial=c(0.4,0.3) #initial p,q value
em.result<-ABO.em.trend(p.ini=pInitial,n.obs=nObs)

par(mfrow=c(1,2))
plot(em.result$p.mle.all,xlab = "iter",ylab = "p.mle",ylim = c(0,0.4))

plot(em.result$q.mle.all,xlab = "iter",ylab = "q.mle",ylim=c(0,0.4))


## -----------------------------------------------------------------------------
plot(em.result$loglikelihoods[-1],xlab = "iter",ylab = "loglikehood")

## -----------------------------------------------------------------------------
formulas<-list(
  mpg~disp,
  mpg~I(1/disp),
  mpg~disp+wt,
  mpg~I(1/disp)+wt
)

## -----------------------------------------------------------------------------
#for loop
for (i in 1:length(formulas)) {
  mod<-lm(formulas[[i]],mtcars)
  print(mod)
}

#lapply()
lapply(formulas, function(x) lm(data=mtcars,x))

## -----------------------------------------------------------------------------
trials<-replicate(
  100,
  t.test(rpois(10,10),rpois(7,10)),
  simplify = FALSE
)

set.seed(20065)
#Use sapply
sapply(trials,function(x) x$p.value)

## -----------------------------------------------------------------------------
#extra challenge: using [[ instead of anonymous function
sapply(trials,"[[",3)

## -----------------------------------------------------------------------------
myapply<-function(data,f,output.type){
  tmp<-Map(f,data)
  vapply(tmp,function(x) x ,output.type)
}

##Example
myapply(mtcars,sd,double(1))

## -----------------------------------------------------------------------------
# R Function

set.seed(3000)
lap_f = function(x) exp(-abs(x))

rw.Metropolis = function(sigma, x0, N){
 x = numeric(N)
 x[1] = x0
 u = runif(N)
 k = 0
 for (i in 2:N) {
  y = rnorm(1, x[i-1], sigma)
  if (u[i] <= (lap_f(y) / lap_f(x[i-1]))) x[i] = y 
  else {
  x[i] = x[i-1]
  k = k+1
  }
 }
 return(list(x = x, k = k))
}

N = 2000
sigma = c(.05, .5, 2, 16)
x0 = 25
rw1 = rw.Metropolis(sigma[1],x0,N)
rw2 = rw.Metropolis(sigma[2],x0,N)
rw3 = rw.Metropolis(sigma[3],x0,N)
rw4 = rw.Metropolis(sigma[4],x0,N)
#number of candidate points rejected
Rej = cbind(rw1$k, rw2$k, rw3$k, rw4$k)
Acc = round((N-Rej)/N,4)
rownames(Acc) = "Accept rates"
colnames(Acc) = paste("sigma",sigma)
knitr::kable(Acc)

par(mfrow=c(2,2))  #display 4 graphs together
    rw = cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
    for (j in 1:4) {
        plot(rw[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rw[,j]))
    }

## -----------------------------------------------------------------------------
#library(Rcpp)
#sourceCpp('MetropolisCpp.cpp')
#test: rw=MetropolisCpp(2,25,2000)

N = 2000
sigma = c(.05, .5, 2, 16)
x0 = 25
rw1.c = MetropolisCpp(sigma[1],x0,N)
rw2.c = MetropolisCpp(sigma[2],x0,N)
rw3.c = MetropolisCpp(sigma[3],x0,N)
rw4.c = MetropolisCpp(sigma[4],x0,N)
#number of candidate points rejected
Rej = cbind(rw1.c$k, rw2.c$k, rw3.c$k, rw4.c$k)
Acc = round((N-Rej)/N,4)
rownames(Acc) = "Accept rates"
colnames(Acc) = paste("sigma",sigma)
knitr::kable(Acc)

par(mfrow=c(2,2))  #display 4 graphs together
    rw.c = cbind(rw1.c$x, rw2.c$x, rw3.c$x,  rw4.c$x)
    for (j in 1:4) {
        plot(rw.c[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rw.c[,j]))
    }

## -----------------------------------------------------------------------------
#Compare the two results with quantiles (qqplot)
a=c(0.05,seq(0.1,0.9,0.1),0.95)
rw = cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
rw.c = cbind(rw1.c$x, rw2.c$x, rw3.c$x,  rw4.c$x)
mc1=rw[501:N,]
mc2=rw.c[501:N,]
Qrw=apply(mc1,2,function(x) quantile(x,a))
Qrw.c=apply(mc2,2,function(x) quantile(x,a))
Qtable=round(cbind(Qrw,Qrw.c),3)
colnames(Qtable)=c("rw1","rw2","rw3","rw4","rw1.c","rw2.c","rw3.c","rw4.c")
Qtable

#qqplot
aa=ppoints(100)
QQrw3=quantile(rw3$x[501:N],aa)
QQrw3.c=quantile(rw3.c$x[501:N],aa)
qqplot(QQrw3,QQrw3.c,main="",xlab="rw3 quantiles",ylab="rw3.c quantiles")
qqline(QQrw3.c)


## -----------------------------------------------------------------------------
#compare the computing time of the two functions with microbenchmark
library(microbenchmark)
microbenchmark(
  rw.Metropolis(sigma[3],x0,N),
  MetropolisCpp(sigma[3],x0,N))
print(22997.425/1011.025)


