<<<<<<< HEAD
## -----------------------------------------------------------------------------
library(pander)

## -----------------------------------------------------------------------------
pander(head(cars))

## -----------------------------------------------------------------------------
lm.cars <- lm(dist~speed, data = cars)
pander(lm.cars$coefficients)
summary(lm.cars)

## -----------------------------------------------------------------------------
plot(cars)
abline(a=-17.58, b=3.932, col="red")
title(main = "Data point and fit line")
legend("topleft", title = "Legend", c("fit line"), col = c("red"), pch = " ", lty = 1)

## -----------------------------------------------------------------------------
plot(lm.cars)

## -----------------------------------------------------------------------------
library(EnvStats)

## -----------------------------------------------------------------------------
set.seed(0)
# Generate random sample by inverse transform method
U <- runif(100)
X <- 2/sqrt(1-U)

# Graph for comparison, the density of Pareto(2,2) is 8/x^3
d <- seq(0, 10, by=0.1)
hist(X, col = "pink", freq = FALSE, xlim = c(1, 10), ylim = c(0, 0.5))
lines(d, 8/d^3, lwd=3)

## -----------------------------------------------------------------------------
set.seed(0)
n <- 1000
j <- 0
k <- 0
y <- numeric(n)
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- runif(1) #random variate from g(.)
  if (27*x^2*(1-x)/4 > u) {
    #we accept x
    k <- k + 1
    y[k] <- x
  }
}
j # The number of experiments for generating n random samples

# histogram of the sample with the theoretical Beta(3,2) density superimposed
d <- seq(0,1,length.out=100)
hist(y, freq = FALSE, col = "pink", ylim = c(0,2))
lines(d, 12*d^2*(1-d), lwd=3)

# Quantile-quantile plot for ‘rbeta’ and ‘acceptance-rejection’ algorithm.
z <- rbeta(1000, shape1 = 3, shape2 = 2)
qqplot(y, z, xlab = "Acceptance-rejection", ylab = "rbeta")
abline(a=0, b=1, col="red")

## -----------------------------------------------------------------------------
set.seed(0)

beta_ac_rej <- function(a, b, n=1000){
  j <- 0
  k <- 0
  y <- numeric(n)
  c <- -optim(0.5, function(x) -x^(a-1)*(1-x)^(b-1)/beta(a,b), method = "Brent", lower = 0, upper = 1)$value
  while (k < n) {
    u <- runif(1)
    j <- j + 1
    x <- runif(1) #random variate from g(.)
    if (x^(a-1)*(1-x)^(b-1)/(c*beta(a,b)) > u) {
      #we accept x
      k <- k + 1
      y[k] <- x
    }
  }
  result <- list(observation=y, iteration=j)
}

result <- beta_ac_rej(3,2)
result$iteration
d <- seq(0, 1, by=0.01)
y <- result$observation
hist(y, freq = FALSE, col = "pink", ylim = c(0,2))
lines(d, 12*d^2*(1-d), lwd=3)

## -----------------------------------------------------------------------------
n <- 1000
lambda <- rgamma(n, shape = 4, rate = 2)
y <- rexp(n, lambda)

## -----------------------------------------------------------------------------
d <- seq(0,5,by=0.01)
hist(y, freq = FALSE, col = "pink", xlim = c(0,5))
lines(d, y=64/(2+d)^5, lwd=3)

## -----------------------------------------------------------------------------
set.seed(0)
# This part is copied from bb
quick_sort <- function(x){
  num <- length(x)
  if(num==0||num==1){return(x)
  }else{
    a <- x[1]
    y <- x[-1]
    lower <- y[y<a]
    upper <- y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}#form a loop
}


test<-sample(1:1e4)
system.time(quick_sort(test))[1]
test <- quick_sort(test)
# show the result of fast sort algorithm
test[1:10]
test[9991:10000]

## -----------------------------------------------------------------------------
set.seed(0)
n <- c(1e4, 2e4, 4e4, 6e4, 8e4)
computation_time <- function(n){
  t <- numeric(100)
  set.seed(0)
  for(i in 1:100){
    test <- sample(1:n)
    t[i] <- system.time(quick_sort(test))[1]
  }
  t_mean <- mean(t)
  return(t_mean)
}


an <- c(computation_time(n[1]),computation_time(n[2]),computation_time(n[3]),
       computation_time(n[4]),computation_time(n[5]))
an

## -----------------------------------------------------------------------------
tn <- n*log(n)
mylm <- lm(an~tn)
x <- seq(0,1e6,length.out=100)
b <- coefficients(mylm)
plot(tn, an, main="Regression line")
lines(x, b[1]+b[2]*x, col="red")

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
U <- runif(m)
theta1 <- mean(exp(U))                # simple MC estimator
theta2 <- mean((exp(U)+exp(1-U))/2)   # antithetic variables estimator
var1 <- var(exp(U))                   # sample variance of simple MC
var2 <- var((exp(U)+exp(1-U))/2)      # sample variance of antithetic variables
theta1
theta2
100*(var1-var2)/var1      # empirical estimator of percent reduction of variance

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
x <- rnorm(m)

g <- function(x){
  x^2*exp(-x^2/2)/sqrt(2*pi)*(x>1)
}
f1 <- function(x) dnorm(x)

theta.hat1 <- mean(g(x)/f1(x))
var1 <- var(g(x)/f1(x))
cbind(theta.hat1, var1)

## -----------------------------------------------------------------------------
set.seed(0)
y <- rgamma(m,shape = 3,rate = 1)
f2 <- function(x) dgamma(x, shape = 3, rate = 1)

theta.hat2 <- mean(g(y)/f2(y))
var2 <- var(g(y)/f2(y))
cbind(theta.hat2, var2)

## -----------------------------------------------------------------------------
d <- seq(1, 5, 0.05)
gs <- c(expression(g(x)==x^2*e^{-x^2/2}/sqrt(2*pi)),
        expression(f[1](x)==e^{-x^2/2}/sqrt(2*pi)),
        expression(f[2](x)==x^2*e^{-x}/2))
par(mfrow=c(1,2))
#figure (a)
plot(d, g(d), type = "l", ylab = "", ylim = c(0,0.5),
     lwd = 2,col=1,main='(A)')
lines(d, f1(d), lty = 2, lwd = 2,col=2)
lines(d, f2(d), lty = 3, lwd = 2,col=3)
legend("topright", legend = gs, lty = 1:3,
       lwd = 2, inset = 0.02,col=1:3)
#figure (b)
plot(d, g(d)/f1(d), type = "l", ylab = "", ylim = c(0,3),
     lwd = 2, lty = 2, col = 2, main = "(B)")
lines(d, g(d)/f2(d), lty = 3, lwd = 2, col = 3)
legend("topright", legend = gs[-1], lty = 2:3, lwd = 2,
       inset = 0.02, col = 2:3)

## -----------------------------------------------------------------------------
a <- numeric(5)
a[1] <- -log(0.8+exp(-1)/5)
a[5] <- 1
for(i in 2:4){
  a[i] <- -log(exp(-a[i-1])-0.2+exp(-1)/5)
}
a

## -----------------------------------------------------------------------------
set.seed(0)
M <- 1e4
U <- runif(M)

g <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}

# density function on each subinterval
f <- function(x){
  5*exp(-x)/(1-exp(-1))
}

# inverse of distribution functions
F1_inv <- function(x){
  -log(1-(1-exp(-1))*x/5)
}
F2_inv <- function(x){
  -log(exp(-a[1])-(1-exp(-1))*x/5)
}
F3_inv <- function(x){
  -log(exp(-a[2])-(1-exp(-1))*x/5)
}
F4_inv <- function(x){
  -log(exp(-a[3])-(1-exp(-1))*x/5)
}
F5_inv <- function(x){
  -log(exp(-a[4])-(1-exp(-1))*x/5)
}

# samples generated by inverse transform method
x <- matrix(0, nrow = 2000, ncol = 5)
x[,1] <- F1_inv(U[1:2000])
x[,2] <- F2_inv(U[2001:4000])
x[,3] <- F3_inv(U[4001:6000])
x[,4] <- F4_inv(U[6001:8000])
x[,5] <- F5_inv(U[8001:10000])

# estimator of mean and variance on each subinterval
theta.hat <- numeric(5)
sigma2.hat <- numeric(5)
for(i in 1:5){
  theta.hat[i] <- mean(g(x[,i])/f(x[,i]))
  sigma2.hat[i] <- var(g(x[,i])/f(x[,i]))
}

# show the result
theta <- sum(theta.hat)
sigma2 <- sum(sigma2.hat)
se <- sqrt(sigma2)
cbind(theta, sigma2,se)

## -----------------------------------------------------------------------------
# sample generation function
sample_gen <- function(n, mu=0, sigma=1){
  x <- rlnorm(n=n, meanlog = mu, sdlog = sigma)
  return(x)
}

# data analysis function (constuct a confidence interval with level alpha)
CI <- function(x, alpha=0.05){
  n <- length(x)
  y <- log(x)
  mu.hat <- mean(y)
  sigma2.hat <- var(y)
  lower <- mu.hat+qt(alpha/2,df=n-1)*sqrt(sigma2.hat/n)
  upper <- mu.hat+qt(1-alpha/2,df=n-1)*sqrt(sigma2.hat/n)
  return(c("lower.bound"=lower,"upper.bound"=upper))
}

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
lower <- upper <- numeric(m)

for(i in 1:m){
  Sample <- sample_gen(n=10, mu=0, sigma=1)
  lower[i] <- CI(x=Sample)[1]
  upper[i] <- CI(x=Sample)[2]
}

CP <- mean((lower<0)&(upper>0))
cat("CP =",CP)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
# The functions of "Count Five" test is copied from the book
maxout <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(outx, outy)))
}

count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}

F.test <- function(x, y, alpha=0.05){
  S1 <- var(x)
  S2 <- var(y)
  m <- length(x)
  n <- length(y)
  f <- S2/S1
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(f>qf(1-alpha/2,df1 = n-1,df2 = m-1)||
                           f<qf(alpha/2,df1 = n-1,df2 = m-1)))
}

## -----------------------------------------------------------------------------
power_count5test <- function(m, n1, n2, sigma1, sigma2){
  mean(replicate(m, expr={
    x <- rnorm(n1, 0, sigma1)
    y <- rnorm(n2, 0, sigma2)
    count5test(x, y)
  }))
}

power_F.test <- function(m, n1, n2, sigma1, sigma2){
  mean(replicate(m, expr = {
    x <- rnorm(n1, 0, sigma1)
    y <- rnorm(n2, 0, sigma2)
    F.test(x, y, alpha = 0.055)
  }))
}

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
# generate samples under H1 to estimate power
sigma1 <- 1
sigma2 <- 1.5
result1 <- numeric(3)
result2 <- numeric(3)
n <- c(20,100,1000)

for(i in 1:3){
  result1[i] <- power_count5test(m, n1=n[i], n2=n[i], sigma1, sigma2)
  result2[i] <- power_F.test(m, n1=n[i], n2=n[i], sigma1, sigma2)
}


pander::pander(data.frame("size"=c(20,100,200),"count five test"=result1,
                          "F test"=result2))

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
library(boot)
lambda <- 1/mean(aircondit$hours) # MLE of lambda
# bootstrap estimates
set.seed(0)
B <- 1e4
lambdastar <- numeric(B)
for(b in 1:B){
xstar <- sample(aircondit$hours,replace=TRUE)
lambdastar[b] <- 1/mean(xstar)
}

pander::pander(round(c(MLE=lambda,bias=mean(lambdastar)-lambda,
                       se.boot=sd(lambdastar)),4))

## -----------------------------------------------------------------------------
library(boot)
boot.mean <- function(x,i) mean(x[i])
set.seed(0)
de <- boot(data=aircondit$hours,statistic=boot.mean, R = 999)
ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
ci
cat("MLE=",ci$t0)

## -----------------------------------------------------------------------------
hist(de$t)
abline(v=de$t0,col='red',lwd=2)
# lower alpha/2 quantile
abline(v=sort(de$t)[25],col='green',lwd=2)
# upper alpha/2 quantile
abline(v=sort(de$t)[975],col='green',lwd=2)

## -----------------------------------------------------------------------------
# compute the length of the confidence intervals
interval.length <- c(ci$normal[3]-ci$normal[2],ci$basic[5]-ci$basic[4],
                  ci$percent[5]-ci$percent[4],ci$bca[5]-ci$bca[4])
names(interval.length) <- c("normal","basic","percentile","BCa")
pander::pander(interval.length)

## -----------------------------------------------------------------------------
library(boot)
n <- 2e1
m <- 1e2
set.seed(1)
boot.mean <- function(x,i) mean(x[i])
ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)
# set the real parameter
mu <- 0
sigma <- 1

for(i in 1:m){
  R <- rnorm(n,mu,sigma)

  de <- boot(data=R,statistic=boot.mean, R = 999)
  ci <- boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,] <- ci$norm[2:3]
  ci.basic[i,] <- ci$basic[4:5]
  ci.perc[i,] <- ci$percent[4:5]
}
# Coverage probability
CP <- c(mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu),
        mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu),
        mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu))
# the proportion of times that the confidence intervals miss on the left
miss.left <- c(mean(ci.norm[,2] < mu),
               mean(ci.basic[,2] < mu),
               mean(ci.perc[,2] < mu))
# the proportion of times that the confidence intervals miss on the right
miss.right <- c(mean(ci.norm[,1] > mu),
                mean(ci.basic[,1] > mu),
                mean(ci.perc[,1] > mu))

pander::pander(data.frame(CP,miss.left,miss.right,
                          row.names = c("normal","basic","percentile")))

## -----------------------------------------------------------------------------
library(bootstrap)

## -----------------------------------------------------------------------------
n <- nrow(scor)
Sigma <- (n-1)*cov(scor)/n
lambda <- eigen(Sigma)$values
theta.hat <- lambda[1]/sum(lambda)
theta.jack <- numeric(n)

for(i in 1:n){
  lambda.jack <- eigen((n-2)*cov(scor[-i,])/(n-1))$values
  theta.jack[i] <- lambda.jack[1]/sum(lambda.jack)
}

bias <- (n-1)*(mean(theta.jack)-theta.hat)
sd <- sqrt((n-1)^2*var(theta.jack)/n)
pander::pander(round(c(bias=bias,standard.error=sd),3))

## -----------------------------------------------------------------------------
rm(list = ls())
library(DAAG)

attach(ironslag)
n <- length(magnetic) 
e1 <- e2 <- e3 <- e4 <- matrix(0,nrow = n,ncol = n)

# fit models on leave-two-out samples
for (i in 1:(n-1)) {
  for(j in (i+1):n){
  y <- magnetic[-c(i,j)]
  x <- chemical[-c(i,j)]
  
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[c(i,j)]
  e1[i,j] <- mean((magnetic[c(i,j)] - yhat1)^2)
  
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[c(i,j)] +
  J2$coef[3] * chemical[c(i,j)]^2
  e2[i,j] <- mean((magnetic[c(i,j)] - yhat2)^2)
  
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[c(i,j)]
  yhat3 <- exp(logyhat3)
  e3[i,j] <- mean((magnetic[c(i,j)] - yhat3)^2)
  
  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[c(i,j)])
  yhat4 <- exp(logyhat4)
  e4[i,j] <- mean((magnetic[c(i,j)] - yhat4)^2)
  }
}

round(c(sum(e1)/choose(n,2), sum(e2)/choose(n,2), sum(e3)/choose(n,2), 
        sum(e4)/choose(n,2)),4)

detach(ironslag)

## -----------------------------------------------------------------------------
rm(list = ls())
library(MASS)

set.seed(123)
n <- 20
rho <- 0.4
mu <- c(0,0)
Sigma <- matrix(c(1,rho,rho,1), ncol = 2)
xy <- mvrnorm(n, mu, Sigma)
x <- xy[,1]
y <- xy[,2]
rho.hat <- cor(x,y,method = "spearman")

# apply the permutation test
B <- 1e3
rho.perm <- numeric(B)
for(i in 1:B){
  y.perm <- sample(y, size = n, replace = FALSE)
  rho.perm[i] <- cor(x, y.perm, method = "spearman")
}

# compute p-value of permutation test (two-side)
p.hat <- mean(abs(rho.perm) > abs(rho.hat))
# compute the theoretical p-value of spearman test
p.tilde <- cor.test(x, y, alternative = "two.sided", method = "spearman")$p.value
pander::pander(data.frame(permutation=p.hat, cor.test=p.tilde,row.names = "p.value"))

## -----------------------------------------------------------------------------
f <- function(x){ # the density of standard Laplace distribution
  exp(-abs(x))/2
}

rw.Metropolis <- function(sigma,x0,N){ #random walk sampler
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for(i in 2:N){
    y <- rnorm(1,mean = x[i-1],sd = sigma)
    alpha <- (f(y)*dnorm(x[i-1],mean = y,sd = sigma))/
      (f(x[i-1])*dnorm(y,mean = x[i-1],sd = sigma))
    if(u[i] <= alpha){
      x[i] <- y
    }else{
      x[i] <- x[i-1]
      k <- k+1
    }
  }
  return(list(x=x,k=k,accept.rate=1-k/N))
}

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

n <- 12000
b <- 1000
k <- 4
sgm <- 1:3
accept.rate <- matrix(0,nrow = 3,ncol = k)
r.hat <- matrix(0,nrow = 3,ncol = k)
x0 <- c(-10, -5, 5, 10)
X1 <- X2 <- X3 <- matrix(0,nrow = k,ncol = n)

set.seed(0)
for(i in 1:k){
  chain <- rw.Metropolis(sgm[1],x0[i],n)
  X1[i, ] <- chain$x
  accept.rate[1,i] <- chain$accept.rate
}
for(i in 1:k){
  chain <- rw.Metropolis(sgm[2],x0[i],n)
  X2[i, ] <- chain$x
  accept.rate[2,i] <- chain$accept.rate
}
for(i in 1:k){
  chain <- rw.Metropolis(sgm[3],x0[i],n)
  X3[i, ] <- chain$x
  accept.rate[3,i] <- chain$accept.rate
}

psi1 <- t(apply(X1, 1, cumsum))
for (i in 1:nrow(psi1))
psi1[i,] <- psi1[i,] / (1:ncol(psi1))


psi2 <- t(apply(X2, 1, cumsum))
for (i in 1:nrow(psi2))
psi2[i,] <- psi2[i,] / (1:ncol(psi2))


psi3 <- t(apply(X3, 1, cumsum))
for (i in 1:nrow(psi3))
psi3[i,] <- psi3[i,] / (1:ncol(psi3))

rownames(accept.rate) <- c("sigma.1","sigma.2","sigma.3")
colnames(accept.rate) <- c("chain.1","chain.2","chain.3","chain.4")
pander::pander(accept.rate)

pander::pander(data.frame("sigma.1"=Gelman.Rubin(psi1),
                          "sigma.2"=Gelman.Rubin(psi2),
                          "sigma.3"=Gelman.Rubin(psi3),row.names = "R.hat"))

## -----------------------------------------------------------------------------
#par(mfrow=c(2,2))

rhat1 <- rep(0, n)
for (j in (b+1):n)
rhat1[j] <- Gelman.Rubin(psi1[,1:j])
plot(rhat1[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)

rhat2 <- rep(0, n)
for (j in (b+1):n)
rhat2[j] <- Gelman.Rubin(psi2[,1:j])
plot(rhat2[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)

rhat3 <- rep(0, n)
for (j in (b+1):n)
rhat3[j] <- Gelman.Rubin(psi3[,1:j])
plot(rhat3[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)

## -----------------------------------------------------------------------------
#initialize constants and parameters
N <- 10000 #length of chain
burn <- 1000 #burn-in length
k <- 4
X <- matrix(0, N, 2*k) #the chain, a bivariate sample
rho <- 0.9 #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2
###### generate the chain #####
X[1, ] <- rep(c(-3,-1,1,3),2) #initialize

set.seed(100)
for(j in 1:k){
  for(i in 2:N){
    x2 <- X[i-1, k+j]
    m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
    X[i, j] <- rnorm(1, m1, s1)
    x1 <- X[i, j]
    m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
    X[i, k+j] <- rnorm(1, m2, s2)
  }
}

b <- burn + 1
x <- X[, 1:k]
y <- X[, (k+1):(2*k)]
plot(x[b:N,1],y[b:N,1])
mylm <- lm(y[b:N,1]~x[b:N,1])
coef(mylm)
#par(mfrow=c(2,2))
plot(mylm)

## -----------------------------------------------------------------------------
psi1 <- t(apply(x, 2, cumsum))
for (i in 1:nrow(psi1))
psi1[i,] <- psi1[i,] / (1:ncol(psi1))


psi2 <- t(apply(y, 2, cumsum))
for (i in 1:nrow(psi2))
psi2[i,] <- psi2[i,] / (1:ncol(psi2))

pander::pander(data.frame("x"=Gelman.Rubin(psi1),
                          "y"=Gelman.Rubin(psi2),
                          row.names = "R.hat"))

#par(mfrow=c(1,2))
rhat1 <- rep(0, N)
for (j in (b+1):N)
rhat1[j] <- Gelman.Rubin(psi1[,1:j])
plot(rhat1[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

rhat2 <- rep(0, N)
for (j in (b+1):N)
rhat2[j] <- Gelman.Rubin(psi2[,1:j])
plot(rhat2[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
T.stat <- function(X,M,Y){
  model.m <- lm(M~X)
  model.y <- lm(Y~M+X)
  a.hat <- summary(model.m)$coef[2,1]
  b.hat <- summary(model.y)$coef[2,1]
  sa <- summary(model.m)$coef[2,2]
  sb <- summary(model.y)$coef[2,2]
  T.stat <- (a.hat*b.hat)/sqrt(a.hat^2*sb^2+b.hat^2*sa^2)
  return(T.stat)
}

## -----------------------------------------------------------------------------
type1.err.rate <- numeric(3)
stat <- numeric(3)
n <- 20  # size of samples
N <- 200 # times of simulations
b <- 100 # times of permutation

# Case 1: alpha=0, beta=0

alpha <- 0
beta <- 0

p.perm <- numeric(N)
stat.perm <- numeric(b)

set.seed(1)
X <- rexp(n,rate = 0.3)
em <- rnorm(n)
ey <- rnorm(n)
M <- alpha*X+em
Y <- beta*M+X+ey
stat[1] <- T.stat(X,M,Y)

for(i in 1:N){
  for(j in 1:b){
    M.perm <- sample(M)
    stat.perm[j] <- T.stat(X,M.perm,Y)
  }
  p.perm[i] <- mean(abs(stat.perm)>abs(stat[1]))
}
type1.err.rate[1] <- mean(p.perm<0.05)

# Case 2: alpha=0, beta=1

alpha <- 0
beta <- 1

p.perm <- numeric(N)
stat.perm <- numeric(b)

set.seed(2)
X <- rexp(n,rate = 0.3)
em <- rnorm(n)
ey <- rnorm(n)
M <- alpha*X+em
Y <- beta*M+X+ey
stat[2] <- T.stat(X,M,Y)

for(i in 1:N){
  for(j in 1:b){
    X.perm <- sample(X)
    stat.perm[j] <- T.stat(X.perm,M,Y)
  }
  p.perm[i] <- mean(abs(stat.perm)>abs(stat[2]))
}
type1.err.rate[2] <- mean(p.perm<0.05)

# Case 3: alpha=1, beta=0

alpha <- 1
beta <- 0

p.perm <- numeric(N)
stat.perm <- numeric(b)

set.seed(3)
X <- rexp(n,rate = 0.3)
em <- rnorm(n)
ey <- rnorm(n)
M <- alpha*X+em
Y <- beta*M+X+ey
stat[3] <- T.stat(X,M,Y)

for(i in 1:N){
  for(j in 1:b){
    Y.perm <- sample(Y)
    stat.perm[j] <- T.stat(X,M,Y.perm)
  }
  p.perm[i] <- mean(abs(stat.perm)>abs(stat[3]))
}
type1.err.rate[3] <- mean(p.perm<0.05)

pander::pander(data.frame(alpha=c(0,0,1),beta=c(0,1,0),
                          type1.err.rate=type1.err.rate))

## -----------------------------------------------------------------------------
# (1) write down the R function
myfun <- function(N,b1,b2,b3,f0){
  x1 <- rpois(N,1)
  x2 <- rexp(N,1)
  x3 <- rbinom(N,1,0.5)
  g <- function(alpha){
    tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
    p <- 1/(1+tmp)
    mean(p) - f0
  }
  solution <- uniroot(g,c(-20,0))
  return(solution$root)
}

# (2) set the parameters and apply the function
N <- 1e6
b1 <- 0
b2 <- 1
b3 <- -1
f0 <- c(1e-1,1e-2,1e-3,1e-4)
alpha <- numeric(length(f0))

set.seed(0)
for(i in 1:length(f0)){
  alpha[i] <- myfun(N,b1,b2,b3,f0[i])
}

# (3) report the result
pander::pander(data.frame(f0=f0,alpha=alpha))
plot(alpha,f0,col="black",pch=10)

## -----------------------------------------------------------------------------
u <- c(11,8,27,13,16,0,23,10,24,2)
v <- c(12,9,28,14,17,1,24,11,25,3)

EM <- function(u,v,init,max.iter=1e4){
  lambda0 <- 0
  lambda1 <- init
  n <- length(u)
  k <- 0
  
  while(abs(lambda1-lambda0)>1e-5){
    lambda0 <- lambda1
    lambda1 <- n*(sum((u*exp(-lambda1*u)-v*exp(-lambda1*v))/
                        (exp(-lambda1*u)-exp(-lambda1*v)))+n/lambda1)^(-1)
    k <- k+1
    if(k>max.iter) break
  }
  return(data.frame(lambda=round(lambda1,4),iter=k))
}

MLE <- function(u,v){
  f <- function(lambda){
    sum((-u*exp(-lambda*u)+v*exp(-lambda*v))/(exp(-lambda*u)-exp(-lambda*v)))
  }
  mle <- uniroot(f,c(0,5))$root
  return(mle)
}

pander::pander(EM(u,v,init = 1))
round(MLE(u,v),4)

## -----------------------------------------------------------------------------
a <- list(1:3)
a
is.list(a)
is.vector(a)
unlist(a)

## -----------------------------------------------------------------------------
is.character("1")
is.numeric(1)
is.numeric(-1)
is.logical(FALSE)

## -----------------------------------------------------------------------------
b <- c(1,2,3)
dim(b)

## -----------------------------------------------------------------------------
x <- matrix(0,nrow = 2,ncol = 2)
is.matrix(x)
is.array(x)

## -----------------------------------------------------------------------------
df <- data.frame(
x = 1:3,
y = c("a", "b", "c"),
stringsAsFactors = FALSE)

attributes(df)

## -----------------------------------------------------------------------------
typeof(df$x)
typeof(df$y)
as.matrix(df)

## -----------------------------------------------------------------------------
df1 <- df[FALSE,]
df1
df2 <- df[,FALSE]
df2

## -----------------------------------------------------------------------------
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
df1 <- data.frame(a=1:3,b=2:4)
lapply(df1,scale01)

## -----------------------------------------------------------------------------
df1$c <- c("x","y","z")
id <- unlist(lapply(df1,is.numeric))
lapply(df1[,id==TRUE],scale01)

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(0)
df2 <- data.frame(a=rnorm(5),b=runif(5))
vapply(df2,FUN = sd,FUN.VALUE = 1)

## -----------------------------------------------------------------------------
df2$c <- as.character(1:5)
id <- vapply(df2,is.numeric,FUN.VALUE = TRUE)
vapply(df2[,id==TRUE],FUN = sd,FUN.VALUE = 1)

## -----------------------------------------------------------------------------

gibbsR <- function(N, burn, mu, sigma, rho){
  X <- matrix(0, N, 2) #the chain, a bivariate sample
  mu1 <- mu[1]
  mu2 <- mu[2]
  sigma1 <- sigma[1]
  sigma2 <- sigma[2]
  s1 <- sqrt(1-rho^2)*sigma1
  s2 <- sqrt(1-rho^2)*sigma2
  
  ###### generate the chain #####
  X[1, ] <- c(mu1, mu2) #initialize
  for (i in 2:N) {
    x2 <- X[i-1, 2]
    m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
    X[i, 1] <- rnorm(1, m1, s1)
    x1 <- X[i, 1]
    m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
    X[i, 2] <- rnorm(1, m2, s2)
  }
  b <- burn + 1
  x <- X[b:N, ]
  
  return(list(x=x,X=X))
}

#initialize constants and parameters
N <- 500 #length of chain
burn <- 100 #burn-in length

rho <- 0.9 #correlation
mu1 <- mu2 <- 0
mu <- c(mu1,mu2)
sigma1 <- sigma2 <- 1
sigma <- c(sigma1,sigma2)

## -----------------------------------------------------------------------------
library(Rcpp)
library(microbenchmark)
library(StatComp22099)
#sourceCpp('gibbsC.cpp')

set.seed(0)
gibbR <- gibbsR(N,burn,mu,sigma,rho)$x
gibbC <- gibbsC(N,burn,rho)[(burn+1):N,]
#par(mfrow=c(2,2))
plot(gibbR[,1],gibbR[,2])
plot(gibbC[,1],gibbC[,2])
qqplot(gibbR[,1],gibbC[,1])
qqplot(gibbR[,2],gibbC[,2])

ts <- microbenchmark(gibbR=gibbsR(N,burn,mu,sigma,rho)$x,
gibbC=gibbsC(N,burn,rho))
summary(ts)[,c(1,3,5,6)]

=======
## -----------------------------------------------------------------------------
library(pander)

## -----------------------------------------------------------------------------
pander(head(cars))

## -----------------------------------------------------------------------------
lm.cars <- lm(dist~speed, data = cars)
pander(lm.cars$coefficients)
summary(lm.cars)

## -----------------------------------------------------------------------------
plot(cars)
abline(a=-17.58, b=3.932, col="red")
title(main = "Data point and fit line")
legend("topleft", title = "Legend", c("fit line"), col = c("red"), pch = " ", lty = 1)

## -----------------------------------------------------------------------------
plot(lm.cars)

## -----------------------------------------------------------------------------
library(EnvStats)

## -----------------------------------------------------------------------------
set.seed(0)
# Generate random sample by inverse transform method
U <- runif(100)
X <- 2/sqrt(1-U)

# Graph for comparison, the density of Pareto(2,2) is 8/x^3
d <- seq(0, 10, by=0.1)
hist(X, col = "pink", freq = FALSE, xlim = c(1, 10), ylim = c(0, 0.5))
lines(d, 8/d^3, lwd=3)

## -----------------------------------------------------------------------------
set.seed(0)
n <- 1000
j <- 0
k <- 0
y <- numeric(n)
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- runif(1) #random variate from g(.)
  if (27*x^2*(1-x)/4 > u) {
    #we accept x
    k <- k + 1
    y[k] <- x
  }
}
j # The number of experiments for generating n random samples

# histogram of the sample with the theoretical Beta(3,2) density superimposed
d <- seq(0,1,length.out=100)
hist(y, freq = FALSE, col = "pink", ylim = c(0,2))
lines(d, 12*d^2*(1-d), lwd=3)

# Quantile-quantile plot for ‘rbeta’ and ‘acceptance-rejection’ algorithm.
z <- rbeta(1000, shape1 = 3, shape2 = 2)
qqplot(y, z, xlab = "Acceptance-rejection", ylab = "rbeta")
abline(a=0, b=1, col="red")

## -----------------------------------------------------------------------------
set.seed(0)

beta_ac_rej <- function(a, b, n=1000){
  j <- 0
  k <- 0
  y <- numeric(n)
  c <- -optim(0.5, function(x) -x^(a-1)*(1-x)^(b-1)/beta(a,b), method = "Brent", lower = 0, upper = 1)$value
  while (k < n) {
    u <- runif(1)
    j <- j + 1
    x <- runif(1) #random variate from g(.)
    if (x^(a-1)*(1-x)^(b-1)/(c*beta(a,b)) > u) {
      #we accept x
      k <- k + 1
      y[k] <- x
    }
  }
  result <- list(observation=y, iteration=j)
}

result <- beta_ac_rej(3,2)
result$iteration
d <- seq(0, 1, by=0.01)
y <- result$observation
hist(y, freq = FALSE, col = "pink", ylim = c(0,2))
lines(d, 12*d^2*(1-d), lwd=3)

## -----------------------------------------------------------------------------
n <- 1000
lambda <- rgamma(n, shape = 4, rate = 2)
y <- rexp(n, lambda)

## -----------------------------------------------------------------------------
d <- seq(0,5,by=0.01)
hist(y, freq = FALSE, col = "pink", xlim = c(0,5))
lines(d, y=64/(2+d)^5, lwd=3)

## -----------------------------------------------------------------------------
set.seed(0)
# This part is copied from bb
quick_sort <- function(x){
  num <- length(x)
  if(num==0||num==1){return(x)
  }else{
    a <- x[1]
    y <- x[-1]
    lower <- y[y<a]
    upper <- y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}#form a loop
}


test<-sample(1:1e4)
system.time(quick_sort(test))[1]
test <- quick_sort(test)
# show the result of fast sort algorithm
test[1:10]
test[9991:10000]

## -----------------------------------------------------------------------------
set.seed(0)
n <- c(1e4, 2e4, 4e4, 6e4, 8e4)
computation_time <- function(n){
  t <- numeric(100)
  set.seed(0)
  for(i in 1:100){
    test <- sample(1:n)
    t[i] <- system.time(quick_sort(test))[1]
  }
  t_mean <- mean(t)
  return(t_mean)
}


an <- c(computation_time(n[1]),computation_time(n[2]),computation_time(n[3]),
       computation_time(n[4]),computation_time(n[5]))
an

## -----------------------------------------------------------------------------
tn <- n*log(n)
mylm <- lm(an~tn)
x <- seq(0,1e6,length.out=100)
b <- coefficients(mylm)
plot(tn, an, main="Regression line")
lines(x, b[1]+b[2]*x, col="red")

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
U <- runif(m)
theta1 <- mean(exp(U))                # simple MC estimator
theta2 <- mean((exp(U)+exp(1-U))/2)   # antithetic variables estimator
var1 <- var(exp(U))                   # sample variance of simple MC
var2 <- var((exp(U)+exp(1-U))/2)      # sample variance of antithetic variables
theta1
theta2
100*(var1-var2)/var1      # empirical estimator of percent reduction of variance

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
x <- rnorm(m)

g <- function(x){
  x^2*exp(-x^2/2)/sqrt(2*pi)*(x>1)
}
f1 <- function(x) dnorm(x)

theta.hat1 <- mean(g(x)/f1(x))
var1 <- var(g(x)/f1(x))
cbind(theta.hat1, var1)

## -----------------------------------------------------------------------------
set.seed(0)
y <- rgamma(m,shape = 3,rate = 1)
f2 <- function(x) dgamma(x, shape = 3, rate = 1)

theta.hat2 <- mean(g(y)/f2(y))
var2 <- var(g(y)/f2(y))
cbind(theta.hat2, var2)

## -----------------------------------------------------------------------------
d <- seq(1, 5, 0.05)
gs <- c(expression(g(x)==x^2*e^{-x^2/2}/sqrt(2*pi)),
        expression(f[1](x)==e^{-x^2/2}/sqrt(2*pi)),
        expression(f[2](x)==x^2*e^{-x}/2))
par(mfrow=c(1,2))
#figure (a)
plot(d, g(d), type = "l", ylab = "", ylim = c(0,0.5),
     lwd = 2,col=1,main='(A)')
lines(d, f1(d), lty = 2, lwd = 2,col=2)
lines(d, f2(d), lty = 3, lwd = 2,col=3)
legend("topright", legend = gs, lty = 1:3,
       lwd = 2, inset = 0.02,col=1:3)
#figure (b)
plot(d, g(d)/f1(d), type = "l", ylab = "", ylim = c(0,3),
     lwd = 2, lty = 2, col = 2, main = "(B)")
lines(d, g(d)/f2(d), lty = 3, lwd = 2, col = 3)
legend("topright", legend = gs[-1], lty = 2:3, lwd = 2,
       inset = 0.02, col = 2:3)

## -----------------------------------------------------------------------------
a <- numeric(5)
a[1] <- -log(0.8+exp(-1)/5)
a[5] <- 1
for(i in 2:4){
  a[i] <- -log(exp(-a[i-1])-0.2+exp(-1)/5)
}
a

## -----------------------------------------------------------------------------
set.seed(0)
M <- 1e4
U <- runif(M)

g <- function(x) {
  exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}

# density function on each subinterval
f <- function(x){
  5*exp(-x)/(1-exp(-1))
}

# inverse of distribution functions
F1_inv <- function(x){
  -log(1-(1-exp(-1))*x/5)
}
F2_inv <- function(x){
  -log(exp(-a[1])-(1-exp(-1))*x/5)
}
F3_inv <- function(x){
  -log(exp(-a[2])-(1-exp(-1))*x/5)
}
F4_inv <- function(x){
  -log(exp(-a[3])-(1-exp(-1))*x/5)
}
F5_inv <- function(x){
  -log(exp(-a[4])-(1-exp(-1))*x/5)
}

# samples generated by inverse transform method
x <- matrix(0, nrow = 2000, ncol = 5)
x[,1] <- F1_inv(U[1:2000])
x[,2] <- F2_inv(U[2001:4000])
x[,3] <- F3_inv(U[4001:6000])
x[,4] <- F4_inv(U[6001:8000])
x[,5] <- F5_inv(U[8001:10000])

# estimator of mean and variance on each subinterval
theta.hat <- numeric(5)
sigma2.hat <- numeric(5)
for(i in 1:5){
  theta.hat[i] <- mean(g(x[,i])/f(x[,i]))
  sigma2.hat[i] <- var(g(x[,i])/f(x[,i]))
}

# show the result
theta <- sum(theta.hat)
sigma2 <- sum(sigma2.hat)
se <- sqrt(sigma2)
cbind(theta, sigma2,se)

## -----------------------------------------------------------------------------
# sample generation function
sample_gen <- function(n, mu=0, sigma=1){
  x <- rlnorm(n=n, meanlog = mu, sdlog = sigma)
  return(x)
}

# data analysis function (constuct a confidence interval with level alpha)
CI <- function(x, alpha=0.05){
  n <- length(x)
  y <- log(x)
  mu.hat <- mean(y)
  sigma2.hat <- var(y)
  lower <- mu.hat+qt(alpha/2,df=n-1)*sqrt(sigma2.hat/n)
  upper <- mu.hat+qt(1-alpha/2,df=n-1)*sqrt(sigma2.hat/n)
  return(c("lower.bound"=lower,"upper.bound"=upper))
}

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
lower <- upper <- numeric(m)

for(i in 1:m){
  Sample <- sample_gen(n=10, mu=0, sigma=1)
  lower[i] <- CI(x=Sample)[1]
  upper[i] <- CI(x=Sample)[2]
}

CP <- mean((lower<0)&(upper>0))
cat("CP =",CP)

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
# The functions of "Count Five" test is copied from the book
maxout <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(outx, outy)))
}

count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}

F.test <- function(x, y, alpha=0.05){
  S1 <- var(x)
  S2 <- var(y)
  m <- length(x)
  n <- length(y)
  f <- S2/S1
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(f>qf(1-alpha/2,df1 = n-1,df2 = m-1)||
                           f<qf(alpha/2,df1 = n-1,df2 = m-1)))
}

## -----------------------------------------------------------------------------
power_count5test <- function(m, n1, n2, sigma1, sigma2){
  mean(replicate(m, expr={
    x <- rnorm(n1, 0, sigma1)
    y <- rnorm(n2, 0, sigma2)
    count5test(x, y)
  }))
}

power_F.test <- function(m, n1, n2, sigma1, sigma2){
  mean(replicate(m, expr = {
    x <- rnorm(n1, 0, sigma1)
    y <- rnorm(n2, 0, sigma2)
    F.test(x, y, alpha = 0.055)
  }))
}

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
# generate samples under H1 to estimate power
sigma1 <- 1
sigma2 <- 1.5
result1 <- numeric(3)
result2 <- numeric(3)
n <- c(20,100,1000)

for(i in 1:3){
  result1[i] <- power_count5test(m, n1=n[i], n2=n[i], sigma1, sigma2)
  result2[i] <- power_F.test(m, n1=n[i], n2=n[i], sigma1, sigma2)
}


pander::pander(data.frame("size"=c(20,100,200),"count five test"=result1,
                          "F test"=result2))

## -----------------------------------------------------------------------------
rm(list = ls())

## -----------------------------------------------------------------------------
library(boot)
lambda <- 1/mean(aircondit$hours) # MLE of lambda
# bootstrap estimates
set.seed(0)
B <- 1e4
lambdastar <- numeric(B)
for(b in 1:B){
xstar <- sample(aircondit$hours,replace=TRUE)
lambdastar[b] <- 1/mean(xstar)
}

pander::pander(round(c(MLE=lambda,bias=mean(lambdastar)-lambda,
                       se.boot=sd(lambdastar)),4))

## -----------------------------------------------------------------------------
library(boot)
boot.mean <- function(x,i) mean(x[i])
set.seed(0)
de <- boot(data=aircondit$hours,statistic=boot.mean, R = 999)
ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
ci
cat("MLE=",ci$t0)

## -----------------------------------------------------------------------------
hist(de$t)
abline(v=de$t0,col='red',lwd=2)
# lower alpha/2 quantile
abline(v=sort(de$t)[25],col='green',lwd=2)
# upper alpha/2 quantile
abline(v=sort(de$t)[975],col='green',lwd=2)

## -----------------------------------------------------------------------------
# compute the length of the confidence intervals
interval.length <- c(ci$normal[3]-ci$normal[2],ci$basic[5]-ci$basic[4],
                  ci$percent[5]-ci$percent[4],ci$bca[5]-ci$bca[4])
names(interval.length) <- c("normal","basic","percentile","BCa")
pander::pander(interval.length)

## -----------------------------------------------------------------------------
library(boot)
n <- 2e1
m <- 1e2
set.seed(1)
boot.mean <- function(x,i) mean(x[i])
ci.norm <- ci.basic <- ci.perc <- matrix(NA,m,2)
# set the real parameter
mu <- 0
sigma <- 1

for(i in 1:m){
  R <- rnorm(n,mu,sigma)

  de <- boot(data=R,statistic=boot.mean, R = 999)
  ci <- boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,] <- ci$norm[2:3]
  ci.basic[i,] <- ci$basic[4:5]
  ci.perc[i,] <- ci$percent[4:5]
}
# Coverage probability
CP <- c(mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu),
        mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu),
        mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu))
# the proportion of times that the confidence intervals miss on the left
miss.left <- c(mean(ci.norm[,2] < mu),
               mean(ci.basic[,2] < mu),
               mean(ci.perc[,2] < mu))
# the proportion of times that the confidence intervals miss on the right
miss.right <- c(mean(ci.norm[,1] > mu),
                mean(ci.basic[,1] > mu),
                mean(ci.perc[,1] > mu))

pander::pander(data.frame(CP,miss.left,miss.right,
                          row.names = c("normal","basic","percentile")))

## -----------------------------------------------------------------------------
library(bootstrap)

## -----------------------------------------------------------------------------
n <- nrow(scor)
Sigma <- (n-1)*cov(scor)/n
lambda <- eigen(Sigma)$values
theta.hat <- lambda[1]/sum(lambda)
theta.jack <- numeric(n)

for(i in 1:n){
  lambda.jack <- eigen((n-2)*cov(scor[-i,])/(n-1))$values
  theta.jack[i] <- lambda.jack[1]/sum(lambda.jack)
}

bias <- (n-1)*(mean(theta.jack)-theta.hat)
sd <- sqrt((n-1)^2*var(theta.jack)/n)
pander::pander(round(c(bias=bias,standard.error=sd),3))

## -----------------------------------------------------------------------------
rm(list = ls())
library(DAAG)

attach(ironslag)
n <- length(magnetic) 
e1 <- e2 <- e3 <- e4 <- matrix(0,nrow = n,ncol = n)

# fit models on leave-two-out samples
for (i in 1:(n-1)) {
  for(j in (i+1):n){
  y <- magnetic[-c(i,j)]
  x <- chemical[-c(i,j)]
  
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[c(i,j)]
  e1[i,j] <- mean((magnetic[c(i,j)] - yhat1)^2)
  
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[c(i,j)] +
  J2$coef[3] * chemical[c(i,j)]^2
  e2[i,j] <- mean((magnetic[c(i,j)] - yhat2)^2)
  
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[c(i,j)]
  yhat3 <- exp(logyhat3)
  e3[i,j] <- mean((magnetic[c(i,j)] - yhat3)^2)
  
  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[c(i,j)])
  yhat4 <- exp(logyhat4)
  e4[i,j] <- mean((magnetic[c(i,j)] - yhat4)^2)
  }
}

round(c(sum(e1)/choose(n,2), sum(e2)/choose(n,2), sum(e3)/choose(n,2), 
        sum(e4)/choose(n,2)),4)

detach(ironslag)

## -----------------------------------------------------------------------------
rm(list = ls())
library(MASS)

set.seed(123)
n <- 20
rho <- 0.4
mu <- c(0,0)
Sigma <- matrix(c(1,rho,rho,1), ncol = 2)
xy <- mvrnorm(n, mu, Sigma)
x <- xy[,1]
y <- xy[,2]
rho.hat <- cor(x,y,method = "spearman")

# apply the permutation test
B <- 1e3
rho.perm <- numeric(B)
for(i in 1:B){
  y.perm <- sample(y, size = n, replace = FALSE)
  rho.perm[i] <- cor(x, y.perm, method = "spearman")
}

# compute p-value of permutation test (two-side)
p.hat <- mean(abs(rho.perm) > abs(rho.hat))
# compute the theoretical p-value of spearman test
p.tilde <- cor.test(x, y, alternative = "two.sided", method = "spearman")$p.value
pander::pander(data.frame(permutation=p.hat, cor.test=p.tilde,row.names = "p.value"))

## -----------------------------------------------------------------------------
f <- function(x){ # the density of standard Laplace distribution
  exp(-abs(x))/2
}

rw.Metropolis <- function(sigma,x0,N){ #random walk sampler
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for(i in 2:N){
    y <- rnorm(1,mean = x[i-1],sd = sigma)
    alpha <- (f(y)*dnorm(x[i-1],mean = y,sd = sigma))/
      (f(x[i-1])*dnorm(y,mean = x[i-1],sd = sigma))
    if(u[i] <= alpha){
      x[i] <- y
    }else{
      x[i] <- x[i-1]
      k <- k+1
    }
  }
  return(list(x=x,k=k,accept.rate=1-k/N))
}

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

n <- 12000
b <- 1000
k <- 4
sgm <- 1:3
accept.rate <- matrix(0,nrow = 3,ncol = k)
r.hat <- matrix(0,nrow = 3,ncol = k)
x0 <- c(-10, -5, 5, 10)
X1 <- X2 <- X3 <- matrix(0,nrow = k,ncol = n)

set.seed(0)
for(i in 1:k){
  chain <- rw.Metropolis(sgm[1],x0[i],n)
  X1[i, ] <- chain$x
  accept.rate[1,i] <- chain$accept.rate
}
for(i in 1:k){
  chain <- rw.Metropolis(sgm[2],x0[i],n)
  X2[i, ] <- chain$x
  accept.rate[2,i] <- chain$accept.rate
}
for(i in 1:k){
  chain <- rw.Metropolis(sgm[3],x0[i],n)
  X3[i, ] <- chain$x
  accept.rate[3,i] <- chain$accept.rate
}

psi1 <- t(apply(X1, 1, cumsum))
for (i in 1:nrow(psi1))
psi1[i,] <- psi1[i,] / (1:ncol(psi1))


psi2 <- t(apply(X2, 1, cumsum))
for (i in 1:nrow(psi2))
psi2[i,] <- psi2[i,] / (1:ncol(psi2))


psi3 <- t(apply(X3, 1, cumsum))
for (i in 1:nrow(psi3))
psi3[i,] <- psi3[i,] / (1:ncol(psi3))

rownames(accept.rate) <- c("sigma.1","sigma.2","sigma.3")
colnames(accept.rate) <- c("chain.1","chain.2","chain.3","chain.4")
pander::pander(accept.rate)

pander::pander(data.frame("sigma.1"=Gelman.Rubin(psi1),
                          "sigma.2"=Gelman.Rubin(psi2),
                          "sigma.3"=Gelman.Rubin(psi3),row.names = "R.hat"))

## -----------------------------------------------------------------------------
#par(mfrow=c(2,2))

rhat1 <- rep(0, n)
for (j in (b+1):n)
rhat1[j] <- Gelman.Rubin(psi1[,1:j])
plot(rhat1[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)

rhat2 <- rep(0, n)
for (j in (b+1):n)
rhat2[j] <- Gelman.Rubin(psi2[,1:j])
plot(rhat2[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)

rhat3 <- rep(0, n)
for (j in (b+1):n)
rhat3[j] <- Gelman.Rubin(psi3[,1:j])
plot(rhat3[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)

## -----------------------------------------------------------------------------
#initialize constants and parameters
N <- 10000 #length of chain
burn <- 1000 #burn-in length
k <- 4
X <- matrix(0, N, 2*k) #the chain, a bivariate sample
rho <- 0.9 #correlation
mu1 <- 0
mu2 <- 0
sigma1 <- 1
sigma2 <- 1
s1 <- sqrt(1-rho^2)*sigma1
s2 <- sqrt(1-rho^2)*sigma2
###### generate the chain #####
X[1, ] <- rep(c(-3,-1,1,3),2) #initialize

set.seed(100)
for(j in 1:k){
  for(i in 2:N){
    x2 <- X[i-1, k+j]
    m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
    X[i, j] <- rnorm(1, m1, s1)
    x1 <- X[i, j]
    m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
    X[i, k+j] <- rnorm(1, m2, s2)
  }
}

b <- burn + 1
x <- X[, 1:k]
y <- X[, (k+1):(2*k)]
plot(x[b:N,1],y[b:N,1])
mylm <- lm(y[b:N,1]~x[b:N,1])
coef(mylm)
#par(mfrow=c(2,2))
plot(mylm)

## -----------------------------------------------------------------------------
psi1 <- t(apply(x, 2, cumsum))
for (i in 1:nrow(psi1))
psi1[i,] <- psi1[i,] / (1:ncol(psi1))


psi2 <- t(apply(y, 2, cumsum))
for (i in 1:nrow(psi2))
psi2[i,] <- psi2[i,] / (1:ncol(psi2))

pander::pander(data.frame("x"=Gelman.Rubin(psi1),
                          "y"=Gelman.Rubin(psi2),
                          row.names = "R.hat"))

#par(mfrow=c(1,2))
rhat1 <- rep(0, N)
for (j in (b+1):N)
rhat1[j] <- Gelman.Rubin(psi1[,1:j])
plot(rhat1[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

rhat2 <- rep(0, N)
for (j in (b+1):N)
rhat2[j] <- Gelman.Rubin(psi2[,1:j])
plot(rhat2[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
T.stat <- function(X,M,Y){
  model.m <- lm(M~X)
  model.y <- lm(Y~M+X)
  a.hat <- summary(model.m)$coef[2,1]
  b.hat <- summary(model.y)$coef[2,1]
  sa <- summary(model.m)$coef[2,2]
  sb <- summary(model.y)$coef[2,2]
  T.stat <- (a.hat*b.hat)/sqrt(a.hat^2*sb^2+b.hat^2*sa^2)
  return(T.stat)
}

## -----------------------------------------------------------------------------
type1.err.rate <- numeric(3)
stat <- numeric(3)
n <- 20  # size of samples
N <- 200 # times of simulations
b <- 100 # times of permutation

# Case 1: alpha=0, beta=0

alpha <- 0
beta <- 0

p.perm <- numeric(N)
stat.perm <- numeric(b)

set.seed(1)
X <- rexp(n,rate = 0.3)
em <- rnorm(n)
ey <- rnorm(n)
M <- alpha*X+em
Y <- beta*M+X+ey
stat[1] <- T.stat(X,M,Y)

for(i in 1:N){
  for(j in 1:b){
    M.perm <- sample(M)
    stat.perm[j] <- T.stat(X,M.perm,Y)
  }
  p.perm[i] <- mean(abs(stat.perm)>abs(stat[1]))
}
type1.err.rate[1] <- mean(p.perm<0.05)

# Case 2: alpha=0, beta=1

alpha <- 0
beta <- 1

p.perm <- numeric(N)
stat.perm <- numeric(b)

set.seed(2)
X <- rexp(n,rate = 0.3)
em <- rnorm(n)
ey <- rnorm(n)
M <- alpha*X+em
Y <- beta*M+X+ey
stat[2] <- T.stat(X,M,Y)

for(i in 1:N){
  for(j in 1:b){
    X.perm <- sample(X)
    stat.perm[j] <- T.stat(X.perm,M,Y)
  }
  p.perm[i] <- mean(abs(stat.perm)>abs(stat[2]))
}
type1.err.rate[2] <- mean(p.perm<0.05)

# Case 3: alpha=1, beta=0

alpha <- 1
beta <- 0

p.perm <- numeric(N)
stat.perm <- numeric(b)

set.seed(3)
X <- rexp(n,rate = 0.3)
em <- rnorm(n)
ey <- rnorm(n)
M <- alpha*X+em
Y <- beta*M+X+ey
stat[3] <- T.stat(X,M,Y)

for(i in 1:N){
  for(j in 1:b){
    Y.perm <- sample(Y)
    stat.perm[j] <- T.stat(X,M,Y.perm)
  }
  p.perm[i] <- mean(abs(stat.perm)>abs(stat[3]))
}
type1.err.rate[3] <- mean(p.perm<0.05)

pander::pander(data.frame(alpha=c(0,0,1),beta=c(0,1,0),
                          type1.err.rate=type1.err.rate))

## -----------------------------------------------------------------------------
# (1) write down the R function
myfun <- function(N,b1,b2,b3,f0){
  x1 <- rpois(N,1)
  x2 <- rexp(N,1)
  x3 <- rbinom(N,1,0.5)
  g <- function(alpha){
    tmp <- exp(-alpha-b1*x1-b2*x2-b3*x3)
    p <- 1/(1+tmp)
    mean(p) - f0
  }
  solution <- uniroot(g,c(-20,0))
  return(solution$root)
}

# (2) set the parameters and apply the function
N <- 1e6
b1 <- 0
b2 <- 1
b3 <- -1
f0 <- c(1e-1,1e-2,1e-3,1e-4)
alpha <- numeric(length(f0))

set.seed(0)
for(i in 1:length(f0)){
  alpha[i] <- myfun(N,b1,b2,b3,f0[i])
}

# (3) report the result
pander::pander(data.frame(f0=f0,alpha=alpha))
plot(alpha,f0,col="black",pch=10)

## -----------------------------------------------------------------------------
u <- c(11,8,27,13,16,0,23,10,24,2)
v <- c(12,9,28,14,17,1,24,11,25,3)

EM <- function(u,v,init,max.iter=1e4){
  lambda0 <- 0
  lambda1 <- init
  n <- length(u)
  k <- 0
  
  while(abs(lambda1-lambda0)>1e-5){
    lambda0 <- lambda1
    lambda1 <- n*(sum((u*exp(-lambda1*u)-v*exp(-lambda1*v))/
                        (exp(-lambda1*u)-exp(-lambda1*v)))+n/lambda1)^(-1)
    k <- k+1
    if(k>max.iter) break
  }
  return(data.frame(lambda=round(lambda1,4),iter=k))
}

MLE <- function(u,v){
  f <- function(lambda){
    sum((-u*exp(-lambda*u)+v*exp(-lambda*v))/(exp(-lambda*u)-exp(-lambda*v)))
  }
  mle <- uniroot(f,c(0,5))$root
  return(mle)
}

pander::pander(EM(u,v,init = 1))
round(MLE(u,v),4)

## -----------------------------------------------------------------------------
a <- list(1:3)
a
is.list(a)
is.vector(a)
unlist(a)

## -----------------------------------------------------------------------------
is.character("1")
is.numeric(1)
is.numeric(-1)
is.logical(FALSE)

## -----------------------------------------------------------------------------
b <- c(1,2,3)
dim(b)

## -----------------------------------------------------------------------------
x <- matrix(0,nrow = 2,ncol = 2)
is.matrix(x)
is.array(x)

## -----------------------------------------------------------------------------
df <- data.frame(
x = 1:3,
y = c("a", "b", "c"),
stringsAsFactors = FALSE)

attributes(df)

## -----------------------------------------------------------------------------
typeof(df$x)
typeof(df$y)
as.matrix(df)

## -----------------------------------------------------------------------------
df1 <- df[FALSE,]
df1
df2 <- df[,FALSE]
df2

## -----------------------------------------------------------------------------
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
df1 <- data.frame(a=1:3,b=2:4)
lapply(df1,scale01)

## -----------------------------------------------------------------------------
df1$c <- c("x","y","z")
id <- unlist(lapply(df1,is.numeric))
lapply(df1[,id==TRUE],scale01)

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(0)
df2 <- data.frame(a=rnorm(5),b=runif(5))
vapply(df2,FUN = sd,FUN.VALUE = 1)

## -----------------------------------------------------------------------------
df2$c <- as.character(1:5)
id <- vapply(df2,is.numeric,FUN.VALUE = TRUE)
vapply(df2[,id==TRUE],FUN = sd,FUN.VALUE = 1)

## -----------------------------------------------------------------------------

gibbsR <- function(N, burn, mu, sigma, rho){
  X <- matrix(0, N, 2) #the chain, a bivariate sample
  mu1 <- mu[1]
  mu2 <- mu[2]
  sigma1 <- sigma[1]
  sigma2 <- sigma[2]
  s1 <- sqrt(1-rho^2)*sigma1
  s2 <- sqrt(1-rho^2)*sigma2
  
  ###### generate the chain #####
  X[1, ] <- c(mu1, mu2) #initialize
  for (i in 2:N) {
    x2 <- X[i-1, 2]
    m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
    X[i, 1] <- rnorm(1, m1, s1)
    x1 <- X[i, 1]
    m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
    X[i, 2] <- rnorm(1, m2, s2)
  }
  b <- burn + 1
  x <- X[b:N, ]
  
  return(list(x=x,X=X))
}

#initialize constants and parameters
N <- 500 #length of chain
burn <- 100 #burn-in length

rho <- 0.9 #correlation
mu1 <- mu2 <- 0
mu <- c(mu1,mu2)
sigma1 <- sigma2 <- 1
sigma <- c(sigma1,sigma2)

## -----------------------------------------------------------------------------
library(Rcpp)
library(microbenchmark)
library(StatComp22099)
#sourceCpp('gibbsC.cpp')

set.seed(0)
gibbR <- gibbsR(N,burn,mu,sigma,rho)$x
gibbC <- gibbsC(N,burn,rho)[(burn+1):N,]
#par(mfrow=c(2,2))
plot(gibbR[,1],gibbR[,2])
plot(gibbC[,1],gibbC[,2])
qqplot(gibbR[,1],gibbC[,1])
qqplot(gibbR[,2],gibbC[,2])

ts <- microbenchmark(gibbR=gibbsR(N,burn,mu,sigma,rho)$x,
gibbC=gibbsC(N,burn,rho))
summary(ts)[,c(1,3,5,6)]

>>>>>>> 61e88b7412a19960e2ed8dc2271020f3ea8c2749
