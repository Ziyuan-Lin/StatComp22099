#' @title Fitting Sparse Reduced-Rank Regression Model
#' @description Estimate the coefficient matrix C by subgradient method.
#' @param Y response matrix.
#' @param X prediction matrix.
#' @param lambda numeric scaling parameter for tuning, with default value 0.5.
#' @param r the rank of matrix C.
#' @param method a string indicating which method to utilize, where "sub" corresponds to subgradient (default) method. Up to now, this is the only algorithm we have developed, so do not modify the default setting!.
#' @param weight a numeric vector of weights of each row of matrix B in the penalty term.
#' @return A list of matrices and iteration. Here we obtain coefficient matrix C and its decomposition matrix B and A.
#' @useDynLib StatComp22099
#' @export
SRRR <- function(Y, X, lambda=0.5, r, method="sub", weight){
  n <- nrow(X)
  p <- ncol(X)
  q <- ncol(Y)
  Sxx <- t(X)%*%X/n
  Sxy <- t(X)%*%Y/n
  Syx <- t(Y)%*%X/n
  V <- eigen(Syx%*%solve(Sxx)%*%Sxy,symmetric = TRUE)$vector[,1:r]
  A <- V
  B <- solve(Sxx)%*%Sxy%*%V
  
  fit <- SRRR_cpp(Y, X, method, B, A, r, lambda, conv=1e-4, miter=1e4,
                  inner_conv=1e-3, inner_iter=5e3, WA=weight)
  conv_flag <- fit$conv_flag
  if(!conv_flag){
    print("Warning: not converge")
  }
  
  return(list(B=fit$A, A=fit$V, C=fit$C, iter=fit$iter))
}

#' @title Generating Sample for Sparse Reduced-Rank Regression
#' @description Generating samples from multinormal distribution for simulations tesing algorithms of Sparse Reduced-Rank Regression.
#' @param n sample size.
#' @param p column number of prediction matrix X.
#' @param q column number of response matrix Y.
#' @param p0 number of non-zero rows of matrix B.
#' @param r rank of coefficient matrix C.
#' @param rho.x correlation coefficient of each two columns of prediction matrix X.
#' @param rho.e correlation coefficient of each two columns of noise matrix E.
#' @return A list of matrix for the SRRR model, serving as the real model.
#' @importFrom MASS mvrnorm
#' @importFrom stats rnorm
#' @export
sample.generator <- function(n, p, p0, q, r, rho.x, rho.e){
  X <- matrix(0, nrow = n, ncol = p)
  Y <- E <- matrix(0, nrow = n, ncol = q)
  B <- matrix(c(rnorm(p0*r),rep(0,(p-p0)*r)), nrow = p, ncol = r, byrow = TRUE)
  A <- matrix(rnorm(q*r), nrow = q, ncol = r)
  C <- B%*%t(A)
  
  sigma.x <- matrix(rho.x, nrow = p, ncol = p)
  sigma.x <- sigma.x+diag(1-rho.x, p)
  X <- mvrnorm(n, mu = rep(0,p), Sigma = sigma.x)
  
  sigma.e <- matrix(rho.e, nrow = q, ncol = q)
  sigma.e <- sigma.e+diag(1-rho.e, q)
  sgm2 <- sum(diag(t(C)%*%sigma.x%*%C))/q
  E <- mvrnorm(n, mu = rep(0,q), Sigma = sgm2*sigma.e)
  
  Y <- X%*%C+E
  
  return(list(X=X,Y=Y,E=E,C=C,B=B,A=A))
}


#' @title Simulation of Sparse Reduced-Rank Regression
#' @description Users may set the size and rank of matrix, namely n,p,q,p0 and r, the correlation coefficients rho.x and rho.e, then the function may compare the results of our function SRRR and function srrr in package "rrpack", which includes mean square error, standard error and computation time.
#' @param n,p,q,p0,r,rho.x,rho.e parameters for function "sample.generator"
#' @param runs the time we repeat the simulation in order to obtain mean square error, standard error and computation time.
#' @return A list including mean square error, standard error and computation time for functions SRRR and rrpack::srrr, where MSE1, SE1, Time1 correspond to rrpack::srrr, and MSE2, SE2, Time2 correspond to SRRR.
#' @import rrpack
#' @import microbenchmark
#' @importFrom stats sd
#' @useDynLib StatComp22099
#' @export
SRRR.sim <- function(n, p, q, p0, r, rho.x, rho.e, runs=30){
  mse1 <- mse2 <- time1 <- time2 <- numeric(runs)
  for(i in 1:runs){
    Sample <- sample.generator(n,p,p0,q,r,rho.x,rho.e)
    X <- Sample$X
    Y <- Sample$Y
    C <- Sample$C
    res1 <- srrr(Y, X, nrank = r, method = "adglasso")
    C1 <- res1$U %*% res1$D %*% t(res1$V)
    res2 <- SRRR(Y, X, r=r, weight = rep(1, p))
    C2 <- res2$C
    mse1[i] <- norm(C-C1,type = "F")^2/(n*q)
    mse2[i] <- norm(C-C2,type = "F")^2/(n*q)
    time1[i] <- summary(microbenchmark(srrr(Y, X, nrank = r, method = "adglasso")))$mean
    time2[i] <- summary(microbenchmark(SRRR(Y, X, r=r, weight = rep(1, p))))$mean
  }
  MSE1 <- mean(mse1)
  SE1 <- sd(mse1)
  MSE2 <- mean(mse2)
  SE2 <- sd(mse2)
  Time1 <- mean(time1)
  Time2 <- mean(time2)
  return(list(MSE1=MSE1, SE1=SE1, MSE2=MSE2, SE2=SE2, Time1=Time1, Time2=Time2))
}