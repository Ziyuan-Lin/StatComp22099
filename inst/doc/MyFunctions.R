## ----setup--------------------------------------------------------------------
library(StatComp22099)
library(rrpack)
library(microbenchmark)
library(MASS)

## ---- eval=FALSE--------------------------------------------------------------
#  
#  
#  SRRR <- function(Y, X, lambda=0.5, r, method="sub", weight){
#    n <- nrow(X)
#    p <- ncol(X)
#    q <- ncol(Y)
#    Sxx <- t(X)%*%X/n
#    Sxy <- t(X)%*%Y/n
#    Syx <- t(Y)%*%X/n
#    V <- eigen(Syx%*%solve(Sxx)%*%Sxy,symmetric = TRUE)$vector[,1:r]
#    A <- V
#    B <- solve(Sxx)%*%Sxy%*%V
#  
#    fit <- SRRR_cpp(Y, X, method, B, A, r, lambda, conv=1e-4, miter=1e4,
#                    inner_conv=1e-3, inner_iter=5e3, WA=weight)
#    conv_flag <- fit$conv_flag
#    if(!conv_flag){
#      print("Warning: not converge")
#    }
#  
#    return(list(B=fit$A, A=fit$V, C=fit$C, iter=fit$iter))
#  }
#  
#  
#  sample.generator <- function(n, p, p0, q, r, rho.x, rho.e){
#    X <- matrix(0, nrow = n, ncol = p)
#    Y <- E <- matrix(0, nrow = n, ncol = q)
#    B <- matrix(c(rnorm(p0*r),rep(0,(p-p0)*r)), nrow = p, ncol = r, byrow = TRUE)
#    A <- matrix(rnorm(q*r), nrow = q, ncol = r)
#    C <- B%*%t(A)
#  
#    sigma.x <- matrix(rho.x, nrow = p, ncol = p)
#    sigma.x <- sigma.x+diag(1-rho.x, p)
#    X <- mvrnorm(n, mu = rep(0,p), Sigma = sigma.x)
#  
#    sigma.e <- matrix(rho.e, nrow = q, ncol = q)
#    sigma.e <- sigma.e+diag(1-rho.e, q)
#    sgm2 <- sum(diag(t(C)%*%sigma.x%*%C))/q
#    E <- mvrnorm(n, mu = rep(0,q), Sigma = sgm2*sigma.e)
#  
#    Y <- X%*%C+E
#  
#    return(list(X=X,Y=Y,E=E,C=C,B=B,A=A))
#  }
#  
#  
#  SRRR.sim <- function(n, p, q, p0, r, rho.x, rho.e, runs=30){
#    mse1 <- mse2 <- time1 <- time2 <- numeric(runs)
#    for(i in 1:runs){
#      Sample <- sample.generator(n,p,p0,q,r,rho.x,rho.e)
#      X <- Sample$X
#      Y <- Sample$Y
#      C <- Sample$C
#      res1 <- srrr(Y, X, nrank = r, method = "adglasso")
#      C1 <- res1$U %*% res1$D %*% t(res1$V)
#      res2 <- SRRR(Y, X, r=r, weight = rep(1, p))
#      C2 <- res2$C
#      mse1[i] <- norm(C-C1,type = "F")^2/(n*q)
#      mse2[i] <- norm(C-C2,type = "F")^2/(n*q)
#      time1[i] <- summary(microbenchmark(srrr(Y, X, nrank = r, method = "adglasso")))$mean
#      time2[i] <- summary(microbenchmark(SRRR(Y, X, r=r, weight = rep(1, p))))$mean
#    }
#    MSE1 <- mean(mse1)
#    SE1 <- sd(mse1)
#    MSE2 <- mean(mse2)
#    SE2 <- sd(mse2)
#    Time1 <- mean(time1)
#    Time2 <- mean(time2)
#    return(list(MSE1=MSE1, SE1=SE1, MSE2=MSE2, SE2=SE2, Time1=Time1, Time2=Time2))
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  Rcpp::List SRRR_cpp(arma::mat Y, arma::mat X, String method, arma::mat A0, arma::mat V0,
#                      int r, double lambda,
#                      double conv, int miter,
#                      double inner_conv, double inner_iter,
#                      arma::vec WA){
#    int p=X.n_cols;
#    //int n=X.n_rows, q=Y.n_cols
#    arma::mat YtX=Y.t()*X;
#  
#    double Xrk=accu(svd(X)>0.01);
#    int iter=0;
#    double dfu0,dfv0;
#    bool conv_flag;
#    double C1f, sqe, df;
#    arma::vec s;
#    arma::vec diff(miter+1);
#    diff.fill(conv+1);
#    arma::mat V1, A1, C0, C1, W, u, v, residual;
#    Rcpp::List inner_out;
#    Rcpp::List out;
#  
#    V1 = V0;
#    A1 = A0;
#    C1 = A0*V0.t();
#  
#    while((iter<miter)&(diff(iter)>conv)){
#      V0 = V1;
#      A0 = A1;
#      C0 = C1;
#  
#      arma::mat YV0 = Y*V0;
#      if(method=="sub"){
#        inner_out = subgradient_cpp(YV0, X, lambda*WA, A0, inner_conv, inner_iter);
#      }
#      arma::mat inner_outB = inner_out["B"];
#      A1 = inner_outB;
#      W = YtX*A1;
#      svd(u, s, v, W);
#      u = u.cols(0,r-1);
#      V1 = u * v.t();
#      C1 = A1*V1.t();
#      C1f = accu(square(C1));
#      if (C1f == 0) {
#        diff(iter) = 0;
#      } else {
#        iter = iter + 1;
#        diff(iter) = pow(accu(square(C0 - C1))/C1f,0.5);
#      }
#    }
#  
#    diff = diff.subvec(0, iter);
#    residual = Y - X * C1;
#    sqe = accu(square(residual));
#    dfu0 = accu(A1 != 0);
#    dfv0 = accu(V1 != 0);
#    df = dfu0 * Xrk/p + dfv0 - r*r;
#  
#    if (diff(iter) <= conv) {
#      conv_flag = true;
#    } else {
#      conv_flag = false;
#    }
#  
#    out["diff"] = diff;
#    out["iter"] = iter;
#    out["sqe"]  = sqe;
#    out["df"]   = df;
#    out["conv_flag"] = conv_flag;
#    out["A"]  = A1;
#    out["V"]  = V1;
#    out["C"]  = C1;
#    return(out);
#  }
#  
#  
#  Rcpp::List subgradient_cpp(arma::mat Y, arma::mat X, arma::vec lam, arma::mat B0, double conv, int miter){
#    int p=X.n_cols, iter=0, j;
#    double diff=conv+1, B1f, sqe;
#    arma::mat B1, R, Rj, XRj;
#    arma::rowvec sh = sum(square(X), 0);;
#    Rcpp::List out;
#  
#    B1 = B0;
#    R = Y-X*B1;
#    while((diff>conv)&(iter<miter)){
#      B0 = B1;
#      for(j=0;j<p;j++){
#        Rj = R+X.col(j)*B1.row(j);//nxq
#        XRj = trans(X.col(j))*Rj; //1xq
#        arma::rowvec t1=XRj/as_scalar(sh(j))*max(0.0,1-lam(j)/pow(accu(square(XRj)),0.5));
#        B1.row(j) = t1;
#        R = Rj - X.col(j)*B1.row(j);
#      }
#      B1f = accu(square(B1));
#      if(B1f==0){
#        iter = miter;
#      }else{
#        diff = pow(accu(square(B0 - B1))/B1f,0.5);
#        iter = iter + 1;
#      }
#    }
#    sqe = accu(square(Y-X*B0));
#  
#    out["B"] = B1;
#    out["sqe"] = sqe;
#    out["iter"] = iter;
#  
#    return(out);
#  }

## -----------------------------------------------------------------------------
n = 50
p = 10
p0 = 5
q = 10
r = 3
rho.x = rho.e = 0.1

set.seed(22099)
SRRR.sim(n, p, q, p0, r, rho.x, rho.e)

## -----------------------------------------------------------------------------
n = 50
p = 30
p0 = 5
q = 30
r = 3
rho.x = rho.e = 0

set.seed(22099)
SRRR.sim(n, p, q, p0, r, rho.x, rho.e)

