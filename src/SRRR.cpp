// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include <math.h>

using namespace Rcpp;
using namespace arma;
using namespace std;



// [[Rcpp::export]]
Rcpp::List subgradient_cpp(arma::mat Y, arma::mat X, arma::vec lam, arma::mat B0, double conv, int miter){
  int p=X.n_cols, iter=0, j;
  double diff=conv+1, B1f, sqe;
  arma::mat B1, R, Rj, XRj;
  arma::rowvec sh = sum(square(X), 0);;
  Rcpp::List out;
  
  B1 = B0;
  R = Y-X*B1;
  while((diff>conv)&(iter<miter)){
    B0 = B1;
    for(j=0;j<p;j++){
      Rj = R+X.col(j)*B1.row(j);//nxq
      XRj = trans(X.col(j))*Rj; //1xq
      arma::rowvec t1=XRj/as_scalar(sh(j))*max(0.0,1-lam(j)/pow(accu(square(XRj)),0.5));
      B1.row(j) = t1;
      R = Rj - X.col(j)*B1.row(j);
    }
    B1f = accu(square(B1));
    if(B1f==0){
      iter = miter;
    }else{
      diff = pow(accu(square(B0 - B1))/B1f,0.5);
      iter = iter + 1;
    }
  }
  sqe = accu(square(Y-X*B0));
  
  out["B"] = B1;
  out["sqe"] = sqe;
  out["iter"] = iter;
  
  return(out);
}


// [[Rcpp::export]]
Rcpp::List SRRR_cpp(arma::mat Y, arma::mat X, String method, arma::mat A0, arma::mat V0,
                    int r, double lambda,
                    double conv, int miter,
                    double inner_conv, double inner_iter,
                    arma::vec WA){
  int p=X.n_cols;
  //int n=X.n_rows, q=Y.n_cols
  arma::mat YtX=Y.t()*X;
  
  double Xrk=accu(svd(X)>0.01);
  int iter=0;
  double dfu0,dfv0;
  bool conv_flag;
  double C1f, sqe, df;
  arma::vec s;
  arma::vec diff(miter+1);
  diff.fill(conv+1);
  arma::mat V1, A1, C0, C1, W, u, v, residual;
  Rcpp::List inner_out;
  Rcpp::List out;
  
  V1 = V0;
  A1 = A0;
  C1 = A0*V0.t();
  
  while((iter<miter)&(diff(iter)>conv)){
    V0 = V1;
    A0 = A1;
    C0 = C1;
    
    arma::mat YV0 = Y*V0;
    if(method=="sub"){
      inner_out = subgradient_cpp(YV0, X, lambda*WA, A0, inner_conv, inner_iter);
    }
    arma::mat inner_outB = inner_out["B"];
    A1 = inner_outB;
    W = YtX*A1;
    svd(u, s, v, W);
    u = u.cols(0,r-1);
    V1 = u * v.t();
    C1 = A1*V1.t();
    C1f = accu(square(C1));
    if (C1f == 0) {
      diff(iter) = 0;
    } else {
      iter = iter + 1;
      diff(iter) = pow(accu(square(C0 - C1))/C1f,0.5);
    }
  }
  
  diff = diff.subvec(0, iter);
  residual = Y - X * C1;
  sqe = accu(square(residual));
  dfu0 = accu(A1 != 0);
  dfv0 = accu(V1 != 0);
  df = dfu0 * Xrk/p + dfv0 - r*r;
  
  if (diff(iter) <= conv) {
    conv_flag = true;
  } else {
    conv_flag = false;
  }
  
  out["diff"] = diff;
  out["iter"] = iter;
  out["sqe"]  = sqe;
  out["df"]   = df;
  out["conv_flag"] = conv_flag;
  out["A"]  = A1;
  out["V"]  = V1;
  out["C"]  = C1;
  return(out);
}
