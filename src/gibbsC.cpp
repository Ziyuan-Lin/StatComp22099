#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param burn the burn-in time of the chain
//' @param rho the correlation coefficient of binormal distribution
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- gibbsC(500,100,0.1)
//' par(mfrow=c(2,1));
//' plot(rnC[,1],type='l')
//' plot(rnC[,2],type='l')
//' }
//' @useDynLib StatComp22099
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int N, int burn, double rho) {
  NumericMatrix X(N, 2);
  double s1, s2, x1, x2;
  int i;
  
  s1 = sqrt(1-rho*rho);
  s2 = sqrt(1-rho*rho);
  
  X(0, 0) = 0;
  X(0, 1) = 0;
  for (i=1; i<N; i++) {
    x2 = X(i-1, 1);
    X(i, 0) = rnorm(1, 0.9*x2, s1)[0];
    x1 = X(i, 0);
    X(i, 1) = rnorm(1, 0.9*x1, s2)[0];
  }
  
  return(X);
}


