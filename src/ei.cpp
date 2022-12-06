// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
// using namespace arma;

//' @title xxx
//' @description xxx
//' @param M a square matrix
//' @return eigen values
//' @import Rcpp
//' @import RcppArmadillo
//' @useDynLib StatComp22099
//' @export
// [[Rcpp::export]]
arma::vec ei(arma::mat M) {
  return arma::eig_sym(M);
}
