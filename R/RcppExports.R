# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

subgradient_cpp <- function(Y, X, lam, B0, conv, miter) {
    .Call('_StatComp22099_subgradient_cpp', PACKAGE = 'StatComp22099', Y, X, lam, B0, conv, miter)
}

SRRR_cpp <- function(Y, X, method, A0, V0, r, lambda, conv, miter, inner_conv, inner_iter, WA) {
    .Call('_StatComp22099_SRRR_cpp', PACKAGE = 'StatComp22099', Y, X, method, A0, V0, r, lambda, conv, miter, inner_conv, inner_iter, WA)
}

#' @title xxx
#' @description xxx
#' @param M a square matrix
#' @return eigen values
#' @import Rcpp
#' @import RcppArmadillo
#' @useDynLib StatComp22099
#' @export
ei <- function(M) {
    .Call('_StatComp22099_ei', PACKAGE = 'StatComp22099', M)
}

#' @title A Gibbs sampler using Rcpp
#' @description A Gibbs sampler using Rcpp
#' @param N the number of samples
#' @param burn the burn-in time of the chain
#' @param rho the correlation coefficient of binormal distribution
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' rnC <- gibbsC(500,100,0.1)
#' par(mfrow=c(2,1));
#' plot(rnC[,1],type='l')
#' plot(rnC[,2],type='l')
#' }
#' @useDynLib StatComp22099
#' @export
gibbsC <- function(N, burn, rho) {
    .Call('_StatComp22099_gibbsC', PACKAGE = 'StatComp22099', N, burn, rho)
}

