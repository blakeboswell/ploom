#ifndef PLOOM_BOUNDED_QR_H
#define PLOOM_BOUNDED_QR_H

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]


//'
class BoundedQR {
  
private:
  const double zero_      = 0.0;
  const double near_zero_ = 1.e-69;
  
  int n_cov_;     // number of covariates in model including intercept if present
  int n_obs_;     // online count of number of observations processed
  int r_dim_;     // size of upper triangle of R in QR-factorization
  
public:
  arma::vec D;
  arma::vec rbar;
  arma::vec thetab;
  arma::vec tol;
  
  double sserr;
  bool tol_set;
  
  BoundedQR(int p) {
    
    n_cov_ = p;
    n_obs_ = 0;
    r_dim_ = p * (p - 1) / 2;
    
    D      = arma::zeros(n_cov_);
    rbar   = arma::zeros(r_dim_);
    thetab = arma::zeros(n_cov_);
    tol    = arma::zeros(n_cov_);
    
    sserr   = zero_;
    tol_set = false;
    
  };
  
  ~BoundedQR(){ /* am free? */ };
  
  void update(arma::mat &X, arma::vec &y, arma::vec &w);
  void includ(arma::vec &xrow, double yelem, double weight);
  void tolset();
  void singchk();
  arma::vec regcf();
  
};


RCPP_MODULE(BoundedQRModule) {
  
  using namespace Rcpp;
  
  class_<BoundedQR>("BoundedQR")
    
    .constructor<int>()
    
    .field("D",       &BoundedQR::D)
    .field("rbar",    &BoundedQR::rbar)
    .field("thetab",  &BoundedQR::thetab)
    .field("tol_set", &BoundedQR::tol_set)
    .field("tol",     &BoundedQR::tol)
    .field("sserr",   &BoundedQR::sserr)
    
    .method("update_qr", &BoundedQR::update)
    .method("includ",    &BoundedQR::includ)
    .method("regcf",     &BoundedQR::regcf)
    ;
  
}

#endif