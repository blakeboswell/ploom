#ifndef PLOOM_BOUNDED_QR_H
#define PLOOM_BOUNDED_QR_H
#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]


//' @internal
class BoundedQr {
  
private:
  const double zero_      = 0.0;
  const double near_zero_ = 1.e-69;
  
  int n_cov_;     // number of covariates in model including intercept if present
  int n_obs_;     // online count of number of observations processed
  int r_dim_;     // size of upper triangle of R in QR-factorization
  
public:
  arma::vec D_;
  arma::vec rbar_;
  arma::vec thetab_;
  arma::vec tol_;
  
  double sserr_;
  bool tolset_;
  
  void set_tolerance();
  void check_singularity();
  
  BoundedQr(int p) {
    
    n_cov_ = p;
    n_obs_ = 0;
    r_dim_ = p * (p - 1) / 2;
    
    D_      = arma::zeros(n_cov_);
    rbar_   = arma::zeros(r_dim_);
    thetab_ = arma::zeros(n_cov_);
    tol_    = arma::zeros(n_cov_);
    
    sserr_  = zero_;
    tolset_ = false;
    
  };
  
  ~BoundedQr(){ /* am free? */ };
  
  void include(arma::vec &xrow, double yelem, double weight);
  void update(arma::mat &X, arma::vec &y, arma::vec &w);
  arma::vec betas();
  
};


RCPP_MODULE(BoundedQrModule) {
  
  using namespace Rcpp;
  
  class_<BoundedQr>("BoundedQr")
    
    .constructor<int>()
    
    .field("D",       &BoundedQr::D_)
    .field("rbar",    &BoundedQr::rbar_)
    .field("thetab",  &BoundedQr::thetab_)
    .field("tolset",  &BoundedQr::tolset_)
    .field("tol",     &BoundedQr::tol_)
    .field("sserr",   &BoundedQr::sserr_)
    
    .method("update",  &BoundedQr::update)
    .method("betas",   &BoundedQr::betas)
    .method("check_singularity", &BoundedQr::check_singularity)
    ;
  
}

#endif