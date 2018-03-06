#ifndef PLOOM_BOUNDED_QR_H
#define PLOOM_BOUNDED_QR_H
#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]


//' @keywords internal
class BoundedQr {
  
private:
  const double zero_      = 0.0;
  const double near_zero_ = 1.e-69;
  
  int r_dim_;     // size of upper triangle of R in QR-factorization
  arma::vec lindep_;
  
  bool rssset_;
  bool tolset_;
  bool singchecked_;
  void residual_sumsquares();
  void set_tolerance();
  arma::vec rbar_inverse(int nreq);
  void check_singularity();
  
public:
  
  int n_cov_;     // number of covariates in model including intercept if present
  int n_obs_;     // online count of number of observations processed
  
  arma::vec D_;
  arma::vec rbar_;
  arma::vec thetab_;
  arma::vec tol_;
  arma::vec rss_;
  
  double sserr_;
  double sumysq_;
  double sumy_;

  BoundedQr(int np) {
    
    n_cov_ = np;
    n_obs_ = 0;
    r_dim_ = np * (np - 1) / 2;
    
    D_       = arma::zeros(n_cov_);
    rbar_    = arma::zeros(r_dim_);
    thetab_  = arma::zeros(n_cov_);
    tol_     = arma::zeros(n_cov_);
    lindep_  = arma::vec(n_cov_).fill(false);
    rss_     = arma::zeros(n_cov_);
    
    sserr_   = zero_;
    sumy_    = zero_;
    sumysq_  = zero_;
    
    tolset_      = false;    
    rssset_      = false;
    singchecked_ = false;
    
  };
  
  ~BoundedQr(){ /* am free? */ };
  
  void include(arma::vec &xrow, double yelem, double weight);
  void update(arma::mat &X, arma::vec &y, arma::vec &w);
  arma::vec vcov(int nreq);
  arma::mat vcov_sugar(int nreq);
  arma::vec betas();
  
  // accessors
  arma::vec rss();
  arma::vec lindep();
  
};


RCPP_MODULE(BoundedQrModule) {
  
  using namespace Rcpp;
  
  class_<BoundedQr>("BoundedQr")
    
    .constructor<int>()
    
    .field("D",       &BoundedQr::D_)
    .field("np",      &BoundedQr::n_cov_)
    .field("rbar",    &BoundedQr::rbar_)
    .field("thetab",  &BoundedQr::thetab_)
    .field("tol",     &BoundedQr::tol_)
    .field("nobs",    &BoundedQr::n_obs_)
    .field("sserr",   &BoundedQr::sserr_)
    .field("sumsqy",  &BoundedQr::sumysq_)
    
    .method("update",  &BoundedQr::update)
    .method("betas",   &BoundedQr::betas)
    .method("vcov",    &BoundedQr::vcov)
    .method("rss",     &BoundedQr::rss)
    .method("lindep",  &BoundedQr::lindep)
    ;
  
}

#endif