#ifndef PLOOM_BOUNDED_QR_H
#define PLOOM_BOUNDED_QR_H
#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]


//'  Algorithm AS 274: Least Squares Routines to Supplement Those of Gentleman
//'  Author(s): Alan J. Miller
//'  Source: Journal of the Royal Statistical Society.
//'  Series Applied Statistics, Vol. 41, No. 2
//'  (1992), pp. 458-478
//'
//'  num_obs_
//'  the latest count of observations processed.
//'  
//'  num_params_ 
//'  the total number of dependent variables,
//'  including the constant if present
//'
//'  rbar_dim_
//'  the dimension of the upper triangular matrix rbar_
//'  
//'  tol_set_
//'  a logical variable which is set when subroutine tolset() has
//'  been called to calculate tolerances for use in testing for
//'  singularities.
//'  
//'  rss_set_
//'  a logical variable indicating whether residual sums of squares
//'  are available.
//'  
//'  D_
//'  array of row multipliers for the Cholesky factorization.
//'  
//'  thetab_
//'  vector of projections (after scaling by sqrt(D))
//'
//'  rbar_
//'  upper-triangular matrix excluding the implicit 1's on the diagonal,
//'  representing the Cholesky factorization.
//'
//'  tol_
//'  array of tolerances used in testing for singularities
//'  
//'  rss_
//'  array of residual sums of squares
//'
//'  sserr_
//'  residual sum of squares with all of the variables included
//'  equal to last element in rss_
//'  
//' @keywords internal
class BoundedQr {
  
private:
  const double ZERO_      = 0.0;
  const double NEAR_ZERO_ = 1.e-69;
  
  int rbar_dim_;
  arma::vec lindep_;
  
  bool rss_set_;
  bool tol_set_;
  bool sing_checked_;
  
  void residual_sumsquares();
  void set_tolerance();
  arma::vec rbar_inverse(int nreq);
  void check_singularity();
  
public:
  
  int num_params_;
  int num_obs_;
  
  arma::vec D_;
  arma::vec rbar_;
  arma::vec thetab_;
  arma::vec tol_;
  
  arma::vec rss_;
  double sserr_;
  double sumy_;
  double sumysq_;

  BoundedQr(int np) {
    
    num_params_ = np;
    num_obs_    = 0;
    rbar_dim_   = np * (np - 1) / 2;
    
    D_       = arma::zeros(num_params_);
    rbar_    = arma::zeros(rbar_dim_);
    thetab_  = arma::zeros(num_params_);
    tol_     = arma::zeros(num_params_);
    lindep_  = arma::vec(num_params_).fill(false);
    
    rss_     = arma::zeros(num_params_);
    sserr_   = ZERO_;
    sumy_    = ZERO_;
    sumysq_  = ZERO_;
    
    tol_set_      = false;    
    rss_set_      = false;
    sing_checked_ = false;
    
  };
  
  ~BoundedQr(){ /* am free? */ };
  
  void include(arma::vec &xrow, double yelem, double weight);
  void update(arma::mat &X, arma::vec &y, arma::vec &w);
  arma::vec vcov(int nreq);
  arma::mat vcov_sugar(int nreq);
  arma::vec beta();
  
  // accessors
  arma::vec rss();
  arma::vec lindep();
  double rank();
  
};


RCPP_MODULE(BoundedQrModule) {
  
  using namespace Rcpp;
  
  class_<BoundedQr>("BoundedQr")
    
    .constructor<int>()
    
    .field("D",       &BoundedQr::D_)
    .field("rbar",    &BoundedQr::rbar_)
    .field("thetab",  &BoundedQr::thetab_)
    .field("tol",     &BoundedQr::tol_)

    .field("num_params", &BoundedQr::num_params_)
    .field("num_obs",    &BoundedQr::num_obs_)

    .field("rss_full", &BoundedQr::sserr_)
    .field("sumsqy",   &BoundedQr::sumysq_)
    
    .method("update",  &BoundedQr::update)
    .method("beta",    &BoundedQr::beta)
    .method("vcov",    &BoundedQr::vcov)
    .method("rss",     &BoundedQr::rss)
    .method("lindep",  &BoundedQr::lindep)
    .method("rank",    &BoundedQr::rank)
    ;
  
}

#endif