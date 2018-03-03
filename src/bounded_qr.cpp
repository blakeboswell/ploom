#include "bounded_qr.h"
#include <string>

//' @keywords internal
void BoundedQr::update(arma::mat &X,
                       arma::vec &y,
                       arma::vec &w) {
  
  const int np = n_cov_;
  arma::vec xrow = arma::vec(np);
  
  for(int i = 0; i < X.n_rows; ++i) {
    
    for(int j = 0; j < X.n_cols; ++j) {
      xrow[j] = X(i, j);
    }
    
    include(xrow, y[i], w[i]);
    
  }
  
}


//'  ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
//'  Modified from algorithm AS 75.1
//'
//'  Calling this routine updates `D_`, `rbar_`, `thetab_` and `sserr` by the
// ' inclusion of `xrow`, `yelem` with the specified `weight`.   The number
//'  of columns (variables) may exceed the number of rows (cases).
//'  @param xrow
//'  @param yelem
//'  @param weight
//'  @keywords internal
void BoundedQr::include(arma::vec &xrow,
                        double yelem,
                        double weight) {

  const int np = n_cov_;
  
  double w = weight;
  double y = yelem;
  double xk;
  
  ++n_obs_;
  int nextr = 0;
  
  rssset_ = false;
  
  for(int i = 0; i < np; ++i) {
  //  Skip unnecessary transformations.   Test on exact zeroes must be
  //  used or stability can be destroyed.
  
    if(::abs(w) <= near_zero_) {
      return;
    }
    
    double xi = xrow[i];
    if(::abs(xi) <= near_zero_) {
      nextr += np - i - 1;
      continue;
    }
    
    double di  = D_[i];
    double wxi = w * xi;
    double dpi = di + wxi * xi;
    
    if(dpi <= near_zero_) { /* TODO: div by zero? */};
    
    double cbar = di / dpi;
    double sbar = wxi / dpi;
  
    w = cbar * w;
    
    D_[i] = dpi;
    
    for(int k = i + 1; k < np; ++k) {
      xk           = xrow[k];
      xrow[k]      = xk - xi * rbar_[nextr];
      rbar_[nextr] = cbar * rbar_[nextr] + sbar * xk;
      ++nextr;
    }
    
    xk = y;
    y  = xk - xi * thetab_[i];
    thetab_[i] = cbar * thetab_[i] + sbar * xk;
    
  }
  // `yelem` * `sqrt(weight)` is now equal to Brown & Durbin's recursive residual.
  
  sserr_ += w * y * y;
  singchecked_ = false;
  tolset_      = false;
  
}


//'  ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
//'
//'  Sets up array TOL for testing for zeroes in an orthogonal
//'  reduction formed using AS75.1.
//' @keywords internal
void BoundedQr::set_tolerance() {

    const int np = n_cov_;
  
//  EPS is a machine-dependent constant.   For compilers which use
//  the IEEE format for floating-point numbers, recommended values
//  are 1.E-06 for single precision and 1.D-12 for double precision.
    
    const double eps = 1.0e-12;
    
//  Set `tol_[i]` = sum of absolute values in column `i` of `rbar_` after
//  scaling each element by the square root of its row multiplier.

    arma::vec work = arma::sqrt(D_);

    for(int col = 1; col < np; ++col) {
      
      int pos = col - 1;
      double total = work[col];
      
      for(int row = 0; row < col; ++row) {
        total += ::abs(rbar_[pos]) * work[row];
        pos   += np - row - 2;
      }
      
      tol_[col] = eps * total;
      
    }
    
    tolset_ = true;

}


//'  ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
//'
//'  Checks for singularities, reports, and adjusts orthogonal
//'  reductions produced by AS75.1.
//' @keywords internal
void BoundedQr::check_singularity() {

  const int np   = n_cov_;
  arma::vec work = arma::sqrt(D_);
  
  if(!tolset_) {
    set_tolerance();
  }
  
  for(int col = 0; col < np; ++col) {
  //  Set elements within `rbar_` to `zero`` if they are less than `tol_[col]` in
  //  absolute value after being scaled by the square root of their row
  //  multiplier.
    
    double temp = tol_[col];
    int pos  = col - 1;
    
    for(int row = 0; row < col - 1; ++row) {
      if(::abs(rbar_[pos]) * work[row] < temp) {
        rbar_[pos] = zero_;
      }
      pos += np - row - 2;
    }
    
  //  If diagonal element is near zero, set it to zero, set appropriate
  //  element of `lindep`, and use `include` to augment the projections in
  //  the lower rows of the orthogonalization.
  
    lindep_[col] = false;
    
    if(work[col] <= temp) {
      
      lindep_[col] = true;
      
      if(col < np - 1) {
        
        arma::vec x = arma::zeros(np);
        int pos2 = col * (np + np - col - 1) / 2;
        
        for(int k = col + 1; k < np; ++k, ++pos2) {
          x[k]       = rbar_[pos2];
          rbar_[pos2] = zero_;
        }
        
        double y = thetab_[col];
        double w = D_[col];
        
        D_[col] = zero_;
        thetab_[col] = zero_;
        
        include(x, w, y);
        
        // undo n_obs_ increment performed in include
        --n_obs_;
        
      } else {
        sserr_ += D_[col] * thetab_[col] * thetab_[col];
      }
    }
  }
  singchecked_ = true;
}


//'  ALGORITHM AS274  APPL. STATIST. (1992) VOL 41, NO. x
//'
//'  Modified version of AS75.4 to calculate regression coefficients
//'  for the first NREQ variables, given an orthogonal reduction from
//'  AS75.1.
//' @keywords internal
arma::vec BoundedQr::betas() {
  
  const int np = n_cov_;

  if(!singchecked_) {
    check_singularity();
  }
  
  arma::vec beta = arma::zeros(np);
  arma::vec work = arma::sqrt(D_);
  
  for(int i = np - 1; i > -1; --i) {
    
    if(work[i] < tol_[i]) {
      beta[i] = zero_;
      D_[i]   = zero_;
    } else {
      
      beta[i] = thetab_[i];
      int nextr = i * (np + np - i - 1) / 2;
      
      for(int j = i + 1; j < np; ++j) {
        beta[i] -= rbar_[nextr] * beta[j];
        ++nextr;
      }
    }
  }

  return beta;
  
}


//' @keywords internal
void BoundedQr::residual_sumsquares() {
  
  int np = n_cov_;
  
  double total = sserr_;
  rss_[np - 1] = sserr_;
  
  for(int i = np - 1; i > 0; i--) {
    total += D_[i] * thetab_[i] * thetab_[i];
    rss_[i - 1] = total;
  }
  
  rssset_ = true;
  
}


//' @keywords internal
arma::vec BoundedQr::vcov(int nreq) {
  
  if(n_obs_ <= nreq) {
    // TODO: handle error appropriately
    return arma::vec(1).fill(NA_REAL);
  }
  
  if(!singchecked_) {
    check_singularity();
  }
  
  if(!rssset_) {
    residual_sumsquares();
  }
  
  double rnk = 0.0;
  for (int i = 0; i < nreq; ++i) {
    if(!lindep_[i]) {
      rnk++;
    }
  }
  
  double var       = rss_[nreq - 1] / (n_obs_ - rnk);
  arma::vec rinv   = rbar_inverse(nreq);
  arma::vec covmat = arma::vec(nreq * (nreq + 1) / 2).fill(NA_REAL);
  
  int pos1;
  int pos2;
  int start    = 0;
  double total = 0.0;
  
  for(int row = 0; row < nreq; ++row) {
    
    pos2 = start;
    if(!lindep_[row]) {
      for(int col = row; col < nreq; ++col) {
        if(!lindep_[col]) {
          
          pos1 = start + col - row;
          
          if(row == col) {
            total = 1.0 / D_[col];
          } else {
            total = rinv[pos1 - 1] / D_[col];
          }
          
          for(int k = col + 1; k < nreq; k++) {
            if(!lindep_[k]) {
              total += rinv[pos1] * rinv[pos2] / D_[k];
            }
            ++pos1;
            ++pos2;
          }
          
          covmat[(col + 1) * col / 2 + row] = total * var;
          
        } else {
          pos2 += nreq - col - 1;
        }
      }
    }
    start += nreq - row - 1;
  }
  return covmat;
}


//' @keywords internal
arma::vec BoundedQr::rbar_inverse(int nreq) {
  
  int np   = n_cov_;
  int pos  = nreq * (nreq - 1) / 2 - 1;
  int pos1 = -1;
  int pos2 = -1;
  
  double total   = 0.0;
  arma::vec rinv = arma::vec(pos + 1).fill(NA_REAL);
  
  for(int row = nreq - 1; row > 0; --row) {
    if(!lindep_[row]) {
      
      int start = (row - 1) * (np + np - row) / 2;
      
      for(int col = nreq; col > row; --col) {
        
        pos1  = start;
        pos2  = pos;
        total = 0.0;
        
        for(int k = row; k < col - 1; ++k) {
          pos2 += nreq - k - 1;
          if (!lindep_[k]) {
            total += -rbar_[pos1] * rinv[pos2];
          }
          ++pos1;
        }
        
        rinv[pos] = total - rbar_[pos1];
        --pos;
        
      }
    } else {
      pos -= nreq - row;
    }
  }
  return rinv;
}

