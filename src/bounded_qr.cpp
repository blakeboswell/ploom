#include "bounded_qr.h"


//' @internal
void BoundedQr::update(arma::mat &X,
                       arma::vec &y,
                       arma::vec &w) {
  
  const int np = n_cov_;
  
  arma::vec xrow = arma::vec(np);
  
  for(int i = 0; i < X.n_rows; ++i) {
    
    for(int j = 0; j < X.n_cols; ++i) {
      xrow[j] = X(i, j);
    }
    
    includ(xrow, y[i], w[i]);
    
  }
  
}


//' @internal
//'  ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
//'  Modified from algorithm AS 75.1
//'
//'  Calling this routine updates `D`, `rbar`, `thetab` and `sserr` by the
// ' inclusion of `xrow`, `yelem` with the specified `weight`.   The number
//'  of columns (variables) may exceed the number of rows (cases).
//'  @param xrow
//'  @param yelem
//'  @param weight
void BoundedQr::includ(arma::vec &xrow,
                       double yelem,
                       double weight) {

  const int np     = n_cov_;
  const int n_xrow = xrow.n_rows;
  
  double w = weight;
  double y = yelem; 
  
  ++n_obs_;
  int nextr = 0;
  
  for(int i = 0; i < np; ++i) {
  //  Skip unnecessary transformations.   Test on exact zeroes must be
  //  used or stability can be destroyed.
  
    if(::abs(weight) < near_zero_) {
      return;
    }
    
    double xi = xrow[i];
    if(::abs(xi) < near_zero_) {
      nextr += np - i - 1;
      continue;
    }
    
    double di  = D[i];
    double wxi = w * xi;
    double dpi = di + wxi * xi;
    
    if(dpi <= near_zero_) { /* div by zero? */ };
    
    double cbar = di / dpi;
    double sbar = wxi / dpi;
  
    w = cbar * w;
    
    D[i] = dpi;
    
    for(int k = i + 1; k < np - 1; ++k) {
      double xk   = xrow[k];
      xrow[k]     = xk - xi * rbar[nextr];
      rbar[nextr] = cbar * rbar[nextr] + sbar * xk;
      ++nextr;
    }
    
    double xk = y;
    y  = xk - xi * thetab[i];
    thetab[i] = cbar * thetab[i] + sbar * xk;
    
  }
  // `yelem` * `sqrt(weight)` is now equal to Brown & Durbin's recursive residual.
  
  sserr += w * y * y;
  
}


//' @internal
//'  ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
//'
//'  Sets up array TOL for testing for zeroes in an orthogonal
//'  reduction formed using AS75.1.
void BoundedQr::tolset() {

    const int np = n_cov_;
  
//  EPS is a machine-dependent constant.   For compilers which use
//  the IEEE format for floating-point numbers, recommended values
//  are 1.E-06 for single precision and 1.D-12 for double precision.
    
    const double eps = 1.0e-12;
    
//  Set `tol[i]` = sum of absolute values in column `i` of `rbar` after
//  scaling each element by the square root of its row multiplier.

    arma::vec work = arma::sqrt(D);

    for(int col = 1; col < np; ++col) {
      
      int pos = col - 1;
      double total = work[col];
      
      for(int row = 0; row < col; ++row) {
        total += ::abs(rbar[pos]) * work[row];
        pos   += np - row - 2;
      }
      
      tol[col] = eps * total;
      
    }
    
    tol_set = true;

}


//'  ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
//'
//'  Checks for singularities, reports, and adjusts orthogonal
//'  reductions produced by AS75.1.
void BoundedQr::singchk() {

  const int np = n_cov_;

  arma::vec linedep = arma::vec(np).fill(false);
  arma::vec work    = arma::sqrt(D);
  
  for(int col = 0; col < np; ++col) {
  //  Set elements within `rbar` to `zero`` if they are less than `tol[col]` in
  //  absolute value after being scaled by the square root of their row
  //  multiplier.
    
    double temp = tol[col];
    int pos  = col - 1;
    
    for(int row = 0; row < col - 1; ++row) {
      if(::abs(rbar[pos]) * work[row] < temp) {
        rbar[pos] = zero_;
      }
      pos += np - row - 2;
    }
    
  //  If diagonal element is near zero, set it to zero, set appropriate
  //  element of `lindep`, and use `includ` to augment the projections in
  //  the lower rows of the orthogonalization.
  
    if(work[col] <= temp) {
      
      linedep[col] = true;
      
      if(col < np - 1) {
        
        arma::vec x = arma::zeros(np);
        int pos2 = col * (np + np - col - 1) / 2;
        
        for(int k = col + 1; k < np; ++k, ++pos2) {
          x[k]       = rbar[pos2];
          rbar[pos2] = zero_;
        }
        
        double y = thetab[col];
        double w = D[col];
        
        D[col] = zero_;
        thetab[col] = zero_;
        
        includ(x, w, y);
        
        // undo n_obs_ increment performed in includ
        --n_obs_;
        
      } else {
        sserr += D[col] * thetab[col] * thetab[col];
      }
    }
  }
}


//'  ALGORITHM AS274  APPL. STATIST. (1992) VOL 41, NO. x
//'
//'  Modified version of AS75.4 to calculate regression coefficients
//'  for the first NREQ variables, given an orthogonal reduction from
//'  AS75.1.
arma::vec BoundedQr::regcf() {
  
  const int np = n_cov_;

  if(!tol_set) {
    tolset();
  }
  
  arma::vec beta = arma::zeros(np);
  arma::vec work = arma::sqrt(D);
  
  for(int i = np - 1; i > -1; --i) {
    
    if(work[i] < tol[i]) {
      beta[i] = zero_;
      D[i]    = zero_;
    } else {
      
      beta[i] = thetab[i];
      int nextr = i * (np + np - i - 1) / 2;
      
      for(int j = i + 1; j < np; ++j) {
        beta[i] -= rbar[nextr] * beta[j];
        ++nextr;
      }
    }
  }

  return beta;
  
}


