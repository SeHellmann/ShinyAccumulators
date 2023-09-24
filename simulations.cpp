#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector simulatepaths(NumericVector params, double delta, double maxrt, int N) {
  double a   =    params[0];
  double v   =    params[1];
  double zr  =    params[2];
  double s   =    params[3];
  double sv  =    params[4];
  double szr =    params[5];
  double svis =   params[6];
  double sigvis = params[7];
  double muvis =  params[8];
  double tau =    params[9];
  double w   =    params[10];
  double lambda = params[11];
  
  double mu, x0, vis, vismu;
  int steps, poststeps, K;
  poststeps = ceil(tau/delta);
  steps = ceil(maxrt / delta);
  // output includes (steps+poststeps+1) columns for X (+1) and V states  each
  // and one column for T (dectime), response, final X state, final Vis state, and conf each
  NumericVector NAvec(N*((steps+poststeps+1)*2+6),NA_REAL); 
  NumericMatrix out ((steps+poststeps+1)*2+6, N, NAvec.begin()); 
  //NumericMatrix V (N, steps);
  //NumericVector X (steps);
  //NumericVector T (N);
  //NumericVector resp (N);
  //NumericVector C (N);
  
  for (int i=0; i < N; i++) {
    mu = R::rnorm(v, sv);
    vismu = R::rnorm(muvis, sigvis);
    vis= 0;
    x0 = a* R::runif(zr-szr/2, zr+szr/2);
    out(0, i ) = x0;
    out((poststeps+steps+2) , i) = vis;
    // accumulation to bound
    for (int k=1; k < steps+1; k++) {
      x0 = x0 + R::rnorm(delta*mu, sqrt(delta)*s);
      out(k ,i) = x0;
      vis = vis + R::rnorm(delta*vismu, sqrt(delta)*svis);
      out(k + (poststeps+steps+2) , i) = vis;
      if (x0 < 0) {
        //out(i, Range(k,(steps-1))) = 0;
        x0 = 0;
        out(2*(poststeps+steps+1) + 2 , i) = -1.0;
        K = k;
        break;
      }
      if (x0 > a) {
        x0 = a;
        out(2*(poststeps+steps+1) + 2 , i) = 1.0;
        K = k;
        break;
      }
    }
    if (K == (steps-1) && x0>0 && x0 < a) {
      out(2*(poststeps+steps+1) + 2 , i) = 0.0;
      continue;
    }
    out(K , i) = x0;
    out(steps+1, i) = x0;
    out(2*(poststeps+steps+1)+1 , i) = K;
    // post-decisional accumulation
    for (int k=1; k < poststeps+1; k++) {
      x0 = x0 + R::rnorm(delta*mu, sqrt(delta)*s);
      out(steps+k+1 , i) = x0;
      vis = vis + R::rnorm(delta*vismu, sqrt(delta)*svis);
      out(k+(K+poststeps+steps+2) , i) = vis;
    }
    // compute confidence variable
    out(2*(poststeps+steps+1)+3 , i) = x0;
    out(2*(poststeps+steps+1)+4 , i) = vis;
    out(2*(poststeps+steps+1)+5 , i) = (w*out(2*(poststeps+steps+1) + 1 , i)*(x0 - a*zr) + (1-w)*vis)/pow(K*delta+tau, lambda);
    
    if (i % 200 ==0 ) Rcpp::checkUserInterrupt();
  }
  
  return out;
}

