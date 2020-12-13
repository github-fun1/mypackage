#include <Rcpp.h>
using  namespace Rcpp;

double lap_f(double x){
  return exp(-abs(x));
}

#include <Rcpp.h>
using  namespace Rcpp;
//' @title Metropolis sampler for random walks using Rcpp.
//' @description A Metropolis sampler for random walks using Rcpp(in homework 9).
//' @param sigma variance of the proposal distribution (double)
//' @param x0 the initial position (double)
//' @param N the length of chain (integer)
//' @return a list consists of a generated chain and accept rate of the sampler
//' @examples
//' \dontrun{
//' N = 2000
//' sigma = c(.05, .5, 2, 16)
//' x0 = 25
//' rw1.c = MetropolisCpp(sigma[1],x0,N)
//' rw2.c = MetropolisCpp(sigma[2],x0,N)
//' rw3.c = MetropolisCpp(sigma[3],x0,N)
//' rw4.c = MetropolisCpp(sigma[4],x0,N)
//' #number of candidate points rejected
//' Rej = cbind(rw1.c$k, rw2.c$k, rw3.c$k, rw4.c$k)
//' Acc = round((N-Rej)/N,4)
//' rownames(Acc) = "Accept rates"
//' colnames(Acc) = paste("sigma",sigma)
//' Acc
//' }
//' @export
//[[Rcpp::export]]
List MetropolisCpp(double sigma, double x0, int N) {
  NumericVector x(N);
  x[0]=x0;
  NumericVector u(N);
  u=runif(N);
  int k=0;
  for(int i=1; i<N; i++){
    double y;
    y=rnorm(1,x[i-1],sigma)[0];
    if(u[i]<=(lap_f(y)/lap_f(x[i-1])))
      x[i]=y;
    else{
      x[i]=x[i-1];
      k++;
    }
  }
  List out;
  out["x"]=x;
  out["k"]=k;
  return out;
  //return x;
}