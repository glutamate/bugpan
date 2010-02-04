#include "poisson.h"
#include "math.h"

double likelihood(double rate, double tau, double baseline, double t0, 
		  double* spikes, int nspikes) {
  int i;

  double res = 0;

  for(i=0; i<nspikes;i++) {
    res+=log(r(rate, tau, baseline, t0, spikes[i]));
  }
  return res;
}

double r(double rate, double tau, double baseline, double t0, double t) {
  
  if(t<t0) {
    double tp = (t-t0)/tau;
    return (-tp)*exp(1+tp)*(rate-baseline)+baseline;
  } else {
    return baseline;
  }
}

double test_sum(double *p, int n) {
  int i;

  double res = 0;

  for(i=0; i<n;i++) {
    res+=p[i];
  }
  return res;

}

