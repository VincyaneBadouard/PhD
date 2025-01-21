// Bernoulli elevation only - quadratic

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Elevation ;
}
parameters {
  real alpha ; // intercept
  real tau1 ; // topo sigmoidal slope
  real tau2 ; // topo quadratic form
}
model {
  target += bernoulli_logit_lpmf(Presence | alpha + tau1*Elevation + tau2*Elevation.*Elevation) ;
}
generated quantities {
  vector<lower=0, upper=1>[N] p ;
  p = inv_logit(alpha + tau1 * Elevation + tau2*Elevation.*Elevation) ;
}


