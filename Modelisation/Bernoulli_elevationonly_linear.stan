// Bernoulli elevation only - linear

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Elevation ;
}
parameters {
  real alpha ; // intercept
  real tau1 ; // topo sigmoidal slope
}
model {
 target += bernoulli_logit_lpmf(Presence | alpha + tau1*Elevation) ;
}
generated quantities {
  vector<lower=0, upper=1>[N] p ;
  p = inv_logit(alpha + tau1 * Elevation) ;


}

