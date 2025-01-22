// Bernoulli environment only - quadratic

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Environment ;
}
parameters {
  real alpha ; // intercept
  real beta1 ; // topo sigmoidal slope
  real beta2 ; // topo quadratic form
}
model {
  target += bernoulli_logit_lpmf(Presence | alpha + beta1*Environment + beta2*Environment.*Environment) ; // Likelihood
}
generated quantities {
  vector<lower=0, upper=1>[N] p ;
  p = inv_logit(alpha + beta1 * Environment + beta2*Environment.*Environment) ; // predictions
  
  real o ; // Optimum
  o = -beta1/(2*beta2) ; // Optimum
}
