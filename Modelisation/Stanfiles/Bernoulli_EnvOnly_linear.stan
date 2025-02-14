// Bernoulli environment only - affine

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Environment ;
}
parameters {
  real alpha ; // intercept
  real beta1 ; // sigmoidal slope
}
model {
 target += bernoulli_logit_lpmf(Presence | alpha + beta1*Environment) ; // Likelihood
}
generated quantities {
  vector<lower=0, upper=1>[N] p ;
  p = inv_logit(alpha + beta1 * Environment) ; // predictions
  
    // For model evaluation with loo;
  vector[N] log_lik; // factors of the log-likelihood as a vector
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(Presence[n] | alpha + beta1*Environment[n]);
  } // to produce the log posterior density
  
}

