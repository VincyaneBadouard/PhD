// Bernoulli environment + ontogeny interaction - affine

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Environment ;
  vector[N] DBH ; // tree diameter as ontogeny 
  int<lower=1> Np ; // number of predictions 
  vector[Np] Environmentp ; // environment of predictions
  vector[Np] DBHp ; // tree diameter of predictions 
}
parameters {
  real alpha ; // intercept
  real beta1 ; // sigmoidal slope
  real iota ; // interaction
}
model {
  Presence ~ bernoulli_logit(alpha + beta1*Environment + iota*Environment.*DBH) ; // Likelihood
}
generated quantities {
  vector<lower=0, upper=1>[Np] p ;
  p = inv_logit(alpha + beta1 * Environmentp + iota*Environmentp.*DBHp) ; // predictions
  
  // For model evaluation with loo;
  vector[N] log_lik; // factors of the log-likelihood as a vector
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(Presence[n] | alpha + beta1*Environment[n] + iota*Environment[n].*DBH[n]);
  } // to produce the log posterior density
  
}

