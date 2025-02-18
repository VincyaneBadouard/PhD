// Bernoulli environment only - quadratic

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Environment ;
  int<lower=1> Np ; // number of predictions 
  vector[Np] Environmentp ; // environment of predictions
}
parameters {
  real alpha ; // intercept
  real beta1 ; // ordinary term, sigmoidal slope
  real beta2 ; // quadratic term
}
model {
  Presence ~ bernoulli_logit(alpha + beta1*Environment + beta2*Environment.*Environment); // Likelihood
}
generated quantities {
  vector<lower=0, upper=1>[Np] p ;
  p = inv_logit(alpha + beta1 * Environmentp + beta2*Environmentp.*Environmentp) ; // predictions
  
  real o ; // Optimum
  o = -beta1/(2*beta2) ; // Optimum
  
  // For model evaluation with loo;
  // vector[N] log_lik; // factors of the log-likelihood as a vector
  // for (n in 1:N) {
    //   log_lik[n] = bernoulli_logit_lpmf(Presence[n] | alpha + beta1*Environment[n] + beta2*Environment[n].*Environment[n]);
    // } // to produce the log posterior density
}
