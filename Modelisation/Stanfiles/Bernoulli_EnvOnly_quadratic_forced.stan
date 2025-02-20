// Bernoulli environment only - quadratic - concave form forced

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Environment ;
  int<lower=1> Np ; // number of predictions 
  vector[Np] Environmentp ; // environment of predictions
}
parameters {
  real alpha ; // intercept
  // A concave is obtain when the beta2 parameter is strictly negative:
  real beta1 ; // sigmoidal slope
  real<upper=-10e-10> beta2 ; // quadratic form
}
model {
  Presence ~ bernoulli_logit(alpha + beta1*Environment + beta2*Environment.*Environment) ; // Likelihood
}
generated quantities {
  vector<lower=0, upper=1>[Np] p ;
  p = inv_logit(alpha + beta1 * Environmentp + beta2*Environmentp.*Environmentp) ; // predictions
  
  real o ; // Optimum
  o = -beta1/(2*beta2) ; // Optimum
  
  // For model evaluation with loo;
  vector[N] log_lik; // factors of the log-likelihood as a vector
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(Presence[n] | alpha + beta1*Environment[n] + beta2*Environment[n].*Environment[n]);
  } // to produce the log posterior density
}
