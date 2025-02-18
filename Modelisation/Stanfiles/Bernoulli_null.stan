// Bernoulli - Null model

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
}
parameters {
  real alpha ; // intercept
}
model {
  Presence ~ bernoulli_logit(alpha) ; // Likelihood
}
generated quantities {
  vector<lower=0, upper=1>[N] p ;
  for(n in 1:N)
  p[n] = inv_logit(alpha) ; // predictions
  
  // For model evaluation with loo;
  vector[N] log_lik; // factors of the log-likelihood as a vector
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(Presence[n] | alpha);
  } // to produce the log posterior density
}
