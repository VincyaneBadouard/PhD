// Bernoulli environment only - quadratic in canonic form forced to be concave

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
  real<upper=0> a ; // beta2 forced for a concave form
  real O ; // extremum : -beta1/(2*beta2)
  real gamma ; // alpha-(beta1^2/4*beta2)
  real iota ; // interaction
}
model {
  // Presence ~ bernoulli_logit(alpha + beta1*Environment + beta2*Environment.*Environment); // developped Likelihood
    Presence ~ bernoulli_logit( a * (Environment - (O + iota*DBH))^2 + gamma); // canonic Likelihood
      // Priors
  a ~ cauchy(0,1); 
  O ~ normal(0.5, 1); // O ~ cauchy(0.5, 1) ; O ~ uniform(0.5, 1);
}
generated quantities {
  vector<lower=0, upper=1>[Np] p ;
  p = inv_logit(a * (Environmentp - (O + iota*DBHp))^2 + gamma) ; // predictions
  
  // For model evaluation with loo;
  vector[N] log_lik; // factors of the log-likelihood as a vector
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(Presence[n] | a * (Environment[n] - (O + iota*DBH[n]))^2 + gamma);
  } // to produce the log posterior density
}

