// Bernoulli environment only - quadratic in canonic form forced to be concave

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Environment ;
  int<lower=1> Np ; // number of predictions 
  vector[Np] Environmentp ; // environment of predictions
}
parameters {
  real<lower=-300, upper=-0.02> a ; // beta2 forced for a concave form
  real<lower=-7, upper=0.5> O ; // extremum : -beta1/(2*beta2)
  real<lower=-2, upper=200> gamma ; // alpha-(beta1^2/4*beta2)
}
model {
  // Presence ~ bernoulli_logit(alpha + beta1*Environment + beta2*Environment.*Environment); // developped Likelihood
  Presence ~ bernoulli_logit(a * (Environment - O)^2 + gamma); // canonic Likelihood
}
generated quantities {
  vector<lower=0, upper=1>[Np] p = inv_logit(a * (Environmentp - O)^2 + gamma) ; // predictions
  
  // For model evaluation with loo;
  // vector[N] log_lik; // factors of the log-likelihood as a vector
  // for (n in 1:N) {
  //   log_lik[n] = bernoulli_logit_lpmf(Presence[n] | a * (Environment[n] - O)^2 + gamma);
  // } // to produce the log posterior density
}

