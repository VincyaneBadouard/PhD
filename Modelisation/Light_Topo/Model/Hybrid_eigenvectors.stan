// Bernoulli light + topo + eigenvectors 
// Estimates the parameters of the developed form (∝ , β1, β2) of the quadratic equation,
// constraining them in concave form (β2 < 0; β1 ⋲ [-7*2β2; 0]).
// We define the concave form parameters (a, O, gamma) in transformed parameters,
// and the equation remains a(x - O)2 + gamma.

data {
  int<lower=0,upper=1> Autocor;
  int<lower=1> N ; // observations
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Light ;
  vector[N] Topography ;
  int<lower=1> N_L_p ; // n light predictions
  vector[N_L_p] Lightp ; // Light environment of predictions
  real Topographyp ;  // Topographic environment of predictions
  int<lower=0> K ; // Nbr of eigenvectors
  matrix[N, K] Spatial ; // eigenvectors matrix
  // matrix[1, K] Spatialp ; // eigenvectors matrix
  
}
parameters {
  real<lower=-10, upper=10> beta2_p;  
  // real<lower=-300, upper=-0.02> beta2;
  real<lower=7*2*-exp(beta2_p), upper=0> beta1;
  real alpha;
  real tau; // slope of the topography effect
  vector[K] psi; // spatial effect (i.e. spatial autocorrelation)
}
transformed parameters {
  real beta2 = -exp(beta2_p); // beta2<0 : forced for a concave form
  real a = beta2;
  real O = -beta1/(2*beta2);
  real gamma = alpha-beta1^2/(4*beta2);
}
model {
  // Presence ~ bernoulli_logit(alpha + beta1*Environment + beta2*Environment.*Environment); // developped Likelihood
  if (Autocor == 1) {
    Presence ~ bernoulli_logit(a * (Light - O)^2 + gamma + tau*Topography + Spatial*psi); // canonic Likelihood (affine)
  } else {
    Presence ~ bernoulli_logit(a * (Light - O)^2 + gamma + tau*Topography); // canonic Likelihood (affine)
  }  
}
generated quantities { // predictions
vector<lower=0, upper=1>[N_L_p] p ;

p = inv_logit(a*(Lightp - O)^2 + gamma + tau*Topographyp);

// For model evaluation with loo;
// vector[N] log_lik; // factors of the log-likelihood as a vector
// for (n in 1:N) {
  //   log_lik[n] = bernoulli_logit_lpmf(Presence[n] | a * (Environment[n] - O)^2 + gamma);
  // } // to produce the log posterior density
}

