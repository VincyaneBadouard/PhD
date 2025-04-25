// Bernoulli environment (light + topo) + ontogeny 
// Estimates the parameters of the developed form (∝ , β1, β2) of the quadratic equation,
// constraining them in concave form (β2 < 0; β1 ⋲ [-7*2β2; 0]).
// We define the concave form parameters (a, O, gamma) in transformed parameters,
// and the equation remains a(x - O)2 + gamma.

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Light ;
  vector[N] Topography ;
  vector[N] DBH ;
  int<lower=1> N_p ; // N predictions
  array[N_p] int<lower=1, upper=N> preds ; // predictions index
}
transformed data {
  real adj = N * 1.0 / sum(Presence); // inverse of species relative abundance
}
/*parameters {
real<lower=-300, upper=-0.02> a ; // beta2<0 : forced for a concave form  
real<lower=-7, upper=0> O ; // extremum : -beta1/(2*beta2) 0.5
real<lower=-2, upper=300> gamma ; // alpha-(beta1^2/4*beta2)
}*/
parameters {
  real<lower=-10, upper=10> beta2_p;  
  // real<lower=-300, upper=-0.02> beta2;
  real<lower=7*2*-exp(beta2_p), upper=0> beta1;
  real alpha;
  real tau; // slope of the topography effect
  real iota; // ontogeny effect
}
transformed parameters {
  real beta2 = -exp(beta2_p); // beta2<0 : forced for a concave form
  real a = beta2;
  real O = -beta1/(2*beta2);
  real gamma = alpha-beta1^2/(4*beta2);
}
model {
  // Presence ~ bernoulli_logit(alpha + beta1*Environment + beta2*Environment.*Environment); // developped Likelihood
  Presence ~ bernoulli_logit(a * (Light - (O + iota*DBH))^2 + gamma + tau*Topography); // canonic Likelihood (affine)
  // a * (Light - O)^2 + gamma + a * (Topography - O)^2 //quadra
  
  // a * (Environment - O)^2 + gamma_p
  // gamma_p = gamma0 + tau*topo
}
generated quantities { // predictions
vector<lower=0, upper=1>[N_p] p ;
p = inv_logit(a * (Light[preds] - (O + iota*DBH[preds]))^2 + gamma + tau*Topography[preds]); // i in column *adj

// p[,i] = to_row_vector(inv_logit(a*(Environmentp - O + iota*(DBHp[i]))^2 + gamma));  

// For model evaluation with loo;
// vector[N] log_lik; // factors of the log-likelihood as a vector
// for (n in 1:N) {
  //   log_lik[n] = bernoulli_logit_lpmf(Presence[n] | a * (Environment[n] - O)^2 + gamma);
  // } // to produce the log posterior density
}

