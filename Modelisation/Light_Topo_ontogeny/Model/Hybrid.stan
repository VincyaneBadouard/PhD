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
  int<lower=1> N_L_p ; // n light predictions
  vector[N_L_p] Lightp ; // Light environment of predictions
  // int<lower=1> N_T_p ; // n topo predictions
  real Topographyp ; // Topography environment of predictions
  int<lower=1> N_D_p ; // n DBH predictions
  vector[N_D_p] DBHp ; // DBH of predictions
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
matrix<lower=0, upper=1>[N_L_p, N_D_p] p ;
// for(i in 1:n_L_p)
//   for(j in 1:n_T_p)
//     p[i,j] = inv_logit(a*(Lightp[i] - O)^2 + gamma + tau*Topographyp[j]); // plus couteux

for(i in 1:N_D_p)
p[,i] = inv_logit(a*(Lightp - (O + iota*DBHp[i]))^2 + gamma + tau*Topographyp); // i in column, *adj

// p[,i] = to_row_vector(inv_logit(a*(Environmentp - O + iota*(DBHp[i]))^2 + gamma));  

// For model evaluation with loo;
// vector[N] log_lik; // factors of the log-likelihood as a vector
// for (n in 1:N) {
  //   log_lik[n] = bernoulli_logit_lpmf(Presence[n] | a * (Environment[n] - O)^2 + gamma);
  // } // to produce the log posterior density
}

