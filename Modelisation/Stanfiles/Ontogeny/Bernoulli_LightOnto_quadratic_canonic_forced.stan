// Bernoulli environment only - quadratic in canonic form forced to be concave

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Environment ;
  vector[N] DBH ; // tree diameter as ontogeny 
  int<lower=1> N_e_p ; // n env predictions
  vector[N_e_p] Environmentp ; // environment of predictions
  int<lower=1> N_d_p ; // n DBH predictions
  vector[N_d_p] DBHp ; // DBH of predictions
}
parameters {
  real<upper=0> a ; // beta2 forced for a concave form
  real<lower=-100, upper=100> O ; // extremum : -beta1/(2*beta2)
  real gamma ; // alpha-(beta1^2/4*beta2)
  real iota ; // interaction
}
model {
  // Presence ~ bernoulli_logit(alpha + beta1*Environment + beta2*Environment.*Environment); // developped Likelihood
  Presence ~ bernoulli_logit( a * (Environment - (O + iota*DBH))^2 + gamma); // canonic Likelihood
  // Priors
  // a ~ cauchy(0,1); 
  // O ~ normal(0.5, 1); // O ~ cauchy(0.5, 1) ; O ~ uniform(0.5, 1);
}
generated quantities { // predictions
matrix<lower=0, upper=1>[N_e_p, N_d_p] p ;
// for(i in 1:n_e_p)
//   for(j in 1:n_d_p)
//     y_p[i,j] = inv_logit(a*(Environmentp[i] - O + iota*DBHp[j]))^2 + gamma); // plus couteux

for(i in 1:N_d_p)
p[,i] = inv_logit(a*(Environmentp - O + iota*DBHp[i])^2 + gamma); // i in column

// y_p[,i] = to_row_vector(inv_logit(a*(Environmentp - O + iota*log(DBHp[i]))^2 + gamma));


// For model evaluation with loo;
// vector[N] log_lik; // factors of the log-likelihood as a vector
// for (n in 1:N) {
  //   log_lik[n] = bernoulli_logit_lpmf(Presence[n] | a * (Environment[n] - (O + iota*DBH[n]))^2 + gamma);
  // } // to produce the log posterior density
}

