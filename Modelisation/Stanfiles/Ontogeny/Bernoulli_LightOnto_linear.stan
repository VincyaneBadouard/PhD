// Bernoulli environment + ontogeny interaction - affine

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
  real alpha ; // intercept
  real beta1 ; // sigmoidal slope
  real iota ; // interaction
}
model {
  Presence ~ bernoulli_logit(alpha + beta1*Environment + iota*Environment.*DBH) ; // Likelihood
}
generated quantities { // predictions
  matrix<lower=0, upper=1>[N_e_p, N_d_p] p ;

// for(i in 1:N_e_p)
// for(j in 1:N_d_p)
// y_p[i,j] = inv_logit(alpha + beta1*Environmentp[i] + iota*Environmentp[i].*DBHp[j]); // plus couteux

for(i in 1:N_d_p)
p[,i] = inv_logit(alpha + beta1*Environmentp + iota*Environmentp.*DBHp[i]); // i in column

// y_p[,i] = to_row_vector(alpha + beta1*Environmentp + iota*Environmentp.*DBHp[i]);

// For model evaluation with loo;
// vector[N] log_lik; // factors of the log-likelihood as a vector
// for (n in 1:N) {
  //   log_lik[n] = bernoulli_logit_lpmf(Presence[n] | alpha + beta1*Environment[n] + iota*Environment[n].*DBH[n]);
  // } // to produce the log posterior density
  
}

