// Bernoulli environment + ontogeny interaction - affine

data {
  int<lower=1> N ; // obs
  array[N] int<lower=0, upper=1> Presence ;
  vector[N] Environment ;
  vector[N] DBH ; // tree diameter as ontogeny 
  int<lower=1> Np ; // number of predictions 
  vector[Np] Environmentp ; // environment of predictions
  vector[Np] DBH15 ; // tree diameter of predictions 
  vector[Np] DBH510 ; // tree diameter of predictions 
  vector[Np] DBH1020 ; // tree diameter of predictions 
  vector[Np] DBH20 ; // tree diameter of predictions 
  
  n_e_p; // # valeurs env pred
  n_d_p; // # valeurs dbh pred
  matrix<lower=0, upper=1>[n_e_p, n_d_p];
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

for(i in 1:n_e_p)
    for(j in 1:n_d_p)
      y_p[i,j] = inv_logit(alpha*(x_p[i] - o + iota*DBH[j]))^2 + gamma); // plus couteux
      
        for(i in 1:n_d_p)
    y_p[,i] = inv_logit(a*(x_p-o + iota*log(dbh[i]))^2+gamma); # i in column
    
    // y_p[,i] = to_row_vector(inv_logit(a*(x_p-o + iota*log(dbh[i]))^2+gamma));

// For model evaluation with loo;
// vector[N] log_lik; // factors of the log-likelihood as a vector
// for (n in 1:N) {
  //   log_lik[n] = bernoulli_logit_lpmf(Presence[n] | alpha + beta1*Environment[n] + iota*Environment[n].*DBH[n]);
  // } // to produce the log posterior density
  
}

