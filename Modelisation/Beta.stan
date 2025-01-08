// Beta model
// https://github.com/daltonhance/stan_beta_reg
// https://m-clark.github.io/models-by-example/bayesian-beta-regression.html


data {
  int<lower=1> N; // observations nbr
  int<lower=1> K; // ncol(X)
  int<lower=1> J;
  vector<lower=0,upper=1>[N] y; // response var
  matrix[N,K] X; // var exp 1 matrix
  matrix[N,J] Z; // var exp 2 matrix
}

parameters {
  vector[K] beta; # parameter of the X Var
  vector[J] gamma;
}

transformed parameters{
  vector<lower=0,upper=1>[N] mu;    // transformed linear predictor for mean of beta distribution
  vector<lower=0>[N] phi;           // transformed linear predictor for precision of beta distribution (dispersion parmeter)
  vector<lower=0>[N] A;             // parameter for beta distn
  vector<lower=0>[N] B;             // parameter for beta distn

  for (i in 1:N) {
    mu[i]  = inv_logit(X[i,] * beta);   // invlogit(linear predictor)
    phi[i] = exp(Z[i,] * gamma); // dispersion parmeter
  }

  A = mu .* phi; // parameter for beta distn
  B = (1.0 - mu) .* phi; // parameter for beta distn
}

model {
  // priors

  // likelihood
  y ~ beta(A, B); // beta distn of A and B parameters
}

generated quantities{
  vector[N] log_lik;
  vector[N] log_lik_rep;
  vector<lower=0,upper=1>[N] y_rep;
  real total_log_lik;
  real total_log_lik_rep;
  
  int<lower=0, upper=1> p_omni;

  for (n in 1:N) {
    log_lik[n] = beta_lpdf(y[n] | A[n], B[n]);
    y_rep[n] = beta_rng(A[n], B[n]);
    log_lik_rep[n] = beta_lpdf(y_rep[n] | A[n], B[n]);
  }

  total_log_lik = sum(log_lik);
  total_log_lik_rep = sum(log_lik_rep);

  p_omni = (total_log_lik_rep > total_log_lik);
}
