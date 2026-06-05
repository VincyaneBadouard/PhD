data {
  int<lower=0> N;
  vector[N] d;
  vector[N] h;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  h ~ normal((alpha*d) ./ (beta+d), sigma);
}

