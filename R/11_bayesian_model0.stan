data {
  int<lower = 1> N;  // Total number of trials
  vector[N] y;  // Score in each trial

}

parameters {
  real mu;
  real<lower = 0> sigma;
  
}

model {

  // Priors:
  mu ~ normal(1, 1);
  sigma ~ lognormal(3, 1);

  // Likelihood:
  y ~ normal(mu, sigma);

}
