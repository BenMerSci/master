data {
  int<lower = 1> N;  // Total number of trials
  vector[N] y;  // Score in each trial
  vector[N] biomass_prey; // prey biomasses
  vector[N] biomass_predator; // predator biomasses
  vector[N] bodymass_mean_predator; //  predator bodymasses
}

parameters {
  real loga;
  real<lower = 0> sigma;
  
}

model {
  vector[N] mu;

  // Priors:
  loga ~ normal(1, 1);
  sigma ~ lognormal(3, 1);

  // Likelihood:
  for(i in 1:N)
  mu[i] = loga + biomass_prey[i] + (biomass_predator[i] - bodymass_mean_predator[i]);
  y ~ normal(mu, sigma);

}
