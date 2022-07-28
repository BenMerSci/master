data {
  // Size integer
  int<lower = 1> N;  // Sample size
  // Vector data
  vector[N] y;  // Predicted data
  vector[N] biomass_prey; // Prey biomasses
  vector[N] biomass_predator; // Predator biomasses
  vector[N] bodymass_mean_predator; //  Predator bodymasses
}

parameters {
  real a_pop; // Population-level alpha
  real<lower = 0> sigma;
  
}

model {
  vector[N] mu;

  // Priors:
  a_pop ~ normal(1, 10);
  sigma ~ lognormal(3, 1);

  // Likelihood:
  for(i in 1:N)
  mu[i] = a_pop + biomass_prey[i] + (biomass_predator[i] - bodymass_mean_predator[i]);
  
  y ~ normal(mu, sigma);

}
