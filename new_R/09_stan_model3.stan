data { 
  // Size integer
  int<lower = 1> N;  // Total number of trials
  int<lower =1> npred; // Total number of predators, which is the number of different alphas
  // Vector data
  vector[N] y;  // Score in each trial
  vector[N] biomass_prey; // prey biomasses
  vector[N] biomass_predator; // predator biomasses
  vector[N] bodymass_mean_predator; //  predator bodymasses
  int pred_id[N]; // predator Id to assess each alpha
  vector[N] degree_predator; // degrees of each predator
}

parameters {
  vector[npred] alpha; // Specific alpha for each predator to add to the global alpha
  real a_mu;
  real a_sd;
  real<lower = 0> sigma; // 
}

model {
  vector[N] mu;
  vector[N] alpha_spec;

  // Priors:
  alpha ~ normal(a_mu,a_sd);
  a_mu ~ normal(1,1);
  a_sd ~ lognormal(1,1);
  sigma ~ lognormal(3, 1);

  // Likelihood:
  for(j in 1:N){
    alpha_spec[j] = alpha[pred_id[j]];
  }

  for(i in 1:N)
   mu[i] = (alpha_spec[i]-degree_predator[i]) + biomass_prey[i] + (biomass_predator[i] - bodymass_mean_predator[i]);

  y ~ normal(mu, sigma);
}
