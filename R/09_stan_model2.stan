data {
  // Size integer
  int<lower = 1> N;  // Sample size
  int<lower =1> npred; // Total number of predators, which is the number of different alphas
  // Vector data
  vector[N] y;  // Predicted data
  vector[N] biomass_prey; // Prey biomasses
  vector[N] biomass_predator; // Predator biomasses
  vector[N] bodymass_mean_predator; //  Predator bodymasses
  int pred_id[N]; // Predator id to assess each alpha
}

parameters {
  real a_pop; // Population-level alpha
  vector[npred] a_grp; // Group-level effect for each predator to be added to the Population-level alpha
  real a_sd;
  real<lower = 0> sigma; //
}

model {
  vector[N] mu;
  vector[N] alpha_spec;

  // Priors:
  a_pop ~ normal(1, 10);
  a_grp ~ normal(1, a_sd);
  a_sd ~ lognormal(1,1);
  sigma ~ lognormal(3, 1);

  // Likelihood:
  for(j in 1:npred){
    alpha_spec[j] = a_pop + a_grp[j];
  }

  for(i in 1:N)
   mu[i] = alpha_spec[pred_id[i]] + biomass_prey[i] + (biomass_predator[i] - bodymass_mean_predator[i]);
  
  y ~ normal(mu, sigma);

}
