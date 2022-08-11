data {
  // Size integer
  int<lower = 1> N;  // Sample size
  int<lower = 1> npred; // Total number of unique predators globally
  // Vector data
  vector[N] y;  // Target data
  vector[N] biomass_prey; // Prey biomasses
  vector[N] abundance_pred; // Predator abundances
  int pred_id[N]; // Predator ids to assess each alpha
}

transformed data {
   vector[N] log_y = log(y);
   vector[N] log_biomass_prey = log(biomass_prey);
   vector[N] log_abundance_pred = log(abundance_pred);
}

parameters {
  real a_pop; // Population-level alpha
  vector[npred] a_grp; // Group-level effect for each predator to be added to the Population-level alpha
  real<lower = 0> a_sd;
  real<lower = 0> sigma; //
}

model {
  vector[N] mu;
  vector[npred] alpha_spec;

  // Priors:
  a_pop ~ normal(1, 10);
  a_grp ~ normal(1, a_sd);
  a_sd ~ exponential(0.2);
  sigma ~ exponential(0.2);

  // Likelihood:
  // Computing each alpha by predator
   alpha_spec = a_pop + a_grp;
  
  // Computing target's mean
   mu = alpha_spec[pred_id] + log_biomass_prey + log_abundance_pred;

  // Computing target
   log_y ~ normal(mu, sigma);

}
