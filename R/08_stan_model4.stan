data { 
  // Size integer
  int<lower = 1> N;  // Sample size
  int<lower = 1> npred; // Total number of predators globally, which is the number of different alphas
  // Vector data
  vector[N] y;  // Predicted data
  vector[N] biomass_prey; // Prey biomasses
  vector[N] abundance_pred;
  int pred_id[N]; // predator id to assess each alpha
  vector[N] degree_predator; // Degrees of each predator
  vector[N] sum_biomass_prey; // Total biomass consumption
  vector[N] h_j; // Predator handling time
    //array[N] int<lower=1,upper=npred> pred_id; Andrew example for cmdstanr
}

transformed data {
  vector[N] log_y = log(y);
  vector[N] log_biomass_prey = log(biomass_prey);
  vector[N] log_abundance_pred = log(abundance_pred);
  vector[N] log_degree_predator = log(degree_predator);
  vector[N] log_sum_biomass_prey = log(sum_biomass_prey);
  vector[N] log_h_j = log(h_j);
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
  vector[N] pred_factor;

  // Priors:
  a_pop ~ normal(1, 5);
  a_grp ~ normal(2, a_sd);
  a_sd ~ exponential(0.16);
  sigma ~ exponential(0.16);

  // Likelihood:
  // Computing each alpha by predator
   alpha_spec = a_pop + a_grp;

  // Computing predators part for the numerator and denominator
   pred_factor = (alpha_spec[pred_id] - log_degree_predator) + log_abundance_pred;

   mu = pred_factor + log_biomass_prey - log1p_exp(log_h_j + pred_factor + log_sum_biomass_prey);

  log_y ~ normal(mu, sigma);

}
