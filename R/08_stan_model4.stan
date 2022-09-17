data { 
  // Size integer
  int<lower = 1> n;  // Sample size
  int<lower = 1> n_predator; // Total number of predators globally, which is the number of different alphas
  // Vector data
  vector[n] pred_flow;  // Predicted data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator;
  int pred_id[n]; // predator id to assess each alpha
  vector[n] degree_predator; // Degrees of each predator
  vector[n] sum_biomass_prey; // Total biomass consumption
  vector[n] h_j; // Predator handling time
    //array[N] int<lower=1,upper=npred> pred_id; Andrew example for cmdstanr
}

transformed data {
  vector[n] log_pred_flow = log(pred_flow);
  vector[n] log_biomass_prey = log(biomass_prey);
  vector[n] log_abundance_predator = log(abundance_predator);
  vector[n] log_degree_predator = log(degree_predator);
  vector[n] log_sum_biomass_prey = log(sum_biomass_prey);
  vector[n] log_h_j = log(h_j);
}

parameters {
  real a_pop; // Population-level alpha
  vector[n_predator] a_grp; // Group-level effect for each predator to be added to the Population-level alpha
  real<lower = 0> a_sd;
  real<lower = 0> sigma; // 
} 

model {
  vector[n] mu;
  vector[n_predator] alpha_spec;
  vector[n] pred_factor;

  // Priors:
  a_pop ~ normal(1, 5);
  a_grp ~ normal(2, a_sd);
  a_sd ~ exponential(0.16);
  sigma ~ exponential(0.16);

  // Likelihood:
  // Computing each alpha by predator
   alpha_spec = a_pop + a_grp;

  // Computing predators part for the numerator and denominator
   pred_factor = (alpha_spec[pred_id] - log_degree_predator) + log_abundance_predator;

   mu = pred_factor + log_biomass_prey - log1p_exp(log_h_j + pred_factor + log_sum_biomass_prey);

  log_pred_flow ~ normal(mu, sigma);

}
