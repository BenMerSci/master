data { 
  // Size integer
  int<lower = 1> n;  // Sample size
  int<lower = 1> n_predator; // Total number of predators globally, which is the number of different alphas
  // Vector data
  vector[n] biomass_flow;  // Predicted data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator;
  int pred_id[n]; // predator id to assess each alpha
  vector[n] sum_biomass_prey; // Total biomass consumption
}

transformed data {
  vector[n] log_biomass_flow = log(biomass_flow);
  vector[n] log_biomass_prey = log(biomass_prey);
  vector[n] log_abundance_predator = log(abundance_predator);
  vector[n] log_sum_biomass_prey = log(sum_biomass_prey);
}

parameters {
  real mu_alpha; // Population-level alpha
  vector[n_predator] alpha; // Group-level effect for each predator to be added to the Population-level alpha
  real<lower = 0> sd_alpha;
  real mu_ht; // Population-level ht
  vector[n_predator] ht; // Group-level effect for each predator to be added to the Population-level ht
  real<lower = 0> sd_ht;
  real<lower = 0> sigma; //
} 

model {
  vector[n] mu_flow;
  vector[n] pred_factor;

  // Priors:
  mu_alpha ~ normal(-5,2);
  alpha ~ normal(mu_alpha, sd_alpha);
  sd_alpha ~ exponential(1);
  mu_ht ~ normal(-3,2);
  ht ~ normal(mu_ht, sd_ht);
  sd_ht ~ exponential(1);
  sigma ~ exponential(5);

  // Computing predators part for the numerator and denominator

   pred_factor = alpha[pred_id] + log_abundance_predator;
   
   mu_flow = pred_factor + log_biomass_prey - log1p_exp(ht[pred_id] + pred_factor + log_sum_biomass_prey);

  log_biomass_flow ~ normal(mu_flow, sigma);

}

generated quantities {
    vector[n] pred_factor;
    vector[n] log_biomass_flow_hat;
    vector[n] log_lik; //compute log-likelihood
    vector[n] y_rep; //replications from posterior predictive distribution
    real<lower = 0, upper = 1> Rsq_4;

  //alpha_spec = a_pop + a_grp[pred_id];
  pred_factor = alpha[pred_id] + log_abundance_predator;

  log_biomass_flow_hat = pred_factor + log_biomass_prey - log1p_exp(ht[pred_id] + pred_factor + log_sum_biomass_prey);
      
  for (i in 1:n) {

    log_lik[i] = normal_lpdf(log_biomass_flow[i] | log_biomass_flow_hat[i], sigma);

    y_rep[i] = normal_rng(log_biomass_flow_hat[i], sigma);

  }

  Rsq_4 = variance(log_biomass_flow_hat) / (variance(log_biomass_flow_hat) + square(sigma));

}
