data {
  // Size integer
  int<lower = 1> n;  // Sample size
  int<lower = 1> n_predator; // Total number of unique predators globally
  // Vector data
  vector[n] biomass_flow;  // Target data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator; // Predator abundances
  vector[n] bodymass_mean_predator;
  vector[n] bodymass_mean_prey;
  int pred_id[n]; // Predator ids to assess each alpha
}

transformed data {
   vector[n] log_biomass_flow = log(biomass_flow);
   vector[n] log_biomass_prey = log(biomass_prey);
   vector[n] log_abundance_predator = log(abundance_predator);
   vector[n] log_bodymass_predator = log(bodymass_mean_predator);
   vector[n] log_bodymass_prey = log(bodymass_mean_prey);

  vector[n] log_ppmr = log_bodymass_predator - log_bodymass_prey; 
}

parameters {
  real mu_alpha; // Population-level alpha
  vector[n_predator] alpha; // Group-level effect for each predator to be added to the Population-level alpha
  real<lower = 0> sd_alpha;
  real mu_ppmr;
  real<lower = 0> sigma_ppmr;
  real<lower = 0> sigma_flow; 
}

model {
  vector[n] mu_flow;
  vector[n] trait_match;
  

  // Priors:
  mu_alpha ~ normal(-4, 2);
  alpha ~ normal(mu_alpha, sd_alpha);
  sd_alpha ~ exponential(3);
  mu_ppmr ~ normal(3,3);
  sigma_ppmr ~ normal(3,5);
  sigma_flow ~ exponential(5);

  trait_match = alpha[pred_id] + -1/square(sigma_ppmr) * square(log_ppmr - mu_ppmr);

  // Computing target's mean
   mu_flow = trait_match + log_biomass_prey + log_abundance_predator;

  // Computing target
   log_biomass_flow ~ normal(mu_flow, sigma_flow);

}

generated quantities {
    vector[n] trait_match;
    vector[n] log_biomass_flow_hat;
    vector[n] log_lik; //compute log-likelihood
    vector[n] y_rep; //replications from posterior predictive distribution
    real<lower = 0, upper = 1> Rsq_tm;
  
  trait_match = alpha[pred_id] + -1/square(sigma_ppmr) * square(log_ppmr - mu_ppmr);

  log_biomass_flow_hat = trait_match + log_biomass_prey + log_abundance_predator;
  
  for (i in 1:n) {

    log_lik[i] = normal_lpdf(log_biomass_flow[i] | log_biomass_flow_hat[i], sigma_flow);

    y_rep[i] = normal_rng(log_biomass_flow_hat[i], sigma_flow);

  }

  Rsq_tm = variance(log_biomass_flow_hat) / (variance(log_biomass_flow_hat) + square(sigma_flow));

}
