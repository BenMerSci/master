data {
  // Size integer
  int<lower = 1> n;  // Sample size
  int<lower = 1> n_predator; // Total number of unique predators globally
  // Vector data
  vector[n] biomass_flow;  // Target data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator; // Predator abundances
  int pred_id[n]; // Predator ids to assess each alpha
}

transformed data {
   vector[n] log_biomass_flow = log(biomass_flow);
   vector[n] log_biomass_prey = log(biomass_prey);
   vector[n] log_abundance_predator = log(abundance_predator);
}

parameters {
  real mu_alpha; // Population-level alpha
  vector[n_predator] alpha; // Group-level effect for each predator to be added to the Population-level alpha
  real<lower = 0> sd_alpha;
  real<lower = 0> sigma; //
}

model {
  vector[n] mu_flow;
  
  // Priors:
  mu_alpha ~ normal(0, 1);
  alpha ~ normal(mu_alpha, sd_alpha);
  sd_alpha ~ exponential(3);
  sigma ~ exponential(3);

  // Computing target's mean
   mu_flow = alpha[pred_id] + log_biomass_prey + log_abundance_predator;

  // Computing target
   log_biomass_flow ~ normal(mu_flow, sigma);

}

generated quantities {
    vector[n] log_biomass_flow_hat;
    vector[n] log_lik; //compute log-likelihood
    vector[n] y_rep; //replications from posterior predictive distribution
    real<lower = 0, upper = 1> Rsq_2;
  
  log_biomass_flow_hat = alpha[pred_id] + log_biomass_prey + log_abundance_predator;
  
  for (i in 1:n) {

    log_lik[i] = normal_lpdf(log_biomass_flow[i] | log_biomass_flow_hat[i], sigma);

    y_rep[i] = normal_rng(log_biomass_flow_hat[i], sigma);

  }

  Rsq_2 = variance(log_biomass_flow_hat) / (variance(log_biomass_flow_hat) + square(sigma));

}
