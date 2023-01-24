data {
  // Size integer
  int<lower = 1> n;  // Sample size
  int<lower = 1> n_predator; // Total number of unique predators globally
  // Vector data
  vector[n] biomass_flow;  // Target data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator; // Predator abundances
  vector[n] bodymass_mean_predator; // Predator bodymasses
  int pred_id[n]; // Predator ids to assess each alpha
}

transformed data {
   vector[n] log_biomass_flow = log(biomass_flow);
   vector[n] log_biomass_prey = log(biomass_prey);
   vector[n] log_abundance_predator = log(abundance_predator);
   vector[n] log_bodymass_predator = log(bodymass_mean_predator);
}

parameters {
  real c;
  real b;
  real<lower = 0> sigma; //
}

model {
  vector[n] mu_flow;
  
  // Priors:
  c ~ normal(1, 2);
  b ~ normal(0.5, 1.5);
  sigma ~ exponential(5);

  // Computing target's mean
   mu_flow = (c + (b * log_bodymass_predator)) + log_biomass_prey + log_abundance_predator;

  // Computing target
   log_biomass_flow ~ normal(mu_flow, sigma);

}

generated quantities {
    vector[n] log_biomass_flow_hat;
    vector[n] log_lik; //compute log-likelihood
    vector[n] y_rep; //replications from posterior predictive distribution
    real<lower = 0, upper = 1> Rsq_allometric;
  
  log_biomass_flow_hat = (c + (b * log_bodymass_predator)) + log_biomass_prey + log_abundance_predator;
  
  for (i in 1:n) {

    log_lik[i] = normal_lpdf(log_biomass_flow[i] | log_biomass_flow_hat[i], sigma);

    y_rep[i] = normal_rng(log_biomass_flow_hat[i], sigma);

  }

  Rsq_allometric = variance(log_biomass_flow_hat) / (variance(log_biomass_flow_hat) + square(sigma));

}
