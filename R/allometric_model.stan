data {
  // Size integer
  int<lower = 1> n;  // Sample size
  int<lower = 1> n_predator; // Total number of unique predators globally
  // Vector data
  vector[n] pred_flow;  // Target data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator; // Predator abundances
  vector[n] bodymass_mean_predator; // Predator bodymasses
  int pred_id[n]; // Predator ids to assess each alpha
}

transformed data {
   vector[n] log_pred_flow = log(pred_flow);
   vector[n] log_biomass_prey = log(biomass_prey);
   vector[n] log_abundance_predator = log(abundance_predator);
   vector[n] log_bodymass_predator = log(bodymass_mean_predator);
}

parameters {
  //real a_pop; // Population-level alpha
  //vector[n_predator] a_grp; // Group-level effect for each predator to be added to the Population-level alpha
  //real<lower = 0> a_sd;
  real c;
  real b;
  real<lower = 0> sigma; //
}

model {
  vector[n] mu;
  
  // Priors:
  //a_pop ~ normal(-4, 2);
  //a_grp ~ normal(a_pop, a_sd);
  //a_sd ~ exponential(3);
  c ~ normal(1, 2);
  b ~ normal(0.5, 1.5);
  sigma ~ exponential(5);

  // Computing target's mean
   mu = (c + (b * log_bodymass_predator)) + log_biomass_prey + log_abundance_predator;

  // Computing target
   log_pred_flow ~ normal(mu, sigma);

}

//generated quantities {
//    vector[n] log_pred_flow_hat;
//    vector[n] log_lik; //compute log-likelihood
//    vector[n] y_rep; //replications from posterior predictive distribution
//    real<lower = 0, upper = 1> Rsq_2_allo;
//  
//  log_pred_flow_hat = a_grp[pred_id] + log_biomass_prey + log_abundance_predator;
//  
//  for (i in 1:n) {
//
//    log_lik[i] = normal_lpdf(log_pred_flow[i] | log_pred_flow_hat[i], sigma);
//
//    y_rep[i] = normal_rng(log_pred_flow_hat[i], sigma);
//
//  }
//
//  Rsq_2_allo = variance(log_pred_flow_hat) / (variance(log_pred_flow_hat) + square(sigma));
//
//}
