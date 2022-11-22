data { 
  // Size integer
  int<lower = 1> n;  // Sample size
  int<lower = 1> n_predator; // Total number of predators globally, which is the number of different alphas
  // Vector data
  vector[n] pred_flow;  // Predicted data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator;
  int pred_id[n]; // predator id to assess each alpha
  //vector[n] degree_predator; // Degrees of each predator
  vector[n] sum_biomass_prey; // Total biomass consumption
}

transformed data {
  vector[n] log_pred_flow = log(pred_flow);
  vector[n] log_biomass_prey = log(biomass_prey);
  vector[n] log_abundance_predator = log(abundance_predator);
  //vector[n] log_degree_predator = log(degree_predator);
  vector[n] log_sum_biomass_prey = log(sum_biomass_prey);
}

parameters {
  real a_pop; // Population-level alpha
  vector[n_predator] a_grp; // Group-level effect for each predator to be added to the Population-level alpha
  real<lower = 0> a_sd;
  real<lower = 0> sigma; // 
  vector[n_predator] h_j;
} 

model {
  vector[n] mu;
  vector[n] pred_factor;

  // Priors:
  a_pop ~ normal(-4,2);
  a_grp ~ normal(a_pop, a_sd);
  a_sd ~ exponential(3);
  sigma ~ exponential(5);
  h_j ~ normal(-1,6);

  // Computing predators part for the numerator and denominator

   pred_factor = a_grp[pred_id] + log_abundance_predator;
   
   mu = pred_factor + log_biomass_prey - log1p_exp(h_j[pred_id] + pred_factor + log_sum_biomass_prey);

  log_pred_flow ~ normal(mu, sigma);

}

generated quantities {
    //vector[n] alpha_spec;
    vector[n] pred_factor;
    vector[n] log_pred_flow_hat;
    vector[n] log_lik; //compute log-likelihood
    vector[n] y_rep; //replications from posterior predictive distribution
    real<lower = 0, upper = 1> Rsq_4;

  //alpha_spec = a_pop + a_grp[pred_id];
  pred_factor = a_grp[pred_id] + log_abundance_predator;

  log_pred_flow_hat = pred_factor + log_biomass_prey - log1p_exp(h_j[pred_id] + pred_factor + log_sum_biomass_prey);
      
  for (i in 1:n) {

    log_lik[i] = normal_lpdf(log_pred_flow[i] | log_pred_flow_hat[i], sigma);

    y_rep[i] = normal_rng(log_pred_flow_hat[i], sigma);

  }

  Rsq_4 = variance(log_pred_flow_hat) / (variance(log_pred_flow_hat) + square(sigma));

}
