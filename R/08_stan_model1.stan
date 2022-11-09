data {
  // Size integer
  int<lower = 1> n;  // Sample size
  // Vector data
  vector[n] pred_flow;  // Target data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator; // Predator abundances
}

transformed data {
   vector[n] log_pred_flow = log(pred_flow);
   vector[n] log_biomass_prey = log(biomass_prey);
   vector[n] log_abundance_predator = log(abundance_predator);
}

parameters {
  real a_pop; // Population-level alpha
  real<lower = 0> sigma;
  
}

model {
vector[n] mu;

  // Priors:
  a_pop ~ normal(-4, 2);
  sigma ~ exponential(5);

  // Likelihood:
  // Computing target's mean
   mu = a_pop + log_biomass_prey + log_abundance_predator;

  // Computing target
  log_pred_flow ~ normal(mu, sigma);

}

generated quantities {
  vector[n] log_pred_flow_hat;
  vector[n] log_lik; //compute log-likelihood
  vector[n] y_rep; //replications from posterior predictive distribution
  real<lower = 0, upper = 1> Rsq_1;

  log_pred_flow_hat = a_pop + log_biomass_prey + log_abundance_predator;

    for(i in 1:n) {
  
      log_lik[i] = normal_lpdf(log_pred_flow[i] | log_pred_flow_hat[i], sigma );
  
      y_rep[i] = normal_rng(log_pred_flow_hat[i], sigma);
      
    }

  Rsq_1 = variance(log_pred_flow_hat) / (variance(log_pred_flow_hat) + square(sigma));

}
