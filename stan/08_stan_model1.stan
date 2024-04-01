data {
  // Size integer
  int<lower = 1> n;  // Sample size
  // Vector data
  vector[n] biomass_flow;  // Target data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator; // Predator abundances
}

transformed data {
   vector[n] log_biomass_flow = log(biomass_flow);
   vector[n] log_biomass_prey = log(biomass_prey);
   vector[n] log_abundance_predator = log(abundance_predator);
}

parameters {
  real alpha; // Population-level alpha
  real<lower = 0> sigma;
  
}

model {
vector[n] mu_flow;

  // Priors:
  alpha ~ normal(0, 1);
  sigma ~ exponential(3);

  // Likelihood:
  // Computing target's mean
   mu_flow = alpha + log_biomass_prey + log_abundance_predator;

  // Computing target
  log_biomass_flow ~ normal(mu_flow, sigma);

}

generated quantities {
  vector[n] log_biomass_flow_hat;
  vector[n] log_lik; //compute log-likelihood
  vector[n] y_rep; //replications from posterior predictive distribution
  real<lower = 0, upper = 1> Rsq_1;

  log_biomass_flow_hat = alpha + log_biomass_prey + log_abundance_predator;

    for(i in 1:n) {
  
      log_lik[i] = normal_lpdf(log_biomass_flow[i] | log_biomass_flow_hat[i], sigma);
  
      y_rep[i] = normal_rng(log_biomass_flow_hat[i], sigma);
      
    }

  Rsq_1 = variance(log_biomass_flow_hat) / (variance(log_biomass_flow_hat) + square(sigma));

}
