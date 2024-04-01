data {
  int<lower=1> n;  // total number of observations
  vector[n] biomass_flow;  // response variable
}

transformed data {
  vector[n] log_biomass_flow = log(biomass_flow);
}

parameters {
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
}

model {

  // Prios:
  Intercept ~ normal(0, 1);
  sigma ~ exponential(3);


  log_biomass_flow ~ normal(Intercept, sigma);
  
}

generated quantities {
  vector[n] log_biomass_flow_hat;
  vector[n] log_lik; //compute log-likelihood
  vector[n] y_rep; //replications from posterior predictive distribution
  real<lower = 0, upper = 1> Rsq_0;

  log_biomass_flow_hat = Intercept + rep_vector(0.0, n);
  
  for (i in 1:n) {

    log_lik[i] = normal_lpdf(log_biomass_flow[i] | log_biomass_flow_hat[i], sigma);

    y_rep[i] = normal_rng(log_biomass_flow_hat[i], sigma);

  }

  Rsq_0 = variance(log_biomass_flow_hat) / (variance(log_biomass_flow_hat) + square(sigma));

}
