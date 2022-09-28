data {
  // Size integer
  int<lower = 1> n;  // Sample size
  int<lower = 1> n_predator; // Total number of unique predators globally
  // Vector data
  vector[n] pred_flow;  // Target data
  vector[n] biomass_prey; // Prey biomasses
  vector[n] abundance_predator; // Predator abundances
  int pred_id[n]; // Predator ids to assess each alpha
}

transformed data {
   vector[n] log_pred_flow = log(pred_flow);
   vector[n] log_biomass_prey = log(biomass_prey);
   vector[n] log_abundance_predator = log(abundance_predator);
}

parameters {
  real a_pop; // Population-level alpha
  vector[n_predator] a_grp; // Group-level effect for each predator to be added to the Population-level alpha
  real<lower = 0> a_sd;
  real<lower = 0> sigma; //
}

model {
  vector[n] mu;
  vector[n] alpha_spec;

  // Priors:
  a_pop ~ normal(1, 10);
  a_grp ~ normal(1, a_sd);
  a_sd ~ exponential(0.2);
  sigma ~ exponential(0.2);

  // Likelihood:
  // Computing each alpha by predator
   alpha_spec = a_pop + a_grp[pred_id];
  
  // Computing target's mean
   mu = alpha_spec + log_biomass_prey + log_abundance_predator;

  // Computing target
   log_pred_flow ~ normal(mu, sigma);

}

generated quantities {
    vector[n] mu;
    vector[n] alpha_spec;
    vector[n] log_lik;

  alpha_spec = a_pop + a_grp[pred_id];
  mu = alpha_spec + log_biomass_prey + log_abundance_predator;
  
  for (i in 1:n) {
    log_lik[i] = normal_lpdf(log_pred_flow[i] | mu[i], sigma);
  }

  // Values to predict
  //vector[n] log_pred_flow_sim;
  //vector[n_predator] alpha_spec;
  //// To check the fit
  //real<lower=0> rss; // residual sum of squares
  //real<lower=0> totalss; // total SS  
  //real Rsq; // Rsq

  //alpha_spec = a_pop + a_grp;
  //
  //// Use current estimate of alphas to generate new sample
  //log_pred_flow_sim = alpha_spec[pred_id] + log_biomass_prey + log_abundance_predator;
  //
  //rss = dot_self(log_pred_flow-log_pred_flow_sim);
  //totalss = dot_self(log_pred_flow-mean(log_pred_flow));
  //Rsq = 1 - rss/totalss;
}
