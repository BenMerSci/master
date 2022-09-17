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
  a_pop ~ normal(1, 10);
  sigma ~ exponential(0.2);

  // Likelihood:
  // Computing target's mean
   mu = a_pop + log_biomass_prey + log_abundance_predator;

  // Computing target
   log_pred_flow ~ normal(mu, sigma);

}

//generated quantities {
//  vector[N] yhat;                // predictor
//  real<lower=0> rss;             // residual sum of squares
//  real<lower=0> totalss;         // total SS              
//  real Rsq;                      // Rsq
//  
//  yhat = a_pop + biomass_prey + abundance_pred;
//  rss = dot_self(y-yhat);
//  totalss = dot_self(y-mean(y));
//  Rsq = 1 - rss/totalss;
//}
