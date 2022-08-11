data {
  // Size integer
  int<lower = 1> N;  // Sample size
  // Vector data
  vector[N] y;  // Target data
  vector[N] biomass_prey; // Prey biomasses
  vector[N] abundance_pred; // Predator abundances
}

transformed data {
   vector[N] log_y = log(y);
   vector[N] log_biomass_prey = log(biomass_prey);
   vector[N] log_abundance_pred = log(abundance_pred);
}

parameters {
  real a_pop; // Population-level alpha
  real<lower = 0> sigma;
  
}

model {
  vector[N] mu;

  // Priors:
  a_pop ~ normal(1, 10);
  sigma ~ exponential(0.2);

  // Likelihood:
  // Computing target's mean
   mu = a_pop + log_biomass_prey + log_abundance_pred;

  // Computing target
   log_y ~ normal(mu, sigma);

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
