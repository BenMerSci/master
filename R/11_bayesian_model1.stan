data {
  int<lower = 1> N;  // Total number of trials
  vector[N] y;  // Score in each trial
  vector[N] abund_prey; // prey abundances
  vector[N] abund_pred; // predator abundances
  vector[N] mass_prey; //  prey bodymasses
}

parameters {
  real alpha;
  real<lower = 0> sigma;
  
}

model {
  vector[N] mu;

  // Priors:
  alpha ~ normal(1, 1);
  sigma ~ lognormal(3, 1);

  // Likelihood:
  for(i in 1:N)
  mu[i] = alpha * abund_prey[i] * mass_prey[i] * abund_pred[i];
  y ~ normal(mu, sigma);

}
