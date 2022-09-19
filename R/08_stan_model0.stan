data {
  int<lower=1> n;  // total number of observations
  vector[n] pred_flow;  // response variable
}

transformed data {
  vector[n] log_pred_flow = log(pred_flow);
}

parameters {
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
}

model {
  vector[n] mu;

  // Prios:
  Intercept ~ normal(1,10);
  sigma ~ exponential(0.2);

  mu = Intercept + rep_vector(0.0, n);

  log_pred_flow ~ normal(mu, sigma);
}
