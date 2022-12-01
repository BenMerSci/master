data {
  int<lower=1> n;  // total number of observations
  vector[n] pred_flow;  // response variable
}

transformed data {
  vector[n] log_pred_flow = log(pred_flow);
}

parameters {
  real Intercept;  // temporary intercept for centered predictors
  //real<lower=0> sigma;  // dispersion parameter
}

model {

  // Prios:
  Intercept ~ normal(0,1.5);
  //sigma ~ exponential(5);


  log_pred_flow ~ normal(Intercept, 1);
  
}

generated quantities {
  vector[n] log_pred_flow_hat;
  vector[n] log_lik; //compute log-likelihood
  vector[n] y_rep; //replications from posterior predictive distribution
  real<lower = 0, upper = 1> Rsq_0;

  log_pred_flow_hat = Intercept + rep_vector(0.0, n);
  
  for (i in 1:n) {

    log_lik[i] = normal_lpdf(log_pred_flow[i] | log_pred_flow_hat[i], 1);

    y_rep[i] = normal_rng(log_pred_flow_hat[i], 1);

  }

  Rsq_0 = variance(log_pred_flow_hat) / (variance(log_pred_flow_hat) + square(1));

}
