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

generated quantities {
  // Generating log-lik for loo
  vector[n] log_lik;
  vector[n] mu;

  mu = Intercept + rep_vector(0.0, n);
  
  for (i in 1:n) {
    log_lik[i] = normal_lpdf(log_pred_flow[i] | mu[i], sigma);
  }

  // Values to predict
  // vector[n] log_pred_flow_sim;
  // To check the fit
  //real<lower=0> rss; // residual sum of squares
  //real<lower=0> totalss; // total SS  
  //real Rsq; // Rsq
  //
  //log_pred_flow_sim = Intercept + rep_vector(0.0, n);
  
  //rss = dot_self(log_pred_flow-log_pred_flow_sim);
  //totalss = dot_self(log_pred_flow-mean(log_pred_flow));
  //Rsq = 1 - rss/totalss;

}
