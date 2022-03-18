library(rstan)
# The data

#Y <- rnorm(n = 100, mean = 3, sd = 10) dude
dataset <- readRDS("data/clean/dataset.RDS")
obs <- log(dataset$pred_flow)
abund_prey <- log(dataset$abund_prey)
mass_prey <- log(dataset$bodymass_prey)
abund_pred <- log(dataset$abund_pred)
pred_id <- as.numeric(dataset$pred_id)
npred <- length(unique(dataset$predator))

# Store the info in a list
lst_score_data0 <- list(y = obs, N = length(obs))
lst_score_data1 <- list(y = obs, N = length(obs), abund_prey = abund_prey, abund_pred = abund_pred, mass_prey = mass_prey)
lst_score_data2 <- list(y = obs, N = length(obs), npred = npred, abund_prey = abund_prey, abund_pred = abund_pred, mass_prey = mass_prey, pred_id = pred_id)

# Fit the models
fit_score0 <- stan(
  file = "R/11_bayesian_model0.stan",
  iter = 2000,
  chains = 4,
  cores = 3,
  data = lst_score_data0
)
traceplot(fit_score0, pars = c("mu", "sigma"))

fit_score1 <- stan(
  file = "R/11_bayesian_model1.stan",
  iter = 2000,
  chains = 4,
  cores = 3,
  data = lst_score_data1
)
traceplot(fit_score1, pars = c("alpha", "sigma"))

fit_score2 <- stan(
  file = "R/11_bayesian_model2.stan",
  iter = 2000,
  chains = 4,
  cores = 3,
  data = lst_score_data2
)
bayesplot::mcmc_areas(as.matrix(fit_score), pars = paste0("alpha[",1:107,"]"))

post_sd <- c(summary(fit_score)$summary[,"sd"])
post_sd <- post_sd[1:107]
df_pred <- dataset[,c("prey","predator","pred_id")]
df_pred$pred_id <- as.numeric(df_pred$pred_id)
df_pred <- df_pred[order(df_pred$pred_id), ]
df_pred <- split(df_pred, f = df_pred$pred_id)
df_row <- lapply(df_pred, function(x) nrow(x))
pred_deg <- unlist(df_row)
plot(pred_deg, post_sd)

# Plot the chains
traceplot(fit_score0, pars = c("mu", "sigma"))
