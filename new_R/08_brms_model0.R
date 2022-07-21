# Needed libraries
library(tidyverse)
library(tidybayes)
library(brms)
library(bayesplot)
library(rstan)

# Options to run the chains faster
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load the dataset
dataset <- readRDS("data/clean/new_dataset.RDS")

###############
# BRMS method #
###############
dataset_sub <- subset(dataset, select = c("pred_flow", "biomass_prey",
               "biomass_predator", "bodymass_mean_predator",
               "predator"))

curve_bf_const0 <- bf(log(pred_flow) ~ 1,
                     family = gaussian(link = "identity")
                     )

# Get the default priors
get_prior(curve_bf_const0, data = dataset_sub)

# Fit the model
curve_const_post0 <- brm(curve_bf_const0,
                    data = dataset_sub,
                    file = "data/intermediate/new/brms_model0",
                    file_refit = "on_change"
                    )

# Plot the posterior distr. of parameters
plot(curve_const_post0)
# Draw predictions from the model
const_post <- dataset_sub |>
  add_predicted_draws(curve_const_post0)
# Plot the predictions in function of data
const_post |>
  ggplot(aes(x = log(biomass_prey) + (log(biomass_predator) - log(bodymass_max_predator)),
    y = .prediction)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Oranges") +
  geom_point(aes(x = log(biomass_prey) + (log(biomass_predator) - log(bodymass_max_predator)),
    y = log(pred_flow)),
    size = 3, pch = 21, fill = "lightblue", data = dataset_sub)



















############
# raw Stan #
############
# The data
obs <- log(dataset$pred_flow)
#biomass_prey <- log(dataset$biomass_prey)
#bodymass_min_predator <- log(dataset$bodymass_min_predator)
#bodymass_max_predator <- log(dataset$bodymass_max_predator)

# Store the info in a list for each model
lst_score_data0 <- list(Y = obs, N = length(obs))


# Fit the models
# Model0
fit_score0 <- stan(
  file = "new_R/bayesian_model0.stan",
  iter = 2000,
  chains = 4,
  cores = 3,
  data = lst_score_data0
)

traceplot(fit_score0, pars = c("mu", "sigma"))

# Alex Hayes method
m1_path <- "new_R/bayesian_model0.stan"
m1 <- stan_model(m1_path)
fit_test <- sampling(m1, data = lst_score_data0, chains = 4, iter = 2000, refresh = 0)
print(fit_test, pars = "Intercept", probs = c(0.025, 0.5, 0.975))
print(fit_test, pars = "sigma", probs = c(0.025, 0.5, 0.975))


# error in variable models
# can weight by the invere of the variance but no cause nothing linear

# -> bodysize is normally distributed, and points in the range its 95%
# extreme-value dist. -> no
