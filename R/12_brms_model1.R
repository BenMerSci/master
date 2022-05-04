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
dataset <- readRDS("data/clean/dataset.RDS")
dataset_sub <- subset(dataset, select = c("pred_flow",
               "abund_prey", "abund_pred", "bodymass_prey"))

###################
# Lognormal model #
###################
# Set the equation with associated distribution
curve_bf_lgnorm <- bf(
                   log(pred_flow) ~ loga +
                    log(abund_prey) + log(bodymass_prey) + log(abund_pred),
                   family = gaussian(link = "identity"),
                   loga ~ 1,
                   nl = TRUE)

# Get the default priors
get_prior(curve_bf_lgnorm, data = dataset_sub)
# Change the priors
curve_lgnorm_prior <- c(
    prior(exponential(4), class = "sigma"),
    prior(normal(1, 1), class = "b", nlpar = "loga")
)
# Fit the model
curve_lgnorm_post <- brm(curve_bf_lgnorm,
                               prior = curve_lgnorm_prior,
                               data = dataset_sub,
                               file = "data/intermediate/brms_model1",
                               file_refit = "on_change")
# Plot the posterior distr. of parameters
plot(curve_lgnorm_post)
# Draw predictions from the model
lgnorm_post <- dataset_sub |>
  add_predicted_draws(curve_lgnorm_post)
# Plot the predictions in function of data
lgnorm_post |>
  ggplot(aes(x = log(abund_prey) + log(abund_pred) +
    log(bodymass_prey), y = .prediction)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Oranges") +
  geom_point(aes(x = log(abund_prey) + log(abund_pred) +
    log(bodymass_prey), y = log(pred_flow)),
    size = 3, pch = 21, fill = "lightblue", data = dataset_sub)

# Plot without logging data, logging axis
lgnorm_post |>
  ggplot(aes(x = abund_prey * abund_pred *
    bodymass_prey, y = exp(.prediction))) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Oranges") +
  geom_point(aes(x = abund_prey * abund_pred * bodymass_prey, y = pred_flow),
    size = 3, pch = 21, fill = "lightblue", data = dataset_sub) +
  coord_trans(x = "log", y = "log")
