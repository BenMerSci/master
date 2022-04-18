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
dataset_sub <- subset(dataset, select = c("pred_flow","abund_prey","abund_pred","bodymass_prey","predator"))

curve_bf_const <- bf(log(pred_flow) ~ 1,
                     family = gaussian(link = "identity")
                     )

# Get the default priors
get_prior(curve_bf_const, data = dataset_sub)

# Fit the model
curve_const_post <- brm(curve_bf_const,
                               data = dataset_sub, 
                               file = "data/intermediate/brms_model0",
                               file_refit = "on_change")

# Plot the posterior distr. of parameters
plot(curve_const_post)
# Draw predictions from the model
const_post <- dataset_sub |> 
  add_predicted_draws(curve_const_post)
# Plot the predictions in function of data
const_post |> 
  ggplot(aes(x = log(abund_prey)+log(abund_pred)+log(bodymass_prey), y = .prediction)) + 
  stat_lineribbon() + 
  scale_fill_brewer(palette = "Oranges") + 
  geom_point(aes(x = log(abund_prey)+log(abund_pred)+log(bodymass_prey), y = log(pred_flow)),
             size = 3, pch = 21, fill = "lightblue", data = dataset_sub)


# Same model but with a random effect on predator
curve_bf_const_rand <- bf(log(pred_flow) ~ 1 + (1 | predator),
                         family = gaussian(link = "identity")
                         )

get_prior(curve_bf_const_rand, data = dataset_sub)

curve_const_rand_post <- brm(curve_bf_const_rand,
                             data = dataset_sub,
                             file = "data/intermediate/brms_model0_rand",
                             file_refit = "on_change")

# Plot the posterior distr. of parameters
plot(curve_const_rand_post)
# Draw predictions from the model
const_rand_post <- dataset_sub |> 
  add_predicted_draws(curve_const_rand_post)
# Plot the predictions in function of data
const_rand_post |> 
  ggplot(aes(x = log(abund_prey)+log(abund_pred)+log(bodymass_prey), y = .prediction)) + 
  stat_lineribbon() + 
  scale_fill_brewer(palette = "Oranges") + 
  geom_point(aes(x = log(abund_prey)+log(abund_pred)+log(bodymass_prey), y = log(pred_flow)),
             size = 3, pch = 21, fill = "lightblue", data = dataset_sub)
