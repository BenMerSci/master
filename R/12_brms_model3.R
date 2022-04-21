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
dataset_sub <- subset(dataset, select = c("pred_flow","abund_prey","abund_pred","bodymass_prey","degrees","predator"))
#dataset_sub$pred_model <- paste0(dataset_sub$predator,"_",dataset_sub$model_name)

###################
# Lognormal model #
###################
# Set the equation with associated distributionf
curve_bf_lgnorm_rand <- bf(log(pred_flow) ~ loga-log(degrees) + log(abund_prey) + log(bodymass_prey) + log(abund_pred),
                     family = gaussian(link = "identity"),
                     loga ~ 1 + (1 | predator),
                     nl = TRUE)
# Get the default priors
get_prior(curve_bf_lgnorm_rand, data = dataset_sub)
# Change the priors
curve_lgnorm_prior_rand <- c(
    prior(exponential(4), class = "sigma"),
    prior(normal(0,3), class = "b", nlpar = "loga"),
    prior(exponential(2), class = "sd", nlpar = "loga")
)
# Fit the model
curve_lgnorm_post_rand <- brm(curve_bf_lgnorm_rand,
                               prior = curve_lgnorm_prior_rand,
                               data = dataset_sub,
                               file = "data/intermediate/brms_model3",
                               file_refit = "on_change")

# Plot the posterior distr. of parameters
plot(curve_lgnorm_post_rand)
# Draw predictions from the model
lgnorm_post_rand <- dataset_sub |> 
  add_predicted_draws(curve_lgnorm_post_rand, re_formula = NA)
# Plot the predictions in function of data
lgnorm_post_rand |> 
  ggplot(aes(x = log(abund_prey)-log(degrees)+log(abund_pred)+log(bodymass_prey), y = .prediction)) + 
  stat_lineribbon() + 
  scale_fill_brewer(palette = "Oranges") + 
  geom_point(aes(x = log(abund_prey)-log(degrees)+log(abund_pred)+log(bodymass_prey), y = log(pred_flow)),
             size = 3, pch = 21, fill = "lightblue", data = dataset_sub)
