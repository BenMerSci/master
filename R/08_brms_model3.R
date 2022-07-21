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
# BRMS model #
###############
dataset_sub <- subset(dataset, select = c("pred_flow", "biomass_prey",
               "biomass_predator", "bodymass_mean_predator",
               "degree_predator", "predator"))

# Define the model
curve_bf_const3 <- bf(log(pred_flow) ~
                          (loga - degree_predator) + log(biomass_prey) +
                          (log(biomass_predator) - log(bodymass_mean_predator)),
                        family = gaussian(link = "identity"),
                        loga ~ 1 + (1 | predator),
                        nl = TRUE)

# Get the default priors
get_prior(curve_bf_lgnorm_rand, data = dataset_sub)

# Change priors
curve_lgnorm_prior <- c(
    prior(exponential(4), class = "sigma"),
    prior(normal(0, 3), class = "b", nlpar = "loga"),
    prior(exponential(2), class = "sd", nlpar = "loga")
)

# Fit the model
curve_const_post2 <- brm(curve_bf_const3,
                    prior = curve_lgnorm_prior,
                    data = dataset_sub,
                    file = "results/model_outputs/output_brms_model3.RDS",
                    file_refit = "on_change")

# Plot the posterior distr. of parameters
#plot(curve_const_post3)
## Draw predictions from the model
#const_post <- dataset_sub |>
#  add_predicted_draws(curve_const_post3)
## Plot the predictions in function of data
#const_post |>
#  ggplot(aes(x = log(biomass_prey) + (log(biomass_predator) - log(bodymass_mean_predator)),
#    y = .prediction)) +
#  stat_lineribbon() +
#  scale_fill_brewer(palette = "Oranges") +
#  geom_point(aes(x = log(biomass_prey) + (log(biomass_predator) - log(bodymass_mean_predator)),
#    y = log(pred_flow)),
#    size = 3, pch = 21, fill = "lightblue", data = dataset_sub)
