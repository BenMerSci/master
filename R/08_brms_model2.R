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
               "biomass_predator", "bodymass_mean_predator", "predator"))

# Define the model
curve_bf_const2 <- bf(log(pred_flow) ~ loga +
                      log(biomass_prey) +
                      (log(biomass_predator) - log(bodymass_mean_predator)),
                      family = gaussian(link = "identity"),
                      loga ~ 1 + (1 | predator),
                      nl = TRUE)


# Get the default priors
get_prior(curve_bf_const2, data = dataset_sub)

# Set the priors
curve_lgnorm_prior <- c(
    prior(exponential(4), class = "sigma"),
    prior(normal(1, 1), class = "b", nlpar = "loga")
)

# Fit the model
output_brms_model2 <- brm(curve_bf_const2,
                    prior = curve_lgnorm_prior,
                    data = dataset_sub,
                    file = "results/model_outputs/output_brms_model2.RDS",
                    file_refit = "on_change")

# Draw predictions from the model
#const_post <- dataset_sub |>
#  add_predicted_draws(curve_const_post2)
## Plot the predictions in function of data
#const_post |>
#  ggplot(aes(x = log(biomass_prey) + (log(biomass_predator) - log(bodymass_mean_predator)),
#    y = .prediction)) +
#  stat_lineribbon() +
#  scale_fill_brewer(palette = "Oranges") +
#  geom_point(aes(x = log(biomass_prey) + (log(biomass_predator) - log(bodymass_mean_predator)),
#    y = log(pred_flow)),
#    size = 3, pch = 21, fill = "lightblue", data = dataset_sub)
