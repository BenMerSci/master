# Load needed libraries
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(ggdist)

# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model1 <- readRDS("results/model_outputs/output_stan_model1.RDS")
source("lib/plot_functions.R")

# Recover types
output_stan_model1 <- recover_types(output_stan_model1)
# Draw the posterior for all parameters
general_params <- output_stan_model1 |>
                   tidybayes::gather_draws(alpha, sigma)

# Plot posterior dist. of parameters
ggplot(general_params, aes(x = `.value`, y = .variable)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
    labs(title = "") +
     theme_ipsum() +
      theme(
       legend.title = element_text(size = 15),
       legend.text = element_text(size = 13),
       axis.title.y = element_blank(),
       axis.title.x = element_text(size = 18),
       plot.title = element_text(size = 30),
       axis.text.x = element_text(size = 20),
       axis.text.y = element_text(size = 20)
      ) +
       xlab("Parameters log-valued")

# Predictions vs observed data
plot_sim_noerror(output_stan_model1)
plot_sim_error(output_stan_model1)
