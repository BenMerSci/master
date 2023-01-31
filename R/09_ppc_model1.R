# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model1 <- readRDS("results/model_outputs/output_stan_model1.RDS")

#### Post-predictive checks ####

## Recover types
output_stan_model1 <- tidybayes::recover_types(output_stan_model1)

## Extract loglikelihood and compute loo
log_lik_1 <- loo::extract_log_lik(output_stan_model1, merge_chains = FALSE)
r_eff_1 <- loo::relative_eff(exp(log_lik_1))
loo_1 <- loo::loo(log_lik_1, r_eff = r_eff_1, save_psis = TRUE)

## Extract predictions and compute mean
yrep_1 <- rstan::extract(output_stan_model1, pars = "y_rep")[[1]]

dataset <- dataset |>
           dplyr::mutate(
            yrep_1_mean = apply(yrep_1, 2, mean)
           )

# Post-predictive plots
# Plots for model 1
bayesplot::ppc_loo_pit_overlay(
  y = log(dataset$biomass_flow),
  yrep = yrep_1,
  lw = weights(loo_1$psis_object)
) + ggplot2::labs(title = "Model 1")

bayesplot::ppc_loo_pit_qq(
  y = log(dataset$biomass_flow),
  yrep = yrep_1,
  lw = weights(loo_1$psis_object)
) + ggplot2::labs(title = "Model 1")

bayesplot::ppc_dens_overlay(log(dataset$biomass_flow), yrep_1[1:500, ]) + ggplot2::labs(title = "Model 1")
