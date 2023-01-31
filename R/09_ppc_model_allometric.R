# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model_allometric <- readRDS("results/model_outputs/output_stan_model_allometric.RDS")

#### Post-predictive checks ####

## Recover types
output_stan_model_allometric <- tidybayes::recover_types(output_stan_model_allometric)

## Extract loglikelihood and compute loo
log_lik_allometric <- loo::extract_log_lik(output_stan_model_allometric, merge_chains = FALSE)
r_eff_allometric <- loo::relative_eff(exp(log_lik_allometric))
loo_allometric <- loo::loo(log_lik_allometric, r_eff = r_eff_allometric, save_psis = TRUE)

## Extract predictions and compute mean
yrep_allometric <- rstan::extract(output_stan_model_allometric, pars = "y_rep")[[1]]

dataset <- dataset |>
           dplyr::mutate(
            yrep_allometric_mean = apply(yrep_allometric, 2, mean)
           )

# Post-predictive plots
# Plots for model allometric
bayesplot::ppc_loo_pit_overlay(
  y = log(dataset$biomass_flow),
  yrep = yrep_allometric,
  lw = weights(loo_allometric$psis_object)
) + ggplot2::labs(title = "Model allometric")

bayesplot::ppc_loo_pit_qq(
  y = log(dataset$biomass_flow),
  yrep = yrep_allometric,
  lw = weights(loo_allometric$psis_object)
) + ggplot2::labs(title = "Model allometric")

bayesplot::ppc_dens_overlay(log(dataset$biomass_flow), yrep_allometric[1:500, ]) + ggplot2::labs(title = "Model allometric")
