# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model0 <- readRDS("results/model_outputs/output_stan_model0.RDS")

#### Post-predictive checks ####

## Recover types
output_stan_model0 <- tidybayes::recover_types(output_stan_model0)

## Extract loglikelihood and compute loo
log_lik_0 <- loo::extract_log_lik(output_stan_model0, merge_chains = FALSE)
r_eff_0 <- loo::relative_eff(exp(log_lik_0))
loo_0 <- loo::loo(log_lik_0, r_eff = r_eff_0, save_psis = TRUE)

## Extract predictions and compute mean
yrep_0 <- rstan::extract(output_stan_model0, pars = "y_rep")[[1]]

dataset <- dataset |>
           dplyr::mutate(
            yrep_0_mean = apply(yrep_0, 2, mean)
           )

# Post-predictive plots
# Plots for model 0
bayesplot::ppc_loo_pit_overlay(
  y = log(dataset$biomass_flow),
  yrep = yrep_0,
  lw = weights(loo_0$psis_object)
) + ggplot2::labs(title = "Model 0")

bayesplot::ppc_loo_pit_qq(
  y = log(dataset$biomass_flow),
  yrep = yrep_0,
  lw = weights(loo_0$psis_object)
) + ggplot2::labs(title = "Model 0")

bayesplot::ppc_dens_overlay(log(dataset$biomass_flow), yrep_0[1:500, ]) + ggplot2::labs(title = "Model 0")
