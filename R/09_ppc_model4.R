# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model4 <- readRDS("results/model_outputs/output_stan_model4.RDS")

#### Post-predictive checks ####

## Recover types
output_stan_model4 <- tidybayes::recover_types(output_stan_model4)

## Extract loglikelihood and compute loo
log_lik_4 <- loo::extract_log_lik(output_stan_model4, merge_chains = FALSE)
r_eff_4 <- loo::relative_eff(exp(log_lik_4))
loo_4 <- loo::loo(log_lik_4, r_eff = r_eff_4, save_psis = TRUE)

## Extract predictions and compute mean
yrep_4 <- rstan::extract(output_stan_model4, pars = "y_rep")[[1]]

dataset <- dataset |>
           dplyr::mutate(
            yrep_4_mean = apply(yrep_4, 2, mean)
           )

# Post-predictive plots
# Plots for model 4
bayesplot::ppc_loo_pit_overlay(
  y = log(dataset$biomass_flow),
  yrep = yrep_4,
  lw = weights(loo_4$psis_object)
) + ggplot2::labs(title = "Model 4")

bayesplot::ppc_loo_pit_qq(
  y = log(dataset$biomass_flow),
  yrep = yrep_4,
  lw = weights(loo_4$psis_object)
) + ggplot2::labs(title = "Model 4")

bayesplot::ppc_dens_overlay(log(dataset$biomass_flow), yrep_4[1:500, ]) + ggplot2::labs(title = "Model 4")


