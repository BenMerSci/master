# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model3 <- readRDS("results/model_outputs/output_stan_model3.RDS")

#### Post-predictive checks ####

## Recover types
output_stan_model3 <- tidybayes::recover_types(output_stan_model3)

## Extract loglikelihood and compute loo
log_lik_3 <- loo::extract_log_lik(output_stan_model3, merge_chains = FALSE)
r_eff_3 <- loo::relative_eff(exp(log_lik_3))
loo_3 <- loo::loo(log_lik_3, r_eff = r_eff_3, save_psis = TRUE)

## Extract predictions and compute mean
yrep_3 <- rstan::extract(output_stan_model3, pars = "y_rep")[[1]]

dataset <- dataset |>
           dplyr::mutate(
            yrep_3_mean = apply(yrep_3, 2, mean)
           )

# Post-predictive plots
# Plots for model 3
bayesplot::ppc_loo_pit_overlay(
  y = log(dataset$biomass_flow),
  yrep = yrep_3,
  lw = weights(loo_3$psis_object)
) + ggplot2::labs(title = "Model 3")

bayesplot::ppc_loo_pit_qq(
  y = log(dataset$biomass_flow),
  yrep = yrep_3,
  lw = weights(loo_3$psis_object)
) + ggplot2::labs(title = "Model 3")

bayesplot::ppc_dens_overlay(log(dataset$biomass_flow), yrep_3[1:500, ]) + ggplot2::labs(title = "Model 3")
