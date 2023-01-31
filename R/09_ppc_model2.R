# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model2 <- readRDS("results/model_outputs/output_stan_model2.RDS")

#### Post-predictive checks ####

## Recover types
output_stan_model2 <- tidybayes::recover_types(output_stan_model2)

## Extract loglikelihood and compute loo
log_lik_2 <- loo::extract_log_lik(output_stan_model2, merge_chains = FALSE)
r_eff_2 <- loo::relative_eff(exp(log_lik_2))
loo_2 <- loo::loo(log_lik_2, r_eff = r_eff_2, save_psis = TRUE)

## Extract predictions and compute mean
yrep_2 <- rstan::extract(output_stan_model2, pars = "y_rep")[[1]]

dataset <- dataset |>
           dplyr::mutate(
            yrep_2_mean = apply(yrep_2, 2, mean)
           )

# Post-predictive plots
# Plots for model 2
bayesplot::ppc_loo_pit_overlay(
  y = log(dataset$biomass_flow),
  yrep = yrep_2,
  lw = weights(loo_2$psis_object)
) + ggplot2::labs(title = "Model 2")

bayesplot::ppc_loo_pit_qq(
  y = log(dataset$biomass_flow),
  yrep = yrep_2,
  lw = weights(loo_2$psis_object)
) + ggplot2::labs(title = "Model 2")

bayesplot::ppc_dens_overlay(log(dataset$biomass_flow), yrep_2[1:500, ]) + ggplot2::labs(title = "Model 2")
