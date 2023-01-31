# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_modeltm <- readRDS("results/model_outputs/output_stan_modeltm.RDS")

#### Post-predictive checks ####

## Recover types
output_stan_modeltm <- tidybayes::recover_types(output_stan_modeltm)

## Extract loglikelihood and compute loo
log_lik_tm <- loo::extract_log_lik(output_stan_modeltm, merge_chains = FALSE)
r_eff_tm <- loo::relative_eff(exp(log_lik_tm))
loo_tm <- loo::loo(log_lik_tm, r_eff = r_eff_tm, save_psis = TRUE)

## Extract predictions and compute mean
yrep_tm <- rstan::extract(output_stan_modeltm, pars = "y_rep")[[1]]

dataset <- dataset |>
           dplyr::mutate(
            yrep_tm_mean = apply(yrep_tm, 2, mean)
           )

# Post-predictive plots
# Plots for model tm
bayesplot::ppc_loo_pit_overlay(
  y = log(dataset$biomass_flow),
  yrep = yrep_tm,
  lw = weights(loo_tm$psis_object)
) + ggplot2::labs(title = "Model tm")

bayesplot::ppc_loo_pit_qq(
  y = log(dataset$biomass_flow),
  yrep = yrep_tm,
  lw = weights(loo_tm$psis_object)
) + ggplot2::labs(title = "Model tm")

bayesplot::ppc_dens_overlay(log(dataset$biomass_flow), yrep_tm[1:500, ]) + ggplot2::labs(title = "Model tm")
