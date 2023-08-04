# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
fit0 <- readRDS("results/model_outputs/stanfit_model0.RDS")
loo_0 <- readRDS("results/loo_outputs/loo_0.RDS")

#### Post-predictive checks ####

## Extract predictions and compute mean
yrep_0 <- rstan::extract(fit0, pars = "y_rep")[[1]]

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
