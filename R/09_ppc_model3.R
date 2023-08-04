# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
fit3 <- readRDS("results/model_outputs/stanfit_model3.RDS")
loo_3 <- readRDS("results/loo_outputs/loo_3.RDS")

#### Post-predictive checks ####

## Extract predictions and compute mean
yrep_3 <- rstan::extract(fit3, pars = "y_rep")[[1]]

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
