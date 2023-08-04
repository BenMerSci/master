# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
fit4 <- readRDS("results/model_outputs/stanfit_model4.RDS")
loo_4 <- readRDS("results/loo_outputs/loo_4.RDS")

#### Post-predictive checks ####

## Extract predictions and compute mean
yrep_4 <- rstan::extract(fit4, pars = "y_rep")[[1]]

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


