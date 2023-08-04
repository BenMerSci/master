# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
fit1 <- readRDS("results/model_outputs/stanfit_model1.RDS")
loo_1 <- readRDS("results/loo_outputs/loo_1.RDS")

#### Post-predictive checks ####

## Extract predictions and compute mean
yrep_1 <- rstan::extract(fit1, pars = "y_rep")[[1]]

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
