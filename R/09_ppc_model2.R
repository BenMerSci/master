# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
fit2 <- readRDS("results/model_outputs/stanfit_model2.RDS")
loo_2 <- readRDS("results/loo_outputs/loo_2.RDS")

#### Post-predictive checks ####

## Extract predictions and compute mean
yrep_2 <- rstan::extract(fit2, pars = "y_rep")[[1]]

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
