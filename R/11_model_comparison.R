# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
fit0 <- readRDS("results/model_outputs/stanfit_model0.RDS") |>
                        tidybayes::recover_types()
loo_0 <- readRDS("results/loo_outputs/loo_0.RDS")
fit1 <- readRDS("results/model_outputs/stanfit_model1.RDS") |>
                        tidybayes::recover_types()
loo_1 <- readRDS("results/loo_outputs/loo_1.RDS")
fit2 <- readRDS("results/model_outputs/stanfit_model2.RDS") |>
                        tidybayes::recover_types()
loo_2 <- readRDS("results/loo_outputs/loo_2.RDS")
fit3 <- readRDS("results/model_outputs/stanfit_model3.RDS") |>
                        tidybayes::recover_types()
loo_3 <- readRDS("results/loo_outputs/loo_3.RDS")
fit4 <- readRDS("results/model_outputs/stanfit_model4.RDS") |>
                        tidybayes::recover_types()
loo_4 <- readRDS("results/loo_outputs/loo_4.RDS")
#fittm <- readRDS("results/model_outputs/stanfit_modeltm.RDS") |>
#                         tidybayes::recover_types()
#fitallometric <- readRDS("results/model_outputs/stanfit_model_allometric.RDS") |>
#                         tidybayes::recover_types()

# Source useful functions
source("lib/rsq.R")

# Compute the R²
list_model <- list(fit0, fit1, fit2, fit3, fit4)

table_rsq(list_model)

# Rank the models with loo compare
loo::loo_compare(list(model_0 = loo_0, model_1 = loo_1, model_2 = loo_2,
            model_3 = loo_3, model_4 = loo_4))

# Compare model 2 and model 3 alphas
library(ggplot2)
alphas_2 <- tidybayes::gather_draws(fit2, alpha[pred_id]) |>
              dplyr::rename(alpha2 = .value)
alphas_3 <- tidybayes::gather_draws(fit3, alpha[pred_id]) |>
              dplyr::rename(alpha3 = .value)
df_alpha <- cbind(alphas_2, alphas_3)

alpha2 <- rstan::summary(fit2, pars = "alpha")$summary[,"mean"]
alpha3 <- rstan::summary(fit3, pars = "alpha")$summary[,"mean"]
df_alpha <- cbind(alpha2,alpha3) |> as.data.frame()
df_alpha |>
        ggplot(aes(x = alpha2, y = alpha3)) +
        geom_point() +
        theme_minimal() +
        theme(
         legend.title = element_blank(),
         plot.title = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         axis.title.x = element_text(size = 15),
         axis.text.x = element_text(size = 13),
         axis.text.y = element_text(size = 13)
        ) +
        xlim(c(-19,1)) +
        ylim(c(-19,1)) +
        labs(title = "alpha comparisons") +
        xlab("Model 2 alphas") +
        ylab("Model 3 alphas") +
        geom_abline(intercept = 0, slope = 1)
