# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model0 <- readRDS("results/model_outputs/output_stan_model0.RDS") |>
                        tidybayes::recover_types()
output_stan_model1 <- readRDS("results/model_outputs/output_stan_model1.RDS") |>
                        tidybayes::recover_types()
output_stan_model2 <- readRDS("results/model_outputs/output_stan_model2.RDS") |>
                        tidybayes::recover_types()
output_stan_model3 <- readRDS("results/model_outputs/output_stan_model3.RDS") |>
                        tidybayes::recover_types()
output_stan_model4 <- readRDS("results/model_outputs/output_stan_model4.RDS") |>
                        tidybayes::recover_types()
#output_stan_modeltm <- readRDS("results/model_outputs/output_stan_modeltm.RDS") |>
#                         tidybayes::recover_types()
#output_stan_model_allometric <- readRDS("results/model_outputs/output_stan_model_allometric.RDS") |>
#                         tidybayes::recover_types()

# Source useful functions
source("lib/rsq.R")

# Compute the R²
list_model <- list(output_stan_model0, output_stan_model1,
                   output_stan_model2, output_stan_model3,
                   output_stan_model4)

table_rsq(list_model)

# Loo compare earch model
log_lik_0 <- loo::extract_log_lik(output_stan_model0, merge_chains = FALSE)
r_eff_0 <- loo::relative_eff(exp(log_lik_0))
loo_0 <- loo::loo(log_lik_0, r_eff = r_eff_0, save_psis = TRUE)

log_lik_1 <- loo::extract_log_lik(output_stan_model1, merge_chains = FALSE)
r_eff_1 <- loo::relative_eff(exp(log_lik_1))
loo_1 <- loo::loo(log_lik_1, r_eff = r_eff_1, save_psis = TRUE)

log_lik_2 <- loo::extract_log_lik(output_stan_model2, merge_chains = FALSE)
r_eff_2 <- loo::relative_eff(exp(log_lik_2))
loo_2 <- loo::loo(log_lik_2, r_eff = r_eff_2, save_psis = TRUE)

log_lik_3 <- loo::extract_log_lik(output_stan_model3, merge_chains = FALSE)
r_eff_3 <- loo::relative_eff(exp(log_lik_3))
loo_3 <- loo::loo(log_lik_3, r_eff = r_eff_3, save_psis = TRUE)

log_lik_4 <- loo::extract_log_lik(output_stan_model4, merge_chains = FALSE)
r_eff_4 <- loo::relative_eff(exp(log_lik_4))
loo_4 <- loo::loo(log_lik_4, r_eff = r_eff_4, save_psis = TRUE)

#log_lik_tm <- loo::extract_log_lik(output_stan_modeltm, merge_chains = FALSE)
#r_eff_tm <- loo::relative_eff(exp(log_lik_tm))
#loo_tm <- loo::loo(log_lik_tm, r_eff = r_eff_tm, save_psis = TRUE)

#log_lik_allometric <- loo::extract_log_lik(output_stan_model_allometric, merge_chains = FALSE)
#r_eff_allometric <- loo::relative_eff(exp(log_lik_allometric))
#loo_allometric <- loo::loo(log_lik_allometric, r_eff = r_eff_allometric, save_psis = TRUE)

# Rank the models with loo compare
loo::loo_compare(list(model_0 = loo_0, model_1 = loo_1, model_2 = loo_2,
            model_3 = loo_3, model_4 = loo_4))

# Compare model 2 and model 3 alphas
library(ggplot2)
alphas_2 <- tidybayes::gather_draws(output_stan_model2, alpha[pred_id]) |>
              dplyr::rename(alpha2 = .value)
alphas_3 <- tidybayes::gather_draws(output_stan_model3, alpha[pred_id]) |>
              dplyr::rename(alpha3 = .value)
df_alpha <- cbind(alphas_2, alphas_3)

alpha2 <- rstan::summary(output_stan_model2, pars = "alpha")$summary[,"mean"]
alpha3 <- rstan::summary(output_stan_model3, pars = "alpha")$summary[,"mean"]
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
