# Load needed libraries
library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(ggdist)

# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
output_stan_model4 <- readRDS("results/model_outputs/output_stan_model4.RDS")
source("lib/plot_functions.R")

# Recover types
output_stan_model4 <- tidybayes::recover_types(output_stan_model4)
# Draw the posterior for all parameters
general_params <- output_stan_model3 |>
                   tidybayes::gather_draws(mu_alpha, sd_alpha, sigma)

alpha <- output_stan_model4 |> tidybayes::gather_draws(alpha[pred_id])

h_j <- output_stan_model4 |> tidybayes::gather_draws(h_j[pred_id])

# Get pred_id & habitat_type to join with the stan output
pred_ids <- unique(dataset[, c("pred_id", "habitat_type", "trophic_guild")])

alpha <-  dplyr::left_join(alpha, pred_ids, by = "pred_id") |>
          dplyr::mutate(pred_id = as.factor(pred_id),
                        habitat_type = as.factor(habitat_type),
                        trophic_guild = as.factor(trophic_guild))


h_j <- dplyr::left_join(h_j, pred_ids, by = "pred_id") |>
          dplyr::mutate(pred_id = as.factor(pred_id),
                        habitat_type = as.factor(habitat_type),
                        trophic_guild = as.factor(trophic_guild))

pred_ids <- pred_ids |> dplyr::mutate(pred_id = as.factor(pred_id),
                                      habitat_type = as.factor(habitat_type),
                                      trophic_guild = as.factor(trophic_guild))

# Plot posterior dist. of parameters
ggplot(general_params, aes(x = `.value`, y = .variable)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
    labs(title = "") +
     theme_ipsum() +
      theme(
       legend.title = element_text(size = 15),
       legend.text = element_text(size = 13),
       axis.title.y = element_blank(),
       axis.title.x = element_text(size = 18),
       plot.title = element_text(size = 30),
       axis.text.x = element_text(size = 20),
       axis.text.y = element_text(size = 20)
      ) +
       xlab("Parameters log-valued")

# Plot the unique alphas by predator grouped by habitat_type
pred_ids_habitat <- pred_ids |> dplyr::arrange(habitat_type)

ggplot(alpha, aes(x = `.value`, y = pred_id, fill = habitat_type)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
    labs(title = "Predators space clearance rate by habitat type (log scale)") +
     theme_ipsum() +
      theme(
       legend.title = element_text(size = 18),
       legend.text = element_text(size = 15),
       axis.title.y = element_text(size = 20),
       axis.title.x = element_text(size = 20),
       plot.title = element_text(size = 30),
       axis.text.x = element_text(size = 15),
       axis.text.y = element_blank()
      ) +
       ylab("") +
       xlab("Space clearance rate") +
         scale_fill_manual("Ecosystem type",values = c("terrestrial" = "olivedrab",
          "freshwater" = "sandybrown", "marine" = "deepskyblue3","marine_freshwater" = "slateblue4")) +
       scale_y_discrete(guide = guide_axis(n.dodge = 2), limits = pred_ids_habitat$pred_id)

# Plot the unique alphas by predator grouped by trophic_guild
pred_ids_guild <- pred_ids |> dplyr::arrange(trophic_guild)

ggplot(alpha, aes(x = `.value`, y = pred_id, fill = trophic_guild)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
    labs(title = "Predators space clearance rate by trophic guilds (log scale)") +
     theme_ipsum() +
      theme(
       legend.title = element_text(size = 15),
       legend.text = element_text(size = 13),
       axis.title.y = element_text(size = 20),
       axis.title.x = element_text(size = 20),
       plot.title = element_text(size = 30),
       axis.text.x = element_text(size = 15),
       axis.text.y = element_blank()
      ) +
       ylab("") +
       xlab("Space clearance rate") +
        scale_y_discrete(guide = guide_axis(n.dodge = 2), limits = pred_ids_guild$pred_id)

# Plot the unique h_j by predator
ggplot(h_j, aes(x = `.value`, y = pred_id, fill = habitat_type)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
    labs(title = "Predators handling time by habitat type (log scale)") +
     theme_ipsum() +
      theme(
       legend.title = element_text(size = 15),
       legend.text = element_text(size = 13),
       axis.title.y = element_text(size = 20),
       axis.title.x = element_text(size = 20),
       plot.title = element_text(size = 30),
       axis.text.x = element_text(size = 15),
       axis.text.y = element_text(size = 15)
      ) +
       ylab("Predators ID") +
       xlab("Handling time") +
         scale_fill_manual("Ecosystem type",values = c("terrestrial" = "olivedrab",
          "freshwater" = "sandybrown", "marine" = "deepskyblue3","marine_freshwater" = "slateblue4")) +
       scale_y_discrete(guide = guide_axis(n.dodge = 2), limits = pred_ids_habitat$pred_id)

ggplot(h_j, aes(x = `.value`, y = pred_id, fill = trophic_guild)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
    labs(title = "Predators handling time by trophic guilds (log scale)") +
     theme_ipsum() +
      theme(
       legend.title = element_text(size = 15),
       legend.text = element_text(size = 13),
       axis.title.y = element_text(size = 20),
       axis.title.x = element_text(size = 20),
       plot.title = element_text(size = 30),
       axis.text.x = element_text(size = 15),
       axis.text.y = element_text(size = 15)
      ) +
       ylab("Predators ID") +
       xlab("Handling time") +
       scale_y_discrete(guide = guide_axis(n.dodge = 2), limits = pred_ids_guild$pred_id)

# Predictions vs observed data
plot_sim_noerror(output_stan_model4)
plot_sim_error(output_stan_model4)

# Allometric relation between alpha and bodymass
alpha_bodymass <- output_stan_model3 |> tidybayes::gather_rvars(alpha[pred_id]) |>
                    dplyr::left_join(dataset, by = "pred_id") |>
                    dplyr::select(pred_id, .value, bodymass_mean_predator, trophic_guild, habitat_type) |>
                    dplyr::distinct() |>
                    dplyr::mutate(trophic_guild = as.factor(trophic_guild))

alpha_bodymass |>
  ggplot(aes(x = log(bodymass_mean_predator), dist = .value, col = habitat_type)) +
    stat_dist_pointinterval() +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      plot.title = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.title.x = element_text(size = 15),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13)
    ) +
    labs(title = "Relation of space clearance rate and bodymass") +
    xlab("Bodymass (g, log-scale)") +
    ylab("Space clearance rate (km²/ind*year, log-scale)") +
    geom_abline(intercept = 0, slope = 1)
