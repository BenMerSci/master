library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(ggdist)
library(ggthemes)
source("lib/rsq.R")
source("lib/plot_functions.R")

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

# Parameters ridge plots

alpha_trophic_2 <- alpha_ridge_plot(output_stan_model2, dataset = dataset)

alpha_trophic_3 <- alpha_ridge_plot(output_stan_model3, dataset = dataset)

alpha_trophic_4 <- alpha_ridge_plot(output_stan_model4, dataset = dataset)

mylegend <- g_legend(alpha_trophic_2)

plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(alpha_trophic_2 + theme(legend.position = "none"),
                         alpha_trophic_3 + theme(legend.position = "none"),
                         alpha_trophic_4 + theme(legend.position = "none"),
                         nrow = 1), mylegend, nrow = 2, heights = c(10, 1))

ggsave("figures/alpha_comparison.png", plot = plots, dpi = "retina")

# Handling time parameters
hj3_trophic <- ht_ridge_plot(output_stan_model3, dataset = dataset)

hj4_trophic <- ht_ridge_plot(output_stan_model4, dataset = dataset)

mylegend <- g_legend(hj3_trophic)

plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(hj3_trophic + theme(legend.position = "none"),
                         hj4_trophic + theme(legend.position = "none"),
                         nrow = 1), mylegend, nrow = 2, heights = c(10, 1))

ggsave("figures/hj_comparison.png", plot = plots, dpi = "retina")

# R square
rsq_plot <- plot_rsq(list(output_stan_model1, output_stan_model2, output_stan_model3, output_stan_model4))

ggsave("figures/rsq_plot.png", plot = rsq_plot, dpi = "retina")

# Predictive plots
model0_pred <- plot_sim_error(output_stan_model0, dataset = dataset) +
                  theme(legend.position = "none") + labs(title = "Model 0") +
                  ylim(c(-36, 26))

model1_pred <- plot_sim_error(output_stan_model1, dataset = dataset) +
                  theme(legend.position = "none", axis.title.y = element_blank(),
                  axis.text.y = element_blank()) + labs(title = "Model 1") + ylim(c(-36, 26))

model2_pred <- plot_sim_error(output_stan_model2, dataset = dataset) +
                  theme(legend.position = "bottom") + labs(title = "Model 2")

model3_pred <- plot_sim_error(output_stan_model3, dataset = dataset) +
                  theme(legend.position = "none", axis.title.y = element_blank(),
                  axis.text.y = element_blank()) + labs(title = "Model 3")

model4_pred <- plot_sim_error(output_stan_model4, dataset = dataset) +
                  theme(legend.position = "none", axis.title.y = element_blank(),
                  axis.text.y = element_blank()) + labs(title = "Model 4")

mylegend <- g_legend(model2_pred)

plots <- gridExtra::grid.arrange(grid::nullGrob(), model0_pred, model1_pred, grid::nullGrob(),
           model2_pred + theme(legend.position = "none"), model3_pred, model4_pred,
           bottom = mylegend, nrow = 2, layout_matrix = matrix(c(1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7),
           nrow = 2, ncol = 6, byrow = TRUE))

ggsave("figures/pred_plots.png", plot = plots, dpi = "retina")

# One-one plot
model0_oneone <- one_one_plot(output_stan_model0, dataset) +
                   theme(legend.position = "none") + labs(title = "Model 0") +
                   ylim(c(-36, 26))

model1_oneone <- one_one_plot(output_stan_model1, dataset) + theme(legend.position = "none",
                   axis.title.y = element_blank(), axis.text.y = element_blank()) +
                   labs(title = "Model 1") + ylim(c(-36, 26))

model2_oneone <- one_one_plot(output_stan_model2, dataset) + theme(legend.position = "bottom") +
                   labs(title = "Model 2")

model3_oneone <- one_one_plot(output_stan_model3, dataset) + theme(legend.position = "none",
                   axis.title.y = element_blank(), axis.text.y = element_blank()) +
                   labs(title = "Model 3")

model4_oneone <- one_one_plot(output_stan_model4, dataset) + theme(legend.position = "none",
                   axis.title.y = element_blank(), axis.text.y = element_blank()) +
                   labs(title = "Model 4")

mylegend <- g_legend(model2_oneone)

plots <- gridExtra::grid.arrange(grid::nullGrob(), model0_oneone, model1_oneone, grid::nullGrob(),
           model2_oneone + theme(legend.position = "none"), model3_oneone, model4_oneone,
           bottom = mylegend, nrow = 2, layout_matrix = matrix(c(1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7),
           nrow = 2, ncol = 6, byrow = TRUE))

ggsave("figures/oneone_plots.png", plot = plots, dpi = "retina")

# allometric plot with alpha and handling time

alpha_bm2 <- alpha_bodymass_plot(output_stan_model2, dataset = dataset) + theme(legend.position = "bottom") + labs(title = "Model 2")

alpha_bm3 <- alpha_bodymass_plot(output_stan_model3, dataset = dataset) +
                                 theme(legend.position = "none", axis.title.y = element_blank(),
                                      axis.text.y = element_blank()) +
                                 labs(title = "Model 3")

alpha_bm4 <- alpha_bodymass_plot(output_stan_model4, dataset = dataset) +
                                 theme(legend.position = "none", axis.title.y = element_blank(),
                                 axis.text.y = element_blank()) +
                                 labs(title = "Model 4")

mylegend <- g_legend(alpha_bm2)

plots <- gridExtra::grid.arrange(alpha_bm2 + theme(legend.position = "none"), alpha_bm3, alpha_bm4, bottom = mylegend, nrow = 1, ncol = 3)

ggsave("figures/alpha_bodymass.png", plot = plots, dpi = "retina")

# handling time
ht_bm3 <- ht_bodymass_plot(output_stan_model3, dataset = dataset) + theme(legend.position = "bottom") + labs(title = "Model 3")

ht_bm4 <- ht_bodymass_plot(output_stan_model4, dataset = dataset) +
                                 theme(legend.position = "none", axis.title.y = element_blank(),
                                 axis.text.y = element_blank()) +
                                 labs(title = "Model 4")

mylegend <- g_legend(ht_bm3)

plots <- gridExtra::grid.arrange(ht_bm3 + theme(legend.position = "none"), ht_bm4, bottom = mylegend, nrow = 1, ncol = 2)

ggsave("figures/ht_bodymass.png", plot = plots, dpi = "retina")
