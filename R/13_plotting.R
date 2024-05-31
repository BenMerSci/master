library(ggplot2)
library(ggridges)
library(hrbrthemes)
library(ggdist)
library(ggthemes)
library(RColorBrewer)
source("lib/rsq.R")
source("lib/plot_functions.R")

# Load data
dataset <- readRDS("data/clean/new_dataset.RDS")
fit0 <- readRDS("results/model_outputs/stanfit_model0.RDS") |>
                        tidybayes::recover_types()
fit1 <- readRDS("results/model_outputs/stanfit_model1.RDS") |>
                        tidybayes::recover_types()
fit2 <- readRDS("results/model_outputs/stanfit_model2.RDS") |>
                        tidybayes::recover_types()
fit3 <- readRDS("results/model_outputs/stanfit_model3.RDS") |>
                        tidybayes::recover_types()
fit4 <- readRDS("results/model_outputs/stanfit_model4.RDS") |>
                        tidybayes::recover_types()

# Parameters ridge plots
alpha_trophic_2 <- alpha_ridge_plot(fit2, dataset = dataset)

alpha_trophic_3 <- alpha_ridge_plot(fit3, dataset = dataset)

alpha_trophic_4 <- alpha_ridge_plot(fit4, dataset = dataset)

mylegend <- g_legend(alpha_trophic_2)

plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(alpha_trophic_2 + theme(legend.position = "none"),
                         alpha_trophic_3 + theme(legend.position = "none"),
                         alpha_trophic_4 + theme(legend.position = "none"),
                         nrow = 1), mylegend, nrow = 2, heights = c(10, 1))

ggsave("figures/alpha_comparison.png", plot = plots, dpi = "retina")

# Handling time parameters
ht3_trophic <- ht_ridge_plot(fit3, dataset = dataset)

ht4_trophic <- ht_ridge_plot(fit4, dataset = dataset)

mylegend <- g_legend(ht3_trophic)

plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(ht3_trophic + theme(legend.position = "none"),
                         ht4_trophic + theme(legend.position = "none"),
                         nrow = 1), mylegend, nrow = 2, heights = c(10, 1))

ggsave("figures/ht_comparison.png", plot = plots, dpi = "retina")

# R square
rsq_plot <- plot_rsq(list(fit1, fit2, fit3, fit4))

ggsave("figures/rsq_plot.png", plot = rsq_plot, dpi = "retina")

# Predictive plots
model0_pred <- plot_sim_error(fit0, dataset = dataset) +
                  theme(legend.position = "none") + labs(title = "Model 0") +
                  ylim(c(-36, 26))

model1_pred <- plot_sim_error(fit1, dataset = dataset) +
                  theme(legend.position = "none", axis.title.y = element_blank(),
                  axis.text.y = element_blank()) + labs(title = "Model 1") + ylim(c(-36, 26))

model2_pred <- plot_sim_error(fit2, dataset = dataset) +
                  theme(legend.position = "bottom") + labs(title = "Model 2")

model3_pred <- plot_sim_error(fit3, dataset = dataset) +
                  theme(legend.position = "none", axis.title.y = element_blank(),
                  axis.text.y = element_blank()) + labs(title = "Model 3")

model4_pred <- plot_sim_error(fit4, dataset = dataset) +
                  theme(legend.position = "none", axis.title.y = element_blank(),
                  axis.text.y = element_blank()) + labs(title = "Model 4")

mylegend <- g_legend(model2_pred)

plots <- gridExtra::grid.arrange(grid::nullGrob(), model0_pred, model1_pred, grid::nullGrob(),
           model2_pred + theme(legend.position = "none"), model3_pred, model4_pred,
           bottom = mylegend, nrow = 2, layout_matrix = matrix(c(1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7),
           nrow = 2, ncol = 6, byrow = TRUE))

ggsave("figures/pred_plots.png", plot = plots, dpi = "retina")

# One-one plot
model0_oneone <- one_one_plot(fit0, dataset) +
                   theme(legend.position = "none") + labs(title = "Model 0") +
                   ylim(c(-36, 26))

model1_oneone <- one_one_plot(fit1, dataset) + theme(legend.position = "none",
                   axis.title.y = element_blank(), axis.text.y = element_blank()) +
                   labs(title = "Model 1") + ylim(c(-36, 26))

model2_oneone <- one_one_plot(fit2, dataset) + theme(legend.position = "bottom") +
                   labs(title = "Model 2")

model3_oneone <- one_one_plot(fit3, dataset) + theme(legend.position = "none",
                   axis.title.y = element_blank(), axis.text.y = element_blank()) +
                   labs(title = "Model 3")

model4_oneone <- one_one_plot(fit4, dataset) + theme(legend.position = "none",
                   axis.title.y = element_blank(), axis.text.y = element_blank()) +
                   labs(title = "Model 4")

mylegend <- g_legend(model2_oneone)

plots <- gridExtra::grid.arrange(grid::nullGrob(), model0_oneone, model1_oneone, grid::nullGrob(),
           model2_oneone + theme(legend.position = "none"), model3_oneone, model4_oneone,
           bottom = mylegend, nrow = 2, layout_matrix = matrix(c(1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7),
           nrow = 2, ncol = 6, byrow = TRUE))

ggsave("figures/oneone_plots.png", plot = plots, dpi = "retina")

# allometric plot with alpha and handling time
# Model2

alpha_bm2 <- alpha_bodymass_plot_lm(fit2, dataset = dataset) + theme(legend.position = "none") +
             labs(title = expression("Model 2 -" * " " * alpha)) + ylim(c(-20, 10))

# Model 3
alpha_bm3 <- alpha_bodymass_plot(fit3, dataset = dataset) + theme(legend.position = "bottom") +
             labs(title = expression("Model 3 - " * " " * alpha)) + ylim(c(-20, 10))

ht_bm3 <- ht_bodymass_plot(fit3, dataset = dataset) + theme(legend.position = "none") +
          labs(title = expression("Model 3 - " * " " * h[j])) + ylim(c(-20, 10))

mylegend <- g_legend(alpha_bm3)

plots <- gridExtra::grid.arrange(alpha_bm2, alpha_bm3 + theme(legend.position = "none"), ht_bm3, bottom = mylegend, nrow = 1, ncol = 3)

ggsave("figures/model2_3_relationship.png", plot = plots, dpi = "retina")
``
# Model 4
alpha_bm4 <- alpha_bodymass_plot(fit4, dataset = dataset) + theme(legend.position = "bottom") + ylim(c(-20,10))

ht_bm4 <- ht_bodymass_plot(fit4, dataset = dataset) + theme(legend.position = "none") + ylim(c(-20,10))

mylegend <- g_legend(alpha_bm4)

plots <- gridExtra::grid.arrange(alpha_bm4 + theme(legend.position = "none"), ht_bm4, bottom = mylegend, nrow = 1, ncol = 2)

ggsave("figures/model4_relationship.png", plot = plots, dpi = "retina")
