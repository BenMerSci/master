# One-one plot for simulation and observations
one_one_plot <- function(dataset, num_model) {
      dataset |>
        ggplot(aes(x = log(pred_flow), y = .data[[paste0("yrep_", num_model, "_mean")]])) +
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
        labs(title = paste("Model", num_model, " ")) +
        xlab("Observed values") +
        ylab("Simulated values") +
        geom_abline(intercept = 0, slope = 1)
}

# Plots with dist. posterior intervals against observations with colors
plot_sim_noerror <- function(model) {
  df <- model |>
        tidybayes::gather_rvars(log_pred_flow_hat[id]) |>
        dplyr::left_join(dataset |> mutate(id = row_number()))

  df |> ggplot(aes(x= log(biomass_prey) + log(abundance_predator), dist=.value), col="Posterior distribution") +
        stat_dist_pointinterval() +
        geom_point(aes(x=log(biomass_prey) + log(abundance_predator),
         y=log(pred_flow), col=habitat_type), inherit.aes=FALSE, size=3, alpha=0.8) +
        theme_minimal() +
        theme(
         legend.position = "right",
         legend.title = element_blank(),
         plot.title = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         axis.title.x = element_text(size = 15),
         axis.text.x = element_text(size = 13),
         axis.text.y = element_text(size = 13)
        ) +
        labs(title = "Observed against simulated from posterior distribution biomass flows (log-scaled)") +
        xlab(expression(B[i] * N[j])) +
        ylab("Biomass flow") +
        scale_colour_manual(name="Line color", values=c(`Posterior distribution`="black", "terrestrial"="olivedrab",
                            "freshwater" = "sandybrown", "marine"="deepskyblue3", "marine_freshwater"="purple"))
}

plot_sim_error <- function(model) {
  df <- model |>
        tidybayes::gather_rvars(y_rep[id]) |>
        dplyr::left_join(dataset |> mutate(id = row_number()))

  df |> ggplot(aes(x= log(biomass_prey) + log(abundance_predator), dist=.value), col="Posterior distribution") +
        stat_dist_pointinterval() +
        geom_point(aes(x=log(biomass_prey) + log(abundance_predator),
         y=log(pred_flow), col=habitat_type), inherit.aes=FALSE, size=3, alpha=0.8) +
        theme_minimal() +
        theme(
         legend.position = "right",
         legend.title = element_blank(),
         plot.title = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         axis.title.x = element_text(size = 15),
         axis.text.x = element_text(size = 13),
         axis.text.y = element_text(size = 13)
        ) +
        labs(title = "Observed against simulated from posterior distribution biomass flows (log-scaled)") +
        xlab(expression(B[i] * N[j])) +
        ylab("Biomass flow") +
        scale_colour_manual(name="Line color", values=c(`Posterior distribution`="black", "terrestrial"="olivedrab",
                            "freshwater" = "sandybrown", "marine"="deepskyblue3", "marine_freshwater"="purple"))
}