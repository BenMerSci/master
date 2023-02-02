# One-one plot for simulation and observations
one_one_plot <- function(dataset, num_model) {
      dataset |>
        ggplot(aes(x = log(biomass_flow), y = .data[[paste0("yrep_", num_model, "_mean")]])) +
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
        tidybayes::gather_rvars(log_biomass_flow_hat[id]) |>
        dplyr::left_join(dataset |> dplyr::mutate(id = dplyr::row_number())) |>
        dplyr::mutate(habitat_type = as.factor(habitat_type))

  levels(df$habitat_type) <- list("Terrestrial" = "terrestrial",
                                         "Freshwater" = "freshwater",
                                         "Marine" = "marine",
                                         "Marine & freshwater" = "marine_freshwater")

  df |> ggplot(aes(x= log(biomass_prey) + log(abundance_predator), dist=.value, col="Model predictions")) +
        stat_dist_pointinterval() +
        geom_point(aes(x=log(biomass_prey) + log(abundance_predator),
         y=log(biomass_flow), col=habitat_type), inherit.aes=FALSE, size=3, alpha=0.8) +
        theme_minimal() +
        theme(
         legend.position = "bottom",
         legend.title = element_blank(),
         legend.text = element_text(size = 15),
         plot.title = element_text(size = 18),
         axis.title.y = element_text(size = 17),
         axis.title.x = element_text(size = 17),
         axis.text.x = element_text(size = 15),
         axis.text.y = element_text(size = 15)
        ) +
        labs(title = "Observation biomass flow vs predicted (log-scale)") +
        xlab(expression(log(B[i] * N[j]))) +
        ylab(expression(log(F[ij]) (ton/km^2*year))) +
        scale_colour_manual(values=c("Model predictions"="black", "Terrestrial"="olivedrab",
                            "Freshwater" = "sandybrown", "Marine"="deepskyblue3", "Marine & freshwater"="purple"),
                            limits= c("Model predictions","Terrestrial","Freshwater","Marine","Marine & freshwater"))
}

plot_sim_error <- function(model) {
      
  df <- model |>
        tidybayes::gather_rvars(y_rep[id]) |>
        dplyr::left_join(dataset |> dplyr::mutate(id = dplyr::row_number())) |>
        dplyr::mutate(habitat_type = as.factor(habitat_type))

  levels(df$habitat_type) <- list("Terrestrial" = "terrestrial",
                                         "Freshwater" = "freshwater",
                                         "Marine" = "marine",
                                         "Marine & freshwater" = "marine_freshwater")

  df |> ggplot(aes(x= log(biomass_prey) + log(abundance_predator), dist=.value, col="Model predictions")) +
        stat_dist_pointinterval() +
        geom_point(aes(x=log(biomass_prey) + log(abundance_predator),
         y=log(biomass_flow), col=habitat_type), inherit.aes=FALSE, size=3, alpha=0.8) +
        theme_minimal() +
        theme(
         legend.title = element_blank(),
         legend.text = element_text(size = 15),
         legend.position = "bottom",
         plot.title = element_text(size = 18),
         axis.title.y = element_text(size = 17),
         axis.title.x = element_text(size = 17),
         axis.text.x = element_text(size = 15),
         axis.text.y = element_text(size = 15)
        ) +
        labs(title = "Observation biomass flow vs predicted (log-scale)") +
        xlab(expression(log(B[i] * N[j]))) +
        ylab(expression(log(F[ij]) (ton/km^2*year))) +
        scale_colour_manual(values=c("Model predictions"="black", "Terrestrial"="olivedrab",
                            "Freshwater" = "sandybrown", "Marine"="deepskyblue3", "Marine & freshwater"="purple"),
                            limits=(c("Model predictions","Terrestrial","Freshwater","Marine","Marine & freshwater")))
}