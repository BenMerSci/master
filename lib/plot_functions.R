# Funtion for common legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Ridge plots
alpha_ridge_plot <- function(model, dataset) {

model_number <- gsub("\\D", "", deparse(substitute(model)))

 pred_ids <- unique(dataset[, c("pred_id", "habitat_type", "trophic_guild")])

 df <- model |> tidybayes::gather_draws(alpha[pred_id]) |>
            dplyr::left_join(pred_ids, by = "pred_id") |>
              dplyr::mutate(pred_id = as.factor(pred_id),
                        habitat_type = as.factor(habitat_type),
                        trophic_guild = as.factor(trophic_guild))

pred_ids <- pred_ids |> dplyr::mutate(pred_id = as.factor(pred_id),
                                      habitat_type = as.factor(habitat_type),
                                      trophic_guild = as.factor(trophic_guild))

pred_ids_guild <- pred_ids |> dplyr::arrange(trophic_guild)

 ggplot(df, aes(x = `.value`, y = pred_id, fill = trophic_guild)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
     theme_hc() +
      theme(
       legend.position = "bottom",
       legend.text = element_text(size = 9),
       legend.title = element_text(size = 12),
       plot.title = element_text(size = 15),
       axis.title.y = element_blank(),
       axis.title.x = element_blank(),
       axis.text.y = element_blank(),
       axis.text.x = element_text(size = 15),
      ) +
       labs(title = paste0("Model ", model_number), fill = "Trophic guilds") +
       xlim(c(-20, 10)) +
        scale_y_discrete(guide = guide_axis(n.dodge = 2), limits = pred_ids_guild$pred_id)
}

ht_ridge_plot <- function(model, dataset) {

model_number <- gsub("\\D", "", deparse(substitute(model)))

 pred_ids <- unique(dataset[, c("pred_id", "habitat_type", "trophic_guild")])

 df <- model |> tidybayes::gather_draws(ht[pred_id]) |>
            dplyr::left_join(pred_ids, by = "pred_id") |>
              dplyr::mutate(pred_id = as.factor(pred_id),
                        habitat_type = as.factor(habitat_type),
                        trophic_guild = as.factor(trophic_guild))

pred_ids <- pred_ids |> dplyr::mutate(pred_id = as.factor(pred_id),
                                      habitat_type = as.factor(habitat_type),
                                      trophic_guild = as.factor(trophic_guild))

pred_ids_guild <- pred_ids |> dplyr::arrange(trophic_guild)

 ggplot(df, aes(x = `.value`, y = pred_id, fill = trophic_guild)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
     theme_hc() +
      theme(
       legend.position = "bottom",
       legend.text = element_text(size = 9),
       legend.title = element_text(size = 12),
       plot.title = element_text(size = 15),
       axis.title.y = element_blank(),
       axis.title.x = element_blank(),
       axis.text.y = element_blank(),
       axis.text.x = element_text(size = 15),
      ) +
       labs(title = paste0("Model ", model_number), fill = "Trophic guilds") +
       xlim(c(-20, 10)) +
        scale_y_discrete(guide = guide_axis(n.dodge = 2), limits = pred_ids_guild$pred_id)


}

# One-one plot for simulation and observations
one_one_plot <- function(model, dataset) {

  df <- model |>
        tidybayes::gather_rvars(y_rep[id]) |>
        dplyr::left_join(dataset |> dplyr::mutate(id = dplyr::row_number())) |>
        dplyr::mutate(habitat_type = as.factor(habitat_type))

  levels(df$habitat_type) <- list("Terrestrial" = "terrestrial",
                                         "Freshwater" = "freshwater",
                                         "Marine" = "marine",
                                         "Marine & freshwater" = "marine_freshwater")

df |> ggplot(aes(x= log(biomass_flow), dist=.value, col=habitat_type)) +
        stat_dist_pointinterval(alpha=0.8) +
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
        xlab("Observed") +
        ylab("Predicted") +
        ylim(c(-20, 20)) +
        scale_colour_manual(values=c("Terrestrial"="olivedrab",
                            "Freshwater" = "sandybrown", "Marine"="deepskyblue3", "Marine & freshwater"="purple"),
                            limits= c("Terrestrial","Freshwater","Marine","Marine & freshwater")) +
        geom_abline(intercept = 0, slope = 1)
}

# Plots with dist. posterior intervals against observations with colors
plot_sim_noerror <- function(model, dataset) {
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

plot_sim_error <- function(model, dataset) {
      
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
         plot.title = element_text(size = 18),
         axis.title.y = element_text(size = 17),
         axis.title.x = element_text(size = 17),
         axis.text.x = element_text(size = 15),
         axis.text.y = element_text(size = 15)
        ) +
        xlab(expression(log(B[i] * N[j]))) +
        ylab(expression(log(F[ij]) (ton/km^2*year))) +
        ylim(c(-20,20)) +
        scale_colour_manual(values=c("Model predictions"="black", "Terrestrial"="olivedrab",
                            "Freshwater" = "sandybrown", "Marine"="deepskyblue3", "Marine & freshwater"="purple"),
                            limits=(c("Model predictions","Terrestrial","Freshwater","Marine","Marine & freshwater")))
}

alpha_bodymass_plot_lm <- function(model,  dataset = dataset) {

parameter_bodymass <- model |> tidybayes::gather_rvars(alpha[pred_id]) |>
                    dplyr::left_join(dataset, by = "pred_id") |>
                    dplyr::select(pred_id, .value, bodymass_mean_predator, trophic_guild, habitat_type) |>
                    dplyr::distinct() |>
                    dplyr::mutate(trophic_guild = as.factor(trophic_guild), mean_alpha = mean(.value))

parameter_bodymass[which(parameter_bodymass$habitat_type == "terrestrial"),"habitat_type"] <- "Terrestrial"
parameter_bodymass[which(parameter_bodymass$habitat_type == "marine"),"habitat_type"] <- "Marine"
parameter_bodymass[which(parameter_bodymass$habitat_type == "freshwater"),"habitat_type"] <- "Freshwater"
parameter_bodymass[which(parameter_bodymass$habitat_type == "marine_freshwater"),"habitat_type"] <- "Marine & freshwater"

parameter_bodymass |>
  ggplot(aes(x = log(bodymass_mean_predator), dist = .value, col = factor(habitat_type))) +
  stat_pointinterval() +
  geom_smooth(aes(x = log(bodymass_mean_predator), y = mean_alpha), method = "lm", color = "black") +
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
    ylim(-19,6) +
    xlim(-19,1) +
    xlab("Body mass (g, log-scale)") +
    ylab("Space clearance rate (km²/ind*year, log-scale)") +
    scale_colour_manual(values=c("Terrestrial"="olivedrab",
                            "Freshwater" = "sandybrown", "Marine"="deepskyblue3", "Marine & freshwater"="purple"),
                            limits=c("Terrestrial","Freshwater","Marine","Marine & freshwater")) 
}

alpha_bodymass_plot <- function(model,  dataset = dataset) {

parameter_bodymass <- model |> tidybayes::gather_rvars(alpha[pred_id]) |>
                    dplyr::left_join(dataset, by = "pred_id") |>
                    dplyr::select(pred_id, .value, bodymass_mean_predator, trophic_guild, habitat_type) |>
                    dplyr::distinct() |>
                    dplyr::mutate(trophic_guild = as.factor(trophic_guild))

parameter_bodymass[which(parameter_bodymass$habitat_type == "terrestrial"),"habitat_type"] <- "Terrestrial"
parameter_bodymass[which(parameter_bodymass$habitat_type == "marine"),"habitat_type"] <- "Marine"
parameter_bodymass[which(parameter_bodymass$habitat_type == "freshwater"),"habitat_type"] <- "Freshwater"
parameter_bodymass[which(parameter_bodymass$habitat_type == "marine_freshwater"),"habitat_type"] <- "Marine & freshwater"

parameter_bodymass |>
  ggplot(aes(x = log(bodymass_mean_predator), dist = .value, col = factor(habitat_type))) +
  stat_pointinterval() +
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
    ylim(-19,6) +
    xlim(-19,1) +
    xlab("Body mass (g, log-scale)") +
    ylab("Space clearance rate (km²/ind*year, log-scale)") +
    scale_colour_manual(values=c("Terrestrial"="olivedrab",
                            "Freshwater" = "sandybrown", "Marine"="deepskyblue3", "Marine & freshwater"="purple"),
                            limits=c("Terrestrial","Freshwater","Marine","Marine & freshwater"))
}

ht_bodymass_plot <- function(model,  dataset = dataset) {

parameter_bodymass <- model |> tidybayes::gather_rvars(ht[pred_id]) |>
                    dplyr::left_join(dataset, by = "pred_id") |>
                    dplyr::select(pred_id, .value, bodymass_mean_predator, trophic_guild, habitat_type) |>
                    dplyr::distinct() |>
                    dplyr::mutate(trophic_guild = as.factor(trophic_guild))

parameter_bodymass[which(parameter_bodymass$habitat_type == "terrestrial"),"habitat_type"] <- "Terrestrial"
parameter_bodymass[which(parameter_bodymass$habitat_type == "marine"),"habitat_type"] <- "Marine"
parameter_bodymass[which(parameter_bodymass$habitat_type == "freshwater"),"habitat_type"] <- "Freshwater"
parameter_bodymass[which(parameter_bodymass$habitat_type == "marine_freshwater"),"habitat_type"] <- "Marine & freshwater"

parameter_bodymass |>
  ggplot(aes(x = log(bodymass_mean_predator), dist = .value, col = habitat_type)) +
    stat_dist_pointinterval() +
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
    ylim(-10,6) +
    xlim(-17,1) +
    xlab("Body mass (g, log-scale)") +
    ylab("Handling time (year*km²/tons, log-scale)") +
    scale_colour_manual(values=c("Terrestrial"="olivedrab",
                            "Freshwater" = "sandybrown", "Marine"="deepskyblue3", "Marine & freshwater"="purple"),
                            limits=c("Terrestrial","Freshwater","Marine","Marine & freshwater"))
}
