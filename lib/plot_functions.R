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

 model |>
  tidybayes::gather_draws(alpha[pred_id]) |>
  dplyr::left_join(pred_ids, by = "pred_id") |>
  dplyr::mutate(pred_id = as.factor(pred_id),
    habitat_type = as.factor(habitat_type),
    trophic_guild = factor(trophic_guild, 
      levels = c(
        "Small demersal omnivore", "Medium demersal omnivore",
        "Small demersal carnivore", "Medium demersal carnivore",
        "Small pelagic omnivore", "Medium pelagic omnivore",
        "Small pelagic carnivore", "Medium pelagic carnivore",
        "Small reef-coast", "Medium reef-coast",
        "Sharks", "Cephalopods", "Mollusc and crustacea",
        "Shrimps", "Plankton", "Invertebrates", 
        "Reptiles", "Non-predatory birds", "Predatory birds",
        "Small mammal herbivore", "Large mammal herbivore",
        "Small mammal predator", "Medium mammal predator",
        "Large mammal predator"
      )
    )
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    pred_nb = which(trophic_guild == levels(trophic_guild))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    pred_id = reorder(pred_id, pred_nb)
  ) |>
 ggplot() +
 aes(x = `.value`, y = pred_id, fill = trophic_guild) +
  geom_density_ridges(scale = 5, rel_min_height = 0.01, alpha = 0.9)  +
  scale_fill_manual(values = c(
        "Small demersal omnivore" = "aquamarine4", "Medium demersal omnivore" = "aquamarine2",
        "Small demersal carnivore" = "deepskyblue4", "Medium demersal carnivore" = "deepskyblue2",
        "Small pelagic omnivore" = "mediumpurple4", "Medium pelagic omnivore" = "mediumpurple2",
        "Small pelagic carnivore" = "palevioletred4", "Medium pelagic carnivore" = "palevioletred2",
        "Small reef-coast" = "lightsalmon3", "Medium reef-coast" = "lightsalmon1",
        "Sharks" = "ivory3", "Cephalopods" = "greenyellow", "Mollusc and crustacea" = "lightseagreen",
        "Shrimps" = "powderblue" , "Plankton" = "brown", "Invertebrates" = "violetred", 
        "Reptiles" = "palegoldenrod", "Non-predatory birds" = "yellow4", "Predatory birds" = "yellow2",
        "Small mammal herbivore" = "mistyrose4", "Large mammal herbivore" = "mistyrose2",
        "Small mammal predator" = "goldenrod4", "Medium mammal predator" = "goldenrod3",
        "Large mammal predator" = "goldenrod1")) +
     theme_hc() +
      theme(
       legend.position = "bottom",
       legend.text = element_text(size = 10),
       legend.title = element_text(size = 12),
       plot.title = element_text(size = 15),
       axis.title.y = element_blank(),
       axis.title.x = element_blank(),
       axis.text.y = element_blank(),
       axis.text.x = element_text(size = 15),
      ) +
       labs(title = paste0("Model ", model_number), fill = "Trophic guilds") +
       xlim(c(-20.5, 10))
}

ht_ridge_plot <- function(model, dataset) {

model_number <- gsub("\\D", "", deparse(substitute(model)))

 pred_ids <- unique(dataset[, c("pred_id", "habitat_type", "trophic_guild")])

model |>
  tidybayes::gather_draws(ht[pred_id]) |>
  dplyr::left_join(pred_ids, by = "pred_id") |>
  dplyr::mutate(pred_id = as.factor(pred_id),
    habitat_type = as.factor(habitat_type),
    trophic_guild = factor(trophic_guild, 
      levels = c(
        "Small demersal omnivore", "Medium demersal omnivore",
        "Small demersal carnivore", "Medium demersal carnivore",
        "Small pelagic omnivore", "Medium pelagic omnivore",
        "Small pelagic carnivore", "Medium pelagic carnivore",
        "Small reef-coast", "Medium reef-coast",
        "Sharks", "Cephalopods", "Mollusc and crustacea",
        "Shrimps", "Plankton", "Invertebrates", 
        "Reptiles", "Non-predatory birds", "Predatory birds",
        "Small mammal herbivore", "Large mammal herbivore",
        "Small mammal predator", "Medium mammal predator",
        "Large mammal predator"
      )
    )
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    pred_nb = which(trophic_guild == levels(trophic_guild))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    pred_id = reorder(pred_id, pred_nb)
  ) |>
 ggplot() +
 aes(x = `.value`, y = pred_id, fill = trophic_guild) +
  geom_density_ridges(scale = 5, rel_min_height = 0.01, alpha = 0.9)  +
  scale_fill_manual(values = c(
        "Small demersal omnivore" = "aquamarine4", "Medium demersal omnivore" = "aquamarine2",
        "Small demersal carnivore" = "deepskyblue4", "Medium demersal carnivore" = "deepskyblue2",
        "Small pelagic omnivore" = "mediumpurple4", "Medium pelagic omnivore" = "mediumpurple2",
        "Small pelagic carnivore" = "palevioletred4", "Medium pelagic carnivore" = "palevioletred2",
        "Small reef-coast" = "lightsalmon3", "Medium reef-coast" = "lightsalmon1",
        "Sharks" = "ivory3", "Cephalopods" = "greenyellow", "Mollusc and crustacea" = "lightseagreen",
        "Shrimps" = "powderblue" , "Plankton" = "brown", "Invertebrates" = "violetred", 
        "Reptiles" = "palegoldenrod", "Non-predatory birds" = "yellow4", "Predatory birds" = "yellow2",
        "Small mammal herbivore" = "mistyrose4", "Large mammal herbivore" = "mistyrose2",
        "Small mammal predator" = "goldenrod4", "Medium mammal predator" = "goldenrod3",
        "Large mammal predator" = "goldenrod1")) +
     theme_hc() +
      theme(
       legend.position = "bottom",
       legend.text = element_text(size = 10),
       legend.title = element_text(size = 12),
       plot.title = element_text(size = 15),
       axis.title.y = element_blank(),
       axis.title.x = element_blank(),
       axis.text.y = element_blank(),
       axis.text.x = element_text(size = 15),
      ) +
       labs(title = paste0("Model ", model_number), fill = "Trophic guilds") +
       xlim(c(-20.5, 10))
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
        stat_pointinterval(point_interval = "mean_qi", alpha=0.8) +
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
        xlab("Observed ") +
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
        stat_pointinterval() +
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
        stat_pointinterval() +
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
  stat_pointinterval(point_interval = "mean_qi") +
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
    xlab("Body mass (metric tons, log-scale)") +
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
  stat_pointinterval(point_interval = "mean_qi") +
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
    xlab("Body mass (metric tons, log-scale)") +
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
    stat_pointinterval(point_interval = "mean_qi") +
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
    xlab("Body mass (metric tons, log-scale)") +
    ylab("Handling time (year*km²/tons, log-scale)") +
    scale_colour_manual(values=c("Terrestrial"="olivedrab",
                            "Freshwater" = "sandybrown", "Marine"="deepskyblue3", "Marine & freshwater"="purple"),
                            limits=c("Terrestrial","Freshwater","Marine","Marine & freshwater"))
}
