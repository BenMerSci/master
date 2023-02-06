# Load the data
dataset <- readRDS("data/clean/new_dataset.RDS")
coords_habitat <- readRDS("data/intermediate/enviro_traits.RDS") |>
        dplyr::filter(!is.na(currency_units), currency_units == "Wet weight (t/km^2)") |>
        dplyr::select(c("habitat_type","lon","lat")) |>
        dplyr::group_by(habitat_type)

# Map of the networks
library(ggplot2)
world <- map_data("world")
ggplot() + geom_map(data = world, map = world, aes(x = long, y = lat, map_id=region),
                  fill = "gray") +
      geom_point(data=coords_habitat, aes(x=lon, y=lat, color=habitat_type), size = 3) +
      scale_color_manual(values= c("marine" = "deepskyblue3", "freshwater" = "sandybrown", "terrestrial" = "olivedrab")) +
      coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-90, 90)) +
      labs(title="Global distribution of the Ecopath networks", x="Longitude", y="Latitude") +
      labs(color = "Ecosystem") +
      scale_x_continuous(breaks = seq(-200, 200, 50)) +
      scale_y_continuous(breaks = seq(-90, 90, 30)) +
      theme_bw() +
      theme(
         legend.title = element_blank(),
         legend.text = element_text(size = 15),
         legend.position = "bottom",
         plot.title = element_text(size = 18),
         axis.title.y = element_text(size = 15),
         axis.title.x = element_text(size = 15),
         axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14)
      )

ggsave("figures/network_map.png", dpi = "retina")

# Check biomass flow relation with prey biomass and predator abundances
terrestrial <- dataset[which(dataset$habitat_type == "terrestrial"), ]
freshwater <- dataset[which(dataset$habitat_type == "freshwater"), ]
marine <- dataset[which(dataset$habitat_type == "marine"), ]

# Create axis label
x_exp <- expression(B[i] ~ N[j])
x_exp_log <- expression(log(B[i]) + log(N[j]))

# terrestrial
plot1 <- ggplot(data = terrestrial, aes(x = (biomass_prey *
         abundance_predator), y = biomass_flow)) +
         geom_point(color = "olivedrab") +
         theme(legend.position = "none") +
         labs(y = "Biomass flow (tons/km²*year)", x = x_exp) +
         ggtitle("Terrestrial") +
         theme_bw()
# freshwater
plot2 <- ggplot(data = freshwater, aes(x = (biomass_prey *
         abundance_predator), y = biomass_flow)) +
         geom_point(color = "sandybrown") +
         theme(legend.position = "none") +
         labs(y = "", x = x_exp) +
         ggtitle("Freshwater") +
         theme_bw()
# marine
plot3 <- ggplot(data = marine, aes(x = (biomass_prey *
         abundance_predator), y = biomass_flow)) +
         geom_point(color = "deepskyblue3") +
         theme(legend.position = "none", axis.title.y = element_blank()) +
         labs(y = "", x = x_exp) +
         ggtitle("Marine") +
         theme_bw()
# log-terrestrial
plot4 <- ggplot(data = terrestrial, aes(x = log(biomass_prey) +
         log(abundance_predator), y = log(biomass_flow))) +
         geom_point(color = "olivedrab") +
         theme(legend.position = "none") +
         labs(y = "log(Biomass flow (tons/km²*year))", x = x_exp_log) +
         theme_bw()
# log-freshwater
plot5 <- ggplot(data = freshwater, aes(x = log(biomass_prey) +
         log(abundance_predator), y = log(biomass_flow))) +
         geom_point(color = "sandybrown") +
         theme(legend.position = "none", axis.title.y = element_blank()) +
         labs(y = "", x = x_exp_log) +
         theme_bw()
# log-marine
plot6 <- ggplot(data = marine, aes(x = log(biomass_prey) +
         log(abundance_predator), y = log(biomass_flow))) +
         geom_point(color = "deepskyblue3") +
         theme(legend.position = "none", axis.title.y = element_blank()) +
         labs(y = "", x = x_exp_log) +
         theme_bw()

# Arrange plots
plots <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2, ncol=3)
ggsave("figures/prelim_flow.png", plot = plots, dpi = "retina")

# randomForest model
# Check how much potential variance there is to explain
## Dataset for randomForest
RF_datasetsub <- dataset[, c("biomass_flow", "biomass_prey", "bodymass_mean_prey",
                  "metabolism_prey", "biomass_predator", "bodymass_mean_predator",
                  "abundance_predator", "metabolism_predator", "trophic_guild",
                  "habitat_type", "water_temperature", "air_temperature")]

m1 <- randomForest::randomForest(
  formula = biomass_flow ~ .,
  data    = RF_datasetsub,
  ntree = 500,
  mtry = 2
)

plot(m1)
rf_varImp <- randomForest::varImpPlot(m1)
