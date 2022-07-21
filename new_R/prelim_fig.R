# Libraries
library(ggplot2)

# Load the data
dataset <- readRDS("data/clean/new_dataset.RDS")

# Get a mean or median mass
dataset$mean_bodymass_prey <- rowMeans(dataset[,c("bodymass_min_prey","bodymass_max_prey")])
dataset$mean_bodymass_predator <- rowMeans(dataset[,c("bodymass_min_predator","bodymass_max_predator")])
# Check interactions where biomass prey is lower than biomass flux
#bigger_flux <- dataset[which(dataset$pred_flow > dataset$biomass_prey),]

terrestrial <- dataset[which(dataset$habitat_type == "terrestrial"), ]
freshwater <- dataset[which(dataset$habitat_type == "freshwater"), ]
marine <- dataset[which(dataset$habitat_type == "marine"), ]

##########################################################################
# Plots different habitat type

# Create axis label
x_exp <- expression(N[i] * N[j] ~ M[i])
x_exp_log <- expression(log(N[i] * N[j] ~ M[i]))

# terrestrial
plot1 <- ggplot(data = terrestrial, aes(x = ((biomass_prey / mean_bodymass_prey) *
         (biomass_predator / mean_bodymass_predator) * mean_bodymass_prey), y = pred_flow)) +
         geom_point(color = "olivedrab") +
         theme(legend.position = "none") +
         labs(y = "Flux biomasse (g/m²/année)", x = x_exp) +
         ggtitle("Terrestre") +
         theme_bw()
# freshwater
plot2 <- ggplot(data = freshwater, aes(x = ((biomass_prey / mean_bodymass_prey) *
         (biomass_predator / mean_bodymass_predator) * mean_bodymass_prey), y = pred_flow)) +
         geom_point(color = "sandybrown") +
         theme(legend.position = "none", axis.title.y = element_blank()) +
         labs(y = "", x = x_exp) +
         ggtitle("Aquatique") +
         theme_bw()
# marine
plot3 <- ggplot(data = dataset, aes(x = ((biomass_prey / mean_bodymass_prey) *
         (biomass_predator / mean_bodymass_predator) * mean_bodymass_prey), y = pred_flow)) +
         geom_point(color = "deepskyblue3") +
         theme(legend.position = "none", axis.title.y = element_blank()) +
         labs(y = "", x = x_exp) +
         ggtitle("Marin") +
         theme_bw()
# log-terrestrial
plot4 <- ggplot(data = terrestrial, aes(x = log(((biomass_prey / mean_bodymass_prey) *
         (biomass_predator / mean_bodymass_predator)) * mean_bodymass_prey), y = log(pred_flow))) +
         geom_point(color = "olivedrab") +
         theme(legend.position = "none") +
         labs(y = "log(Flux biomasse (g/m²/année))", x = x_exp_log) +
         theme_bw()
# log-freshwater
plot5 <- ggplot(data = freshwater, aes(x = log(((biomass_prey / mean_bodymass_prey) *
         (biomass_predator / mean_bodymass_predator)) * mean_bodymass_prey), y = log(pred_flow))) +
         geom_point(color = "sandybrown") +
         theme(legend.position = "none", axis.title.y = element_blank()) +
         labs(y = "", x = x_exp_log) +
         theme_bw()
# log-marine
plot6 <- ggplot(data = dataset, aes(x = log(((biomass_prey / mean_bodymass_prey) *
         (biomass_predator / mean_bodymass_predator)) * mean_bodymass_prey), y = log(pred_flow))) +
         geom_point(color = "deepskyblue3") +
         theme(legend.position = "none", axis.title.y = element_blank()) +
         labs(y = "", x = x_exp_log) +
         theme_bw()

# Arrange plots
plots <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2, ncol=3)
 
# Save
ggsave("../master_talk/images/flux_ppmr.png", plot = plots,
 width = 8, height = 8, dpi = "retina")
ggsave("../manuscripts/ms_trophic_fluxes/figures/flux_ppmr.png", plot = plots,
 width = 9, height = 5, dpi = "retina")
