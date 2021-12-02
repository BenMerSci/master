# Libraries
library(ggplot2)

# Load the data
dataset <- readRDS("data/clean/dataset.RDS")

# Remove NAs
dataset <- dplyr::select(dataset, -c("speed_prey","speed_pred"))
dataset <- na.omit(dataset)
dataset <- dataset[which(dataset$energy_flow < 50),]
# Check interactions where biomass prey is lower than biomass flux
bigger_flux <- dataset[which(dataset$biomass_prey < dataset$energy_flow),]


terrestrial <- dataset[which(dataset$habitat_type == "terrestrial"),]
freshwater <- dataset[which(dataset$habitat_type == "freshwater"),]
marine <- dataset[which(dataset$habitat_type == "marine"),]

##########################################################################
# Plots different habitat type
ggplot(data = dataset, aes(x = ppmr, y = energy_flow, color = habitat_type)) + 
scale_color_manual(values= c("marine" = "deepskyblue3", "freshwater" = "sandybrown", "terrestrial" = "olivedrab")) +
geom_point()
# Adjust plot frames
# terrestrial
plot1 <- ggplot(data = terrestrial, aes(x = ppmr, y = energy_flow)) + 
geom_point(color = "olivedrab") +
theme(legend.position = "none") +
scale_y_continuous("Flux de biomasse (t/km²/année)") +
ggtitle("Terrestre") +
theme_bw()
# freshwater
plot2 <- ggplot(data = freshwater, aes(x = ppmr, y = energy_flow)) + 
geom_point(color = "sandybrown") +
theme(legend.position = "none", axis.title.y=element_blank()) +
ggtitle("Aquatique") +
theme_bw()
# marine
plot3 <- ggplot(data = dataset, aes(x = ppmr, y = energy_flow)) + 
geom_point(color = "deepskyblue3") +
theme(legend.position = "none", axis.title.y=element_blank()) +
ggtitle("Marin") +
theme_bw()
# log-terrestrial
plot4 <- ggplot(data = terrestrial, aes(x = log(ppmr), y = log(energy_flow))) + 
geom_point(color = "olivedrab") +
theme(legend.position = "none") +
scale_y_continuous("log(Flux de biomasse (t/km²/année))") +
theme_bw()
# log-freshwater
plot5 <- ggplot(data = freshwater, aes(x = log(ppmr), y = log(energy_flow))) + 
geom_point(color = "sandybrown") +
theme(legend.position = "none", axis.title.y=element_blank()) +
theme_bw()
# log-marine
plot6 <- ggplot(data = dataset, aes(x = log(ppmr), y = log(energy_flow))) + 
geom_point(color = "deepskyblue3") +
theme(legend.position = "none", axis.title.y=element_blank()) +
theme_bw()

# Arrange plots
plots <- gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2, ncol=3)

# Save
ggsave("../master_talk/images/flux_ppmr.png",plot = plots, width = 9, height = 5, dpi = "retina")
ggsave("../manuscripts/ms_trophic_fluxes/figures/flux_ppmr.png", width = 9, height = 5, dpi = "retina")
