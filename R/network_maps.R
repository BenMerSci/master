# Libraries needed
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")

# Load the environment dataframe
coords_habitat <- readRDS("data/intermediate/enviro_traits.RDS") |>
			dplyr::select(c("habitat_type","lon","lat")) |>
			dplyr::group_by(habitat_type)

# Mapping

world <- map_data("world")
#world <- st_as_sf(world, coords = c("long","lat"))
ggplot() + geom_map(data = world, map = world, aes(x = long, y = lat, map_id=region),
                  fill = "gray") +
	   geom_point(data=coords_habitat, aes(x=lon, y=lat, color=habitat_type)) +
	   scale_color_manual(values= c("marine" = "deepskyblue3", "freshwater" = "sandybrown", "terrestrial" = "olivedrab")) +
	   coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-90, 90)) +
	   labs(title="Global distribution of the 64 Ecopath networks", x="Longitude", y="Latitude") +
	   labs(color = "Habitat type") + 
	   scale_x_continuous(breaks = seq(-200, 200, 50)) +
	   scale_y_continuous(breaks = seq(-90, 90, 30)) +
    	   theme_bw()

 
# Map with NaturalEarth
#world <- ne_countries(scale = "medium", returnclass = "sf")
#ggplot(data = world) +
#	geom_sf(color = "lightgray", fill = "lightgray") +
#		coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
#	geom_point(data = coords_habitat, aes(x = lon, y = lat, color = habitat_type)) +
#	scale_color_manual(values= c("marine" = "deepskyblue3", "freshwater" = "sandybrown", "terrestrial" = "olivedrab")) +
#	ggtitle("Global distribution of the 64 Ecopath networks") +
#	xlab("Longitude") + ylab("Latitude") +
#	labs(color = "Habitat type") #+
#	theme_classic()
#
#
#asp <- tmaptools::get_asp_ratio(world) # returns 2.070007
#height <- 5

ggsave("../manuscripts/ms_trophic_fluxes/figures/network_map.png", width = 9, height = 5, dpi = "retina")
ggsave("thesis/figs/chapitre1/network_map.pdf", width = 9, height = 5, dpi = "retina")

