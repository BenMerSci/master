# Libraries needed
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("rnaturalearthhires")

# Load the environment dataframe
coords_habitat <- readRDS("data/intermediate/enviro_traits.RDS") |>
			dplyr::select(c("habitat_type","lon","lat")) |>
			dplyr::group_by(habitat_type)


# Mapping
par(mar=c(0.5,0,0,0))
world <- map_data("world")

ggplot() + geom_map(data = world, map = world, aes(x = long, y = lat, map_id=region),
                  fill = "gray") +
	   geom_point(data=coords_habitat, aes(x=lon, y=lat, color=habitat_type)) +
	   scale_color_manual(values= c("marine" = "deepskyblue3", "freshwater" = "sandybrown", "terrestrial" = "olivedrab")) +
	   coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-90, 90)) +
	   labs(title="Global distribution of the Ecopath networks used", x="Longitude", y="Latitude") +
	   labs(color = "Habitat type") +
	   scale_x_continuous(breaks = seq(-200, 200, 50)) +
	   scale_y_continuous(breaks = seq(-90, 90, 30)) +
    	   theme()

# Save it
ggsave("manuscript/figures/network_map.png", width = 9, height = 5, dpi = "screen")


