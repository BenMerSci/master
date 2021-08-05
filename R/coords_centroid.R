# Load the data
inter_table <- readRDS("data/inter_table.RDS")

df_coords <- inter_table$model.geographic_extent
df_coords <- gsub("BOX\\(", "", df_coords)
df_coords <- gsub("\\)", "", df_coords)
df_coords <- gsub(" ", ",", df_coords)

# Transform into a dataframe
df_coords <- strsplit(df_coords, ",") |>
 	  lapply(function(x) data.frame(t(x)))

# Get the model name associated with the bounding box coordinates from Ecopath and keep only the desired columns
df_coords <- as.data.frame(data.table::rbindlist(df_coords, fill = TRUE)) |>
	  cbind(inter_table$model_name) |>
	  dplyr::select(c("inter_table$model_name","X1","X3","X2","X4")) |>
	  dplyr::rename(model_name = "inter_table$model_name")

df_coords[,c(2:5)] <- apply(df_coords[,c(2:5)], 2, function(x) as.numeric(x))

# Removes the rows with NA and 
df_coords <- unique(na.omit(df_coords))

# Get the centroid of the bounding box coordinates
# First it gets the extent of based on the bbox coordinates, then cast it as a polygon and then gets its centroid
centroids <- t(apply(df_coords, 1, function(x) raster::coordinates(as(raster::extent(as.numeric(x[c("X1","X3","X2","X4")])), "SpatialPolygons"))))

# Bind the coordinates back to the dataframe
df_coords <- cbind(df_coords, centroids) |>
		dplyr::select(c("model_name", "1", "2")) |>
		dplyr::rename(lon = "1", lat = "2")

inter_table <- merge(inter_table, df_coords, by.x = "model_name", by.y = "model_name", x.all = TRUE) |>
		dplyr::select(c("model_name","species_from","species_to","energy_flow","habitat_type",
		"lon","lat","model.ecosystem_type","model.currency_units","model.model_year","model.depth_min",
		"model.depth_max","model.currency_units_custom","model.depth_mean","model.temperature_mean",
		"model.temperature_min","model.temperature_max"))

saveRDS(inter_table, "data/inter_table.RDS")

