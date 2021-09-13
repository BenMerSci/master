# Load the ecopath metadata file
ecopath_metadata <- readRDS("data/ecopath_metadata.RDS")

# Load ecopath_models to get the habitat type
load("data_retrieval/ecopath/Data_ncomms12573/Ecopath_models.Rdata")
	
# Merge them
enviro_df <- merge(ecopath_metadata, Ecopath_models, by.x = "model_name", by.y = "Model name", all.y = FALSE) |>
	unique() |>
	dplyr::rename(habitat_type = "Habitat type")

##### Coordinates section #####
# Convert the BOX() models to lat/lon
df_coords <- enviro_df$geographic_extent
df_coords <- gsub("BOX\\(", "", df_coords)
df_coords <- gsub("\\)", "", df_coords)
df_coords <- gsub(" ", ",", df_coords)

# Transform into a dataframe
df_coords <- strsplit(df_coords, ",") |>
 	  lapply(function(x) data.frame(t(x)))

# Get the model name associated with the bounding box coordinates from Ecopath and keep only the desired columns
df_coords <- as.data.frame(data.table::rbindlist(df_coords, fill = TRUE)) |>
	  cbind(enviro_df$model_name) |>
	  dplyr::select(c("enviro_df$model_name","X1","X3","X2","X4")) |>
	  dplyr::rename(model_name = "enviro_df$model_name")

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

enviro_df <- merge(enviro_df, df_coords, by.x = "model_name", by.y = "model_name", all.x = TRUE) |>
		dplyr::select(c("model_name","model_year","ecosystem_type","currency_units","habitat_type",
		"lon","lat"))

# Manually adding the coordinates of the Arctic networks
enviro_df[which(enviro_df$model_name == "Arctic islands, Alert"), c("lon","lat")] <- c(-62.333333, 82.5)
enviro_df[which(enviro_df$model_name == "Arctic islands, Erkuta"), c("lon","lat")] <- c(69.100000, 68.200000)
enviro_df[which(enviro_df$model_name == "Arctic islands, Herschel"), c("lon","lat")] <- c(-138.916667, 69.583333)
enviro_df[which(enviro_df$model_name == "Arctic islands, Nenetsky"), c("lon","lat")] <- c(53.300000, 68.333333)
enviro_df[which(enviro_df$model_name == "Arctic islands, Svalbard"), c("lon","lat")] <- c(16.253873, 78.180778)
enviro_df[which(enviro_df$model_name == "Arctic islands, Zackenberg"), c("lon","lat")] <- c(-21.000000, 74.500000)

# Write the dataframe as .RDS file
saveRDS(enviro_df, "data/enviro_df.RDS")

