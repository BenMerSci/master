# Load the ecopath metadata file
ecopath_metadata <- readRDS("data/intermediate/ecopath_metadata.RDS")

# Load Ecopath_models_modif to get the habitat type
load("data/raw/ecopath/data/Ecopath_models_modif.Rdata")

# Merge them
enviro_df <- merge(ecopath_metadata, Ecopath_models,
              by.x = "model_name", by.y = "Model name", all.y = FALSE) |>
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

# Get the model name associated with the bounding box coordinates
# from Ecopath and keep only the desired columns
df_coords <- as.data.frame(data.table::rbindlist(df_coords, fill = TRUE)) |>
             cbind(enviro_df$model_name) |>
             dplyr::select(c("enviro_df$model_name", "X1", "X3", "X2", "X4")) |>
             dplyr::rename(model_name = "enviro_df$model_name")

df_coords[, c(2:5)] <- apply(df_coords[, c(2:5)], 2, function(x) as.numeric(x))

# Removes the rows with NA and
df_coords <- unique(na.omit(df_coords))

# Get the centroid of the bounding box coordinates
# First it gets the extent of based on the bbox coordinates,
# then cast it as a polygon and then gets its centroid
centroids <- t(apply(df_coords, 1, function(x) {
	         raster::coordinates(as(raster::extent(as.numeric(x[c("X1", "X3", "X2", "X4")])),
              "SpatialPolygons"))
             }))
             
# Bind the coordinates back to the dataframe
df_coords <- cbind(df_coords, centroids) |>
		dplyr::select(c("model_name", "1", "2")) |>
		dplyr::rename(lon = "1", lat = "2")

enviro_df <- merge(enviro_df, df_coords, by.x = "model_name", by.y = "model_name", all.x = TRUE) |>
		dplyr::select(c("model_name","model_year","ecosystem_type","currency_units","habitat_type",
		"lon","lat"))
		
# Manually adding the coordinates of the Arctic networks
enviro_df[which(enviro_df$model_name == "Arctic islands, Alert"), c("lon","lat")] <- c(-62.3, 82)
enviro_df[which(enviro_df$model_name == "Arctic islands, Bylot"), c("lon","lat")] <- c(-80.0, 73)
enviro_df[which(enviro_df$model_name == "Arctic islands, Erkuta"), c("lon","lat")] <- c(69.1, 68.2)
enviro_df[which(enviro_df$model_name == "Arctic islands, Herschel"), c("lon","lat")] <- c(-138.9, 69.6)
enviro_df[which(enviro_df$model_name == "Arctic islands, Nenetsky"), c("lon","lat")] <- c(53.3, 68.8)
enviro_df[which(enviro_df$model_name == "Arctic islands, Svalbard"), c("lon","lat")] <- c(16.3, 78.9)
enviro_df[which(enviro_df$model_name == "Arctic islands, Zackenberg"), c("lon","lat")] <- c(-21.0, 74.5)

# Transform the years into numeric
enviro_df$model_year <- as.numeric(enviro_df$model_year)

#### Climate data ####
# Download BerkeleyEarth data
# Temperature with water temp where there is water (surface water temp)
if(!file.exists("data/raw/berkeleyearth/berkeley_climate_watersurf.nc")){
	download.file(url = "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_Alternate_LatLong1.nc",
	destfile = "data/raw/berkeleyearth/berkeley_climate_watersurf.nc")
}
# Temperature with air temp where there is water (surface air temp)
if(!file.exists("data/raw/berkeleyearth/berkeley_climate_airsurf.nc")){
	download.file(url = "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_LatLong1.nc",
	destfile = "data/raw/berkeleyearth/berkeley_climate_airsurf.nc")
}

# Load the desired stack
water_temp_stack <- raster::stack("data/raw/berkeleyearth/berkeley_climate_watersurf.nc", varname = "climatology")
water_anom_stack <- raster::stack("data/raw/berkeleyearth/berkeley_climate_watersurf.nc", varname = "temperature")
air_temp_stack <- raster::stack("data/raw/berkeleyearth/berkeley_climate_airsurf.nc", varname = "climatology")
air_anom_stack <- raster::stack("data/raw/berkeleyearth/berkeley_climate_airsurf.nc", varname = "temperature")

# Find a way to subset the anom_stack to get only desired years
model_date <- as.character(unique(enviro_df$model_year))
water_anom_date <- gsub(".[^.]*$", "", names(water_anom_stack))
water_anom_date <- gsub("X", "", water_anom_date)
air_anom_date <- gsub(".[^.]*$", "", names(air_anom_stack))
air_anom_date <- gsub("X", "", air_anom_date)

water_anom_stack <- raster::subset(water_anom_stack, names(water_anom_stack)[water_anom_date %in% model_date])
air_anom_stack <- raster::subset(air_anom_stack, names(air_anom_stack)[air_anom_date %in% model_date])

# Subset the desired years
water_stack_list <- list()
air_stack_list <- list()

for (i in 0:((raster::nlayers(water_anom_stack)/12)-1)) {
		temp <- water_anom_stack[[(1+12*i):(12+12*i)]]
		water_stack_list[[i+1]] <- temp
}

for (i in 0:((raster::nlayers(air_anom_stack)/12)-1)) {
		temp <- air_anom_stack[[(1+12*i):(12+12*i)]]
		air_stack_list[[i+1]] <- temp
}

# Calculate the mean over the twelve months per year
water_stack_list <- lapply(water_stack_list, function(x) raster::calc(x, mean))
water_temp_stack <- raster::calc(water_temp_stack, mean)
air_stack_list <- lapply(air_stack_list, function(x) raster::calc(x, mean))
air_temp_stack <- raster::calc(air_temp_stack, mean)

# Add the mean climatology to the mean anomalies
water_final_stack <- list()
air_final_stack <- list()

for (i in 1:length(water_stack_list)) {
	water_final_stack[[i]] <- water_stack_list[[i]] + water_temp_stack
}

for (i in 1:length(air_stack_list)) {
	air_final_stack[[i]] <- air_stack_list[[i]] + air_temp_stack
}

# Change the name of each lists
names(water_final_stack) <- sort(unique(enviro_df$model_year))
names(air_final_stack) <- sort(unique(enviro_df$model_year))

# Extract temperature per year and add it to enviro_df
for (i in 1:nrow(enviro_df)) {
	enviro_df[i,"water_temperature"] <- raster::extract(water_final_stack[[as.character(enviro_df[i,"model_year"])]], enviro_df[i,c("lon","lat")])
	enviro_df[i,"air_temperature"] <- raster::extract(air_final_stack[[as.character(enviro_df[i,"model_year"])]], enviro_df[i,c("lon","lat")])
}

saveRDS(enviro_df, "data/intermediate/enviro_traits.RDS")
