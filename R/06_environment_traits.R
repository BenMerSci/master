# Load the ecopath metadata file
ecopath_metadata <- readRDS("data/intermediate/ecopath_metadata.RDS")

# Load Ecopath_models_modif to get the habitat type
load("data/raw/ecopath/data/Ecopath_models_modif.Rdata")
	
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

library(magrittr)
test <- read.delim("data/legagneux/BPQ_all.txt") %>%
		dplyr::select(c("Site","Mean.temperature", "Lat")) %>%
		dplyr::group_by(Site) %>%
		dplyr::summarize(mean_temp = mean(Mean.temperature), latitude = mean(Lat))
		
# Manually adding the coordinates of the Arctic networks
enviro_df[which(enviro_df$model_name == "Arctic islands, Alert"), c("lon","lat")] <- c(-62.3, 82)
enviro_df[which(enviro_df$model_name == "Arctic islands, Bylot"), c("lon","lat")] <- c(-80.0, 73)
enviro_df[which(enviro_df$model_name == "Arctic islands, Erkuta"), c("lon","lat")] <- c(69.1, 68.2)
enviro_df[which(enviro_df$model_name == "Arctic islands, Herschel"), c("lon","lat")] <- c(-138.9, 69.6)
enviro_df[which(enviro_df$model_name == "Arctic islands, Nenetsky"), c("lon","lat")] <- c(53.3, 68.8)
enviro_df[which(enviro_df$model_name == "Arctic islands, Svalbard"), c("lon","lat")] <- c(16.3, 78.9)
enviro_df[which(enviro_df$model_name == "Arctic islands, Zackenberg"), c("lon","lat")] <- c(-21.0, 74.5)

# Add ecosystem type to the one missing (arctic ones)
enviro_df[which(is.na(enviro_df$ecosystem_type)),"ecosystem_type"] <- "terrestrial"

#Transform the years into numeric
enviro_df$model_year <- as.numeric(enviro_df$model_year)

# Getting energy flux units for each model
enviro_df[which(enviro_df$model_name == "Alaka Prince William Sound OM"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Alto Golfo De California"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Bali Strait"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Bay Of Somme"), "flux_units"] <- "g/m2/yr"
enviro_df[which(enviro_df$model_name == "Campeche Bank, Golf of Mexico"), "flux_units"] <- "g/m2/yr, dry"
enviro_df[which(enviro_df$model_name == "Cape Verde"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Caribbean"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Central Atlantic 50s"), "flux_units"] <- "kg/km2/yr"
enviro_df[which(enviro_df$model_name == "Central Chile 1992"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Central Pacific, sharks"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Chesapeake Present"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Eastern Tropical Pacific"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Etang de Thau, France"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Gulf of Salamanca, Upwelling"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "High Barents Sea AllJuvs1990"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "High Barents Sea Final 1990"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Huizache Caimanero, Mexico"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Iceland Fisheries"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Jalisco y Colima"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Lagoon of Venice"), "flux_units"] <- "kcal/m2/month"
enviro_df[which(enviro_df$model_name == "Lake Aydat, France"), "flux_units"] <- "g/m2/yr"
enviro_df[which(enviro_df$model_name == "Lake Chad, Africa"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Lake George, Uganda"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Lake Kariba, Africa"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Lake Kinneret, Israel"), "flux_units"] <- "g/m2/yr"
enviro_df[which(enviro_df$model_name == "Lake Malawi 2, Africa"), "flux_units"] <- "g/m2/yr"
enviro_df[which(enviro_df$model_name == "Lake Malawi, Africa"), "flux_units"] <- "g/m2/yr"
enviro_df[which(enviro_df$model_name == "Lake Tanganyka, Africa, 1975"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Lake Tanganyka, Africa, 1981"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Lake Turkana, Kenya, 1973"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Lake Victoria, Africa, 1971"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Lake Victoria, Africa, 1985"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "LakeTurkana, Kenya, 1987"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Low Barents Sea 1995"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Low Barents Sea Juvs 1995"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Maputo Bay, Mozambique"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Newfoundland Grand Banks 1900"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Newfoundland Grand Banks mid-1980s"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Newfounland  Grand Banks mid-1990s"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "North Atlantic 1950s"), "flux_units"] <- "kg/km2/yr"
enviro_df[which(enviro_df$model_name == "North Atlantic 1990s"), "flux_units"] <- "kg/km2/yr"
enviro_df[which(enviro_df$model_name == "Northwest Africa"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Orbetello Lagoon"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Peruvian upwelling system 1950s"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Peruvian upwelling system 1960s"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Ria Formosa"), "flux_units"] <- "g/m2/yr, dry"
enviro_df[which(enviro_df$model_name == "Seine Estuary"), "flux_units"] <- "g/m2/yr"
enviro_df[which(enviro_df$model_name == "Strait Of Georgia"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Tamiahua Lagoon, Golf of Mexico"), "flux_units"] <- "g/m2/yr"
enviro_df[which(enviro_df$model_name == "Tampa Bay"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Tampamachoco Lagoon, Mexico"), "flux_units"] <- "g/m2/yr, dry"
enviro_df[which(enviro_df$model_name == "Upper Parana River Floodplain"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "West Coast of Greenland"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "West Coast of Vancouver Island"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "West Greenland, Shrimp Pound"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Western Bering Sea"), "flux_units"] <- "t/km2/yr"
enviro_df[which(enviro_df$model_name == "Western Gulf of Mexico"), "flux_units"] <- "g/m2/yr, dry"
enviro_df[which(enviro_df$model_name == "Yucatan shelf, Gulf of Mexico"), "flux_units"] <- "g/m2/yr, dry"
enviro_df[which(enviro_df$ecosystem_type == "terrestrial"), "flux_units"] <- "kg/km2/yr, dry"

# Getting seasonality for each model
#enviro_df[which(enviro_df$model_name == "Alaka Prince William Sound OM"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Alto Golfo De California"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Bali Strait"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Bay Of Somme"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Campeche Bank, Golf of Mexico"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Cape Verde"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Caribbean"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Central Atlantic 50s"), "seasonality"] <- TRUE/FALSE
#enviro_df[which(enviro_df$model_name == "Central Chile 1992"), "seasonality"] <- FALSE
#enviro_df[which(enviro_df$model_name == "Central Pacific, sharks"), "seasonality"] <- FALSE
#enviro_df[which(enviro_df$model_name == "Chesapeake Present"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Eastern Tropical Pacific"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Etang de Thau, France"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Gulf of Salamanca, Upwelling"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "High Barents Sea AllJuvs1990"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "High Barents Sea Final 1990"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Huizache Caimanero, Mexico"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Iceland Fisheries"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Jalisco y Colima"), "seasonality"] <- TRUE/FALSE
#enviro_df[which(enviro_df$model_name == "Lagoon of Venice"), "seasonality"] <- FALSE # Model only based on summer data
#enviro_df[which(enviro_df$model_name == "Lake Aydat, France"), "seasonality"] <- TRUE/FALSE
#enviro_df[which(enviro_df$model_name == "Lake Chad, Africa"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Lake George, Uganda"), "seasonality"] <- FALSE
#enviro_df[which(enviro_df$model_name == "Lake Kariba, Africa"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Lake Kinneret, Israel"), "seasonality"] <- TRUE
#enviro_df[which(enviro_df$model_name == "Lake Malawi 2, Africa"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Lake Malawi, Africa"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Lake Tanganyka, Africa, 1975"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Lake Tanganyka, Africa, 1981"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Lake Turkana, Kenya, 1973"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Lake Victoria, Africa, 1971"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Lake Victoria, Africa, 1985"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "LakeTurkana, Kenya, 1987"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Low Barents Sea 1995"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Low Barents Sea Juvs 1995"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Maputo Bay, Mozambique"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Newfoundland Grand Banks 1900"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Newfoundland Grand Banks mid-1980s"), "seasonality"] <-
#enviro_df[which(enviro_df$model_name == "Newfounland  Grand Banks mid-1990s"), "seasonality"] <-
#enviro_df[which(enviro_df$model_name == "North Atlantic 1950s"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "North Atlantic 1990s"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Northwest Africa"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Orbetello Lagoon"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Peruvian upwelling system 1950s"), "seasonality"] <-
#enviro_df[which(enviro_df$model_name == "Peruvian upwelling system 1960s"), "seasonality"] <-
#enviro_df[which(enviro_df$model_name == "Ria Formosa"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Seine Estuary"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Strait Of Georgia"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Tamiahua Lagoon, Golf of Mexico"), "seasonality"] <-
#enviro_df[which(enviro_df$model_name == "Tampa Bay"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Tampamachoco Lagoon, Mexico"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Upper Parana River Floodplain"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "West Coast of Greenland"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "West Coast of Vancouver Island"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "West Greenland, Shrimp Pound"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Western Bering Sea"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Western Gulf of Mexico"), "seasonality"] <- 
#enviro_df[which(enviro_df$model_name == "Yucatan shelf, Gulf of Mexico"), "seasonality"] <- 
#enviro_df[which(enviro_df$ecosystem_type == "terrestrial"), "seasonality"] <- TRUE

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









# WolrdClim
#get_temp <- function(enviro_df){
#
#options(timeout = 1800)
#needed_years <- sort(unique(enviro_df$model_year))
#
#df_1900_1969 <- enviro_df[which(enviro_df$model_year <= 1969),]
#
#test <- needed_years[which(needed_years <=1969)]
#needed_years[which(needed_years >= 1970) && which(needed_years <=1979)]
#for(i in needed_years)){
#  
#  if(1960 <= needed_year || needed_year >= 1969) year_gap <- "1960-1969"
#  if(1970 <= needed_year || needed_year >= 1979) year_gap <- "1970-1979"
#  if(1980 <= needed_year || needed_year >= 1989) year_gap <- "1980-1989"
#  if(1990 <= needed_year || needed_year >= 1999) year_gap <- "1990-1999"
#  if(2000 <= needed_year || needed_year >= 2010) year_gap <- "2000-2010"
#
#  # Download the data for the selected years
#  download.file(paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_",year_gap,".zip"), paste0("data/worldclim/zipped/tmin",year_gap,".zip")) #tmin
#  download.file(paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_",year_gap,".zip"), paste0("data/worldclim/zipped/tmax",year_gap,".zip")) #tmax
#  download.file(paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_",year_gap,".zip"), paste0("data/worldclim/zipped/prec",year_gap,".zip")) #prec
#  # Unzip the files
#  unzip("data/worldclim/zipped/tmin1960-1969.zip", exdir = "data/worldclim/unzipped")
#  unzip("data/worldclim/zipped/tmax1960-1969.zip", exdir = "data/worldclim/unzipped")
#  unzip("data/worldclim/zipped/prec1960-1969.zip", exdir = "data/worldclim/unzipped")
#
#  grep("1961",unzip("data/worldclim/zipped/tmin1960-1969.zip", list = TRUE)$Name)
#
#
#
#  temp <- enviro_df[which(enviro_df$model_year == i),]
#
#  bioclim <- dismo::biovars(prec = clim_data[["prec"]], tmin = clim_data[["tmin"]], tmax = clim_data[["tmax"]])
#
#
#  }
#
#  return(enviro_df)
#}


#bioclim <- lapply(years, function(x) {
#  clim_data <- lapply(c("tmin", "tmax", "prec"), function(y) {
#    tmp <- raster::stack(paste0("predictors/climate/raw/",list.files("predictors/climate/raw/", paste0(y,"_",x))))
#    tmp <- raster::crop(tmp, bbox)
#    names(tmp) <- paste0(y,"_",x,"_",1:12)
#    return(tmp)
#  }) 
#  names(clim_data) <- c("tmin", "tmax", "prec")
#
#  bioclim <- dismo::biovars(prec = clim_data[["prec"]], tmin = clim_data[["tmin"]], tmax = clim_data[["tmax"]])
#
#  return(bioclim)
#})
#
#names(bioclim) <- paste0(years)
#
#for(i in paste(years)) {
#  names(bioclim[[i]]) <- paste0(names(bioclim[[i]]), "_", i)
#}
#
## Choisir variables intéressantes
#bioclim <- lapply(bioclim, function(x) {
#  x <- x[[1]]
#})
#
## Mettre bioclim ensemble
#bioclim <- raster::stack(bioclim)