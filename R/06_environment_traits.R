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

# Add ecosystem type to the one missing (arctic ones)
enviro_df[which(is.na(enviro_df$ecosystem_type)),"ecosystem_type"] <- "terrestrial"

#Transform the years into numeric
enviro_df$model_year <- as.numeric(enviro_df$model_year)

# Get temperature data
get_temp <- function(enviro_df){

options(timeout = 1800)
needed_years <- sort(unique(enviro_df$model_year))

df_1900_1969 <- enviro_df[which(enviro_df$model_year <= 1969),]



test <- needed_years[which(needed_years <=1969)]
needed_years[which(needed_years >= 1970) && which(needed_years <=1979)]
for(i in needed_years)){
  
  if(1960 <= needed_year || needed_year >= 1969) year_gap <- "1960-1969"
  if(1970 <= needed_year || needed_year >= 1979) year_gap <- "1970-1979"
  if(1980 <= needed_year || needed_year >= 1989) year_gap <- "1980-1989"
  if(1990 <= needed_year || needed_year >= 1999) year_gap <- "1990-1999"
  if(2000 <= needed_year || needed_year >= 2010) year_gap <- "2000-2010"

  # Download the data for the selected years
  download.file(paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmin_",year_gap,".zip"), paste0("data/worldclim/zipped/tmin",year_gap,".zip")) #tmin
  download.file(paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_tmax_",year_gap,".zip"), paste0("data/worldclim/zipped/tmax",year_gap,".zip")) #tmax
  download.file(paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/wc2.1_2.5m_prec_",year_gap,".zip"), paste0("data/worldclim/zipped/prec",year_gap,".zip")) #prec
  # Unzip the files
  unzip("data/worldclim/zipped/tmin1960-1969.zip", exdir = "data/worldclim/unzipped")
  unzip("data/worldclim/zipped/tmax1960-1969.zip", exdir = "data/worldclim/unzipped")
  unzip("data/worldclim/zipped/prec1960-1969.zip", exdir = "data/worldclim/unzipped")

  grep("1961",unzip("data/worldclim/zipped/tmin1960-1969.zip", list = TRUE)$Name)



  temp <- enviro_df[which(enviro_df$model_year == i),]

  bioclim <- dismo::biovars(prec = clim_data[["prec"]], tmin = clim_data[["tmin"]], tmax = clim_data[["tmax"]])


  }

  return(enviro_df)
}



# Berkeley data
library(ncdf4)
ncfile <- nc_open("data/Land_and_Ocean_Alternate_LatLong1.nc")
    ## create variables for things needed to use data
date <- ncvar_get(ncfile, "date_number")
arr.anom <-ncvar_get(ncfile, "temperature")
arr.clim <- ncvar_get(ncfile, "climatology")

lat <- ncvar_get(ncfile, "latitude")
long <- ncvar_get(ncfile, "longitude")
date <- ncvar_get(ncfile, "date_number")
arr.anom <-ncvar_get(ncfile, "temperature")
arr.clim <- ncvar_get(ncfile, "climatology")


nc_close(ncfile)






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

