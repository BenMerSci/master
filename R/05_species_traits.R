# Load the interaction table to make a unique species list
resolved_inter_table <- readRDS("data/intermediate/resolved_inter_table.RDS")

species_list <- data.frame(unique(c(resolved_inter_table$species_from, resolved_inter_table$species_to))) |>
		dplyr::rename(scientific_name = "unique.c.resolved_inter_table.species_from..resolved_inter_table.species_to..")

# Get the GBIF IDs for each species
species_list$gbif_id <- apply(species_list, 1, function(x) taxize::get_gbifid(x, ask = TRUE, messages = TRUE)[1])

# Get hierarchy for each species from their gbif_id
metabolic_class <- lapply(species_list[,"gbif_id"], function(x) taxize::classification(x, db = "gbif")) |>
		lapply(function(x) data.frame(x[[1]])) |>
		lapply(function(x) x[which(x$rank == "class"),"name"]) |>
		lapply(function(x) ifelse(is.null(x), NA, x))

metabolic_class <- do.call("rbind", metabolic_class)

# Bind the classes to the species list
species_list <- cbind(species_list, metabolic_class)

# Uniformize it
species_list[is.na(species_list$metabolic_class),"metabolic_class"] <- "Plankton"

# Separate the different type of organisms
fish_alike <- species_list[which(species_list$metabolic_class %in% c("Actinopterygii", "Elasmobranchii", "Sarcopterygii")),]
mammalia <- species_list[which(species_list$metabolic_class == "Mammalia"),]
aves <- species_list[which(species_list$metabolic_class == "Aves"),]
sealife <- species_list[which(species_list$metabolic_class %in% c("Malacostraca", "Bivalvia", "Hexanauplia", "Scyphozoa", "Cephalopoda")),]
insecta <- species_list[which(species_list$metabolic_class == "Insecta"),]
plankton <- species_list[which(species_list$metabolic_class == "Plankton"),]


##### Fish #####
# BodyMass are in grams (g)
# Validate the fish names
validated_fish <- rfishbase::validate_names(fish_alike$scientific_name)
# Get their lenght and weight
fish_data <- as.data.frame(rfishbase::species(fish_alike$scientific_name, fields=c("Species", "Length", "Weight")))
fish_data <- merge(fish_alike, fish_data, by.x = "scientific_name", by.y = "Species", all.x = TRUE) |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "Weight")) |>
	dplyr::rename(bodymass = "Weight")
#test <- na.omit(as.data.frame(rfishbase::length_weight(fish_alike$scientific_name, fields=c("Species","a","aTL","b","CoeffDetermination"))))
#miss_weight_fish <- data_fish[which(is.na(data_fish$Weight)),]$Species
#miss_weight_fish %in% test$Species

##### Sealife #####
# BodyMass are in grams (g)
# Validate the malacostraca names
validated_sealife <- rfishbase::validate_names(sealife$scientific_name, server = "sealifebase")
# Get their lenght and weight
sealife_data <- as.data.frame(rfishbase::species(sealife$scientific_name, fields=c("Species", "Length", "Weight"), server = "sealifebase"))
sealife_data <- merge(sealife, sealife_data, by.x = "scientific_name", by.y = "Species", all.x = TRUE) |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "Weight")) |>
	dplyr::rename(bodymass = "Weight")
sealife_data$bodymass <- as.numeric(sealife_data$bodymass)

##### Mammals and birds #####
# Data for mammals and birds are taken from the EltonTraits database 1.0
# BodyMass are in grams (g)
birds_traits <- read.csv("data/raw/eltontraits/BirdFuncDat.txt", sep = "\t")
mamm_traits <- read.csv("data/raw/eltontraits/MamFuncDat.txt", sep = "\t")

aves_data <- merge(aves, birds_traits, by.x = "scientific_name", by.y = "Scientific", all.x = TRUE) |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "BodyMass.Value")) |>
	dplyr::rename(bodymass = "BodyMass.Value")

mammalia_data <- merge(mammalia, mamm_traits, by.x = "scientific_name", by.y = "Scientific", all.x = TRUE) |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "BodyMass.Value")) |>
	dplyr::rename(bodymass = "BodyMass.Value")

mammalia_data[which(mammalia_data$scientific_name == "Canis lupus arctos"), "bodymass"] <- mamm_traits[which(mamm_traits$Scientific == "Canis lupus"), "BodyMass.Value"]
mammalia_data[which(mammalia_data$scientific_name == "Physeter macrocephalus"), "bodymass"] <- mamm_traits[which(mamm_traits$Scientific == "Physeter catodon"), "BodyMass.Value"]
mammalia_data[which(mammalia_data$scientific_name == "Ursus horribilis"), "bodymass"] <- mamm_traits[which(mamm_traits$Scientific == "Ursus arctos"), "BodyMass.Value"]

##### Insecta #####
insecta_data <- insecta
insecta_data[,"bodymass"] <- NA

##### Plankton ##### 
# Data from Yodzis 1998
plankton_data <- plankton
plankton_data["bodymass"] <- NA
plankton_data[which(plankton_data$scientific_name == "Phytoplankton"), "bodymass"] <- 0.0001
plankton_data[which(plankton_data$scientific_name == "Mesozooplankton"), "bodymass"] <- 0.01
plankton_data[which(plankton_data$scientific_name == "Zooplankton"), "bodymass"] <- 0.01
plankton_data[which(plankton_data$scientific_name == "Microzooplankton"), "bodymass"] <- 0.0001
plankton_data[which(plankton_data$scientific_name == "Small Zooplankton"), "bodymass"] <- 0.0001
plankton_data[which(plankton_data$scientific_name == "Large Zooplankton"), "bodymass"] <- 1
plankton_data[which(plankton_data$scientific_name == "Macrozooplankton"), "bodymass"] <- 1
plankton_data[which(plankton_data$scientific_name == "Bakterioplankton"), "bodymass"] <- 0.00000001


# Merge everything back into species_list
# Have 147 bodymass of 240 species
species_traits <- rbind(fish_data, sealife_data, aves_data, mammalia_data, insecta_data, plankton_data)

# Save the traits data_frame
saveRDS(species_traits, "data/intermediate/species_traits.RDS")
