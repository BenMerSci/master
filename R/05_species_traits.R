# Load the interaction table to make a unique species list
resolved_inter_table <- readRDS("data/intermediate/resolved_inter_table.RDS")

species_list <- data.frame(unique(c(resolved_inter_table$prey, resolved_inter_table$predator))) |>
		dplyr::rename(scientific_name = "unique.c.resolved_inter_table.prey..resolved_inter_table.predator..")

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
fish <- species_list[which(species_list$metabolic_class %in% c("Actinopterygii", "Elasmobranchii", "Sarcopterygii")),]
mammalia <- species_list[which(species_list$metabolic_class == "Mammalia"),]
aves <- species_list[which(species_list$metabolic_class == "Aves"),]
sealife <- species_list[which(species_list$metabolic_class %in% c("Malacostraca", "Bivalvia", "Hexanauplia", "Scyphozoa", "Cephalopoda")),]
insecta <- species_list[which(species_list$metabolic_class == "Insecta"),]
plankton <- species_list[which(species_list$metabolic_class == "Plankton"),]


##### Fish #####
# BodyMass are in grams (g)
# Validate the fish names
validated_fish <- data.frame(cbind(fish$scientific_name, rfishbase::validate_names(fish$scientific_name))) |>
			dplyr::rename(original = "X1", validated = "X2")

fish_LW <- rfishbase::length_weight(validated_fish$validated)
fish_LW_ls <- base::split(fish_LW, f = fish_LW$Species)
fish_LW_ls <- lapply(fish_LW_ls, function(x){ x$weight <- x$a*(x$LengthMax^x$b)
			return(x)})
fish_LW_ls <- lapply(fish_LW_ls, function(x){ x$LengthMin <- as.numeric(x$LengthMin)
			x$weightMin <- x$a*(x$LengthMin^x$b)
			return(x)})
fish_LW_ls <- lapply(fish_LW_ls, function(x){ data.frame(cbind(unique(x$Species), mean(x$weightMin, na.rm = TRUE), mean(x$weight, na.rm = TRUE)))})

fish_LW_ls <- do.call("rbind", fish_LW_ls)

fish_speed <- rfishbase::speed(validated_fish$validated)
fish_speed_ls <- base::split(fish_speed, f = fish_speed$Species)

fish_speed_ls <- lapply(fish_speed_ls, function(x){ data.frame(cbind(unique(x$Species), mean(x$Speedms, na.rm = TRUE)))})
fish_speed_ls <- do.call("rbind", fish_speed_ls)
 
fish_data <- merge(fish, fish_LW_ls, by.x = "scientific_name", by.y = "X1", all.x = TRUE) |>
	dplyr::rename(bodymass = "X3", bodymass_min = "X2") |>
	merge(fish_speed_ls, by.x = "scientific_name", by.y = "X1", all.x = TRUE) |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "bodymass", "bodymass_min", "X2")) |>
	dplyr::rename(speed = "X2")


##### Sealife #####
# BodyMass are in grams (g)
# Validate the malacostraca names
validated_sealife <- data.frame(cbind(sealife$scientific_name, rfishbase::validate_names(sealife$scientific_name, server = "sealifebase"))) |>
			dplyr::rename(original = "X1", validated = "X2")
# Get their lenght and weight
sea_LW <- rfishbase::length_weight(validated_sealife$validated, server = "sealifebase")
sea_LW_ls <- base::split(sea_LW, f = sea_LW$Species)

sea_LW_ls <- lapply(sea_LW_ls, function(x){ x$weight <- x$a*(x$LengthMax^x$b)
			return(x)})
sea_LW_ls <- lapply(sea_LW_ls, function(x){ x$LengthMin <- as.numeric(x$LengthMin)
			x$weightMin <- x$a*(x$LengthMin^x$b)
			return(x)})
sea_LW_ls <- lapply(sea_LW_ls, function(x){ data.frame(cbind(unique(x$Species), mean(x$weightMin, na.rm = TRUE), mean(x$weight, na.rm = TRUE)))})

sea_LW_ls <- do.call("rbind", sea_LW_ls)

sealife_data <- merge(sealife, sea_LW_ls, by.x = "scientific_name", by.y = "X1", all.x = TRUE) |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "X2","X3")) |>
	dplyr::rename(bodymass = "X3", bodymass_min = "X2") |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "bodymass", "bodymass_min")) 

##### Mammals and birds #####
# Data for mammals and birds are taken from the EltonTraits database 1.0
# BodyMass are in grams (g)
birds_traits <- read.csv("data/raw/eltontraits/BirdFuncDat.txt", sep = "\t")
mamm_traits <- read.csv("data/raw/eltontraits/MamFuncDat.txt", sep = "\t")

aves_data <- merge(aves, birds_traits, by.x = "scientific_name", by.y = "Scientific", all.x = TRUE) |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "BodyMass.Value")) |>
	dplyr::rename(bodymass = "BodyMass.Value")

aves_data[which(aves_data$scientific_name == "Bubo scandiacus"), "bodymass"] <- birds_traits[which(birds_traits$Scientific == "Bubo scandiaca"), "BodyMass.Value"]
aves_data[which(aves_data$scientific_name == "Leucocarbo bougainvillii"), "bodymass"] <- birds_traits[which(birds_traits$Scientific == "Phalacrocorax bougainvillii"), "BodyMass.Value"]


mammalia_data <- merge(mammalia, mamm_traits, by.x = "scientific_name", by.y = "Scientific", all.x = TRUE) |>
	subset(select = c("scientific_name", "gbif_id", "metabolic_class", "BodyMass.Value")) |>
	dplyr::rename(bodymass = "BodyMass.Value")

mammalia_data[which(mammalia_data$scientific_name == "Canis lupus arctos"), "bodymass"] <- mamm_traits[which(mamm_traits$Scientific == "Canis lupus"), "BodyMass.Value"]
mammalia_data[which(mammalia_data$scientific_name == "Otaria byronia"), "bodymass"] <- mamm_traits[which(mamm_traits$Scientific == "Otaria flavescens"), "BodyMass.Value"]
mammalia_data[which(mammalia_data$scientific_name == "Physeter macrocephalus"), "bodymass"] <- mamm_traits[which(mamm_traits$Scientific == "Physeter catodon"), "BodyMass.Value"]

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
species_traits <- plyr::rbind.fill(fish_data, sealife_data, aves_data, mammalia_data, insecta_data, plankton_data)
species_traits[which(species_traits$bodymass == NaN),]
species_traits[which(species_traits$bodymass == NaN), "bodymass"] <- NA
species_traits[which(species_traits$bodymass == NaN), "bodymass_min"] <- NA
species_traits[which(species_traits$bodymass == NaN), "speed"] <- NA


# Locomotion from https://doi.org/10.1002/ecy.3114
loco <- read.csv("data/raw/locomotion_ecology/Animal_Locomotion_Database_ecy0011-sup-0001-DataS1.txt", header  = TRUE, sep = "\t") |>
	dplyr::select(c("Taxa_ID","SI_Mass","SI_Length","Trait_ID","Trait_Value","Temperature","Temp_Type","Thermy")) |>
	dplyr::filter(Taxa_ID %in% species_traits$scientific_name) |>
	(\(x)base::split(x, f = x$Taxa_ID))()

temp_loco <- loco[c(8,12,28,32)]

loco <- lapply(loco, function(x) tidyr::pivot_wider(x, names_from = "Trait_ID", values_from = "Trait_Value"))

for(i in 1:length(temp_loco)){
	temp <- temp_loco[[i]]
	temp <- base::split(temp, temp$Trait_ID)
	temp <- lapply(temp, function(x) data.frame(cbind(unique(x$Taxa_ID), mean(x$SI_Mass, na.rm = TRUE), mean(x$SI_Length, na.rm = TRUE), unique(x$Trait_ID), mean(x$Trait_Value, na.rm = TRUE), mean(x$Temperature, na.rm = TRUE), unique(x$Temp_Type), unique(x$Thermy))))
	temp <- lapply(temp, function(x) dplyr::rename(x, Taxa_ID = "X1", SI_Mass = "X2", SI_Length = "X3", Trait_ID = "X4", Trait_Value = "X5", Temperature = "X6", Temp_Type = "X7", Thermy = "X8"))
	temp <- do.call("rbind", temp)
	temp <- tidyr::pivot_wider(temp, names_from = "Trait_ID", values_from = "Trait_Value")
	temp_loco[[i]] <- temp

}

temp_loco[[1]] <- data.frame(cbind(unique(temp_loco[[1]]$Taxa_ID), mean(as.numeric(temp_loco[[1]]$SI_Mass)), mean(as.numeric(temp_loco[[1]]$SI_Length)), mean(as.numeric(temp_loco[[1]]$Temperature)), unique(temp_loco[[1]]$Temp_Type), unique(temp_loco[[1]]$Thermy), temp_loco[[1]][1,"Attack Velocity"], temp_loco[[1]][2,"Escape Velocity"]))
temp_loco[[1]] <- dplyr::rename(temp_loco[[1]], Taxa_ID = "unique.temp_loco..1...Taxa_ID.", SI_Mass = "mean.as.numeric.temp_loco..1...SI_Mass..", SI_Length = "mean.as.numeric.temp_loco..1...SI_Length..", Temperature = "mean.as.numeric.temp_loco..1...Temperature..", Temp_Type = "unique.temp_loco..1...Temp_Type.", Thermy = "unique.temp_loco..1...Thermy.", `Attack Velocity` = "Attack.Velocity", `Escape Velocity`= "Escape.Velocity")
temp_loco[[3]] <- data.frame(cbind(unique(temp_loco[[3]]$Taxa_ID), mean(as.numeric(temp_loco[[3]]$SI_Mass)), mean(as.numeric(temp_loco[[3]]$SI_Length)), mean(as.numeric(temp_loco[[3]]$Temperature)), unique(temp_loco[[3]]$Temp_Type), unique(temp_loco[[3]]$Thermy), temp_loco[[3]][1,"Acceleration"], temp_loco[[3]][1,"Escape Velocity"], temp_loco[[3]][2,"Turn Radius"]))
temp_loco[[3]] <- dplyr::rename(temp_loco[[3]], Taxa_ID = "unique.temp_loco..3...Taxa_ID.", SI_Mass = "mean.as.numeric.temp_loco..3...SI_Mass..", SI_Length = "mean.as.numeric.temp_loco..3...SI_Length..", Temperature = "mean.as.numeric.temp_loco..3...Temperature..", Temp_Type = "unique.temp_loco..3...Temp_Type.", Thermy = "unique.temp_loco..3...Thermy.", Acceleration = "Acceleration", `Escape Velocity` = "Escape.Velocity", `Turn Radius` = "Turn.Radius")

loco[c(8,12,28,32)] <- temp_loco[c(1,2,3,4)]

loco_categorical <- lapply(loco, function(x) data.frame(cbind(unique(x$Taxa_ID), unique(x$Temp_Type), unique(x$Thermy))))

loco_categorical[[10]] <- loco_categorical[[10]][1,]
loco_categorical[[31]] <- loco_categorical[[31]][2,]
loco_categorical <- plyr::rbind.fill(loco_categorical)
loco_categorical <- dplyr::rename(loco_categorical, c(Taxa_ID = "X1", Temp_Type = "X2", Thermy = "X3"))
loco_num <- lapply(loco, function(x) dplyr::select(x, -c("Taxa_ID","Temp_Type","Thermy")))

loco_num <- lapply(loco_num, function(x) {
				x <- as.data.frame(x)
				get_col <- colnames(x)
				x[get_col] <- sapply(x[get_col], as.numeric)
				x <- t(data.frame(colMeans(x, na.rm = TRUE)))
				x <- as.data.frame(x)
				return(x)
})

loco_num <- plyr::rbind.fill(loco_num)
loco <- cbind(loco_categorical, loco_num)
loco <- dplyr::rename(loco, c(temp_type = "Temp_Type", thermy = "Thermy", si_mass = "SI_Mass", si_length = "SI_Length", temp_loco = "Temperature", acceleration = "Acceleration", attack_velocity = "Attack Velocity", voluntary_body_velocity = "Voluntary Body Velocity", escape_velocity = "Escape Velocity", turn_radius = "Turn Radius", angular_velocity = "Angular Velocity"))

# Merge species_traits with locomotion data
species_traits <- merge(species_traits, loco, by.x = "scientific_name", by.y = "Taxa_ID", all.x = TRUE)

# Save the traits data_frame
saveRDS(species_traits, "data/intermediate/species_traits.RDS")
