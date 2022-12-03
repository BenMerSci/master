# Overhead section to format the basic data
## Reorder the terrestrial network so they refer to the good one
load("data/raw/ecopath/data/Ecopath_models.Rdata")
temp <- Ecopath_models[c(110:116), ]
Ecopath_models[111, ] <- temp[6, ]
Ecopath_models[112, ] <- temp[3, ]
Ecopath_models[113, ] <- temp[7, ]
Ecopath_models[114, ] <- temp[4, ]
Ecopath_models[115, ] <- temp[2, ]
Ecopath_models[116, ] <- temp[5, ]

## Join the networks from Ecobase
Ecobase_models <- data.frame(c("Chantuto","Mediterranea","Nicoya","Srilanka"), c("marine","marine","marine","marine"))
colnames(Ecobase_models) <- colnames(Ecopath_models)
Ecopath_models <- rbind(Ecopath_models, Ecobase_models)

## Save it back
save(Ecopath_models, file = "data/raw/ecopath/data/Ecopath_models_modif.Rdata")

## Load the Ecobase networks biomass
Ecobase_files <- list.files(path="./data/raw/ecobase_data", pattern = "(biomass)+(.csv)", full.names = T) |>
    purrr::map(~read.csv(.))

# Structure biomass data for ecobase model and save it
ecobase_biomass <- purrr::map(Ecobase_files, function(x) {
    df <- x$"Biomass..t.km.." |>
          gsub(pattern = ",", replacement = ".") |>
          as.numeric()
    return(df)
     })

saveRDS(ecobase_biomass, "data/raw/ecobase_data/biomasses.RDS")

## Combine 116 networks with 4 from Ecobase
load("data/raw/ecopath/data/GroupName.Rdata")

Ecobase_files <- purrr::map(Ecobase_files, ~.$Group.name)

Group_name <- c(GroupName, Ecobase_files)

# Transform each list of names into a DataFrame
# Add an empty column to each DataFrame
# original_name is the name initially found in the 116 Ecopath models
# scientific_name are the names that are retrieved from
# each article related to the model
Group_name <- lapply(Group_name, function(x) as.data.frame(x)) |>
             lapply(function(x) tibble::add_column(x, scientific_name = NA)) |>
             lapply(function(x) dplyr::rename(x, original_name = x))

# Web 34 - article OK
# Huizache Caimanero
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Sciaenidae"),"scientific_name"] <- "Sciaenidae" # Cynoscion xanthulum
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Elopidae"),"scientific_name"] <- "Elops affinis" # Elops affinis
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Lutjanidae"),"scientific_name"] <- "Lutjanidae" # Lutjanus novemfasciatus, Lutjanus argentiventris
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Carangidae"),"scientific_name"] <- "Carangidae" # Caranx hippos
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Centropomidae"),"scientific_name"] <- "Snook" # Centropomus robalito, Centropomus nigrescens
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Ariidae"),"scientific_name"] <- "Small catfish" # Arius guatemalensis
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Haemulidae"),"scientific_name"] <- "Haemulidae" # Anisotremus interreptus, Haemulon sexfasciatum
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Peces de fondo"),"scientific_name"] <- "Cynoglossus zanzibarensis" # Cynoglossus zanzibarensis
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Callinectes"),"scientific_name"] <- "Crabs" # Callinectes arcuatus
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Peces aguja"),"scientific_name"] <- "Hemiramphus" # Hemiramphus balao, Hemiramphus brasilienis
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Pel\xe1gicos menor"),"scientific_name"] <- "Clupeidei" # Atherinomuros stipes, Engraulis mordax, Dorosoma petense
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Gerreidae"),"scientific_name"] <- "Gerreidae" # Diapterus peruvians
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Poeciliidae"),"scientific_name"] <- "Poeciliidae"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Gobios"),"scientific_name"] <- "Dormitator latifrons" # Dormitator latifrons
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Mugilidae"),"scientific_name"] <- "Mugilidae" # Mugil cephalus 
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Palaemonidae"),"scientific_name"] <- "Macrobrachium" # Macrobranchium sp.
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Penaeidae"),"scientific_name"] <- "Litopenaeus" # Penaeus sp.
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Bivalvia"),"scientific_name"] <- "Bivalvia"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Microcrust\xe1ceos"),"scientific_name"] <- "Microcrustacean"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Zooplancton"),"scientific_name"] <- "Zooplankton"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Chanidae"),"scientific_name"] <- "Chanos chanos" # Chanos chanos 
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Gusanos"),"scientific_name"] <- "Nereidae" # Polychaete
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Gastropoda"),"scientific_name"] <- "Mollusca" # Gastropoda
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Fitoplancton"),"scientific_name"] <- "Phytoplankton"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Prod. Prim. Ben"),"scientific_name"] <- "Macrophytes"

# Web 45 - article OK
# Lake George
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Birds"),"scientific_name"] <- "Fishing birds" # Fishing eagles (aliaeetus vocifer), kingfishers (Ceryle rudis as example), cormorants, pelicans
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "H. squamipinnis"),"scientific_name"] <- "Haplochromis squamipinnis"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Bagrus dogmac"),"scientific_name"] <- "Bagrus dogmac"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Protopterus"),"scientific_name"] <- "Protopterus aethiopicus"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Clarias gariopi"),"scientific_name"] <- "Clarias gariepinus"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "H. angustifrons"),"scientific_name"] <- "Haplochromis angustifrons"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Zoobenthos"),"scientific_name"] <- "Zoobenthos" # Chaoborus spp., Copepods, Oligochaetes
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "H. nigripinnis"),"scientific_name"] <- "Haplochromis nigripinnis"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "O. niloticus"),"scientific_name"] <- "Oreochromis niloticus"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "O. leucostictus"),"scientific_name"] <- "Oreochromis leucostictus"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Benthic prod."),"scientific_name"] <- "Benthic producer"

# Web 46 - article OK
# Lake kariba
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "H. vittatus"),"scientific_name"] <- "Hydrocynus vittatus"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "L. miodon"),"scientific_name"] <- "Limnothrissa miodon"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "S. zambezensis"),"scientific_name"] <- "Synodontis zambezensis"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "Cichlids"),"scientific_name"] <- "Cichlids" # Tilapia rendalli, Serranochromis codringtoni, Serranochromis macrocephalus, Oreochromis mortimeri
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "Mussels"),"scientific_name"] <- "Bivalvia"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "Macrophytes"),"scientific_name"] <- "Macrophytes"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "Periphyton"),"scientific_name"] <- "Periphyton"

# Web 49 - article OK
# Lake Malawi 2
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Opsaridium"),"scientific_name"] <- "Opsaridium microcephalus"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Rhamphochromis"),"scientific_name"] <- "Rhamphochromis longiceps"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Diplotaxodon"),"scientific_name"] <- "Diplotaxodon"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Engraulicypris"),"scientific_name"] <- "Engraulicypris sardella"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Chaoborus"),"scientific_name"] <- "Chaoborus edulis"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Haplochromis"),"scientific_name"] <- "Copadichromis azureus"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 51 - article OK
# Lake Tanganyka 1981
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Large predators"),"scientific_name"] <- "Lates" # L. mariae, L. microlepis and L.angustifrons
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Small predators"),"scientific_name"] <- "Luciolates stappersii"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Limnothrissa"),"scientific_name"] <- "Limnothrissa miodon"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Stolothrissa"),"scientific_name"] <- "Stolothrissa tanganyicae"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 53 - article OK
# Lake Turkana 1987
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Tigerfish"),"scientific_name"] <- "Hydrocynus forskalii"
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Lates spp."),"scientific_name"] <- "Lates" # L, niloticus and L. longispinis
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Synod. schall"),"scientific_name"] <- "Synodontis schall"
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Small pelagic"),"scientific_name"] <- "Alepisaurus" # A. minutus and A. ferox
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Benthic fish"),"scientific_name"] <- "Benthic fish" # Labeo horie, Barbus bynni, Citharinus citharis and Distichodus niloticus
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 55 - article OK
# Lake Victoria 1985
# Verify Haplochromis groups
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Nile perch"),"scientific_name"] <- "Lates niloticus"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Predat. d.bream"),"scientific_name"] <- "Predatory haplochromines"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Catfishes"),"scientific_name"] <- "Large catfish" # Bagrus and Clarias
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Snoutf./squeek."),"scientific_name"] <- "Snout" # Synodontis and mormyrids
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Lungfish"),"scientific_name"] <- "Protopterus aethiopicus"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Benthiv.d.bream"),"scientific_name"] <- "Benthivorous haplochromines"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Dagaa"),"scientific_name"] <- "Rastrineobola argentea"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Lake prawn"),"scientific_name"] <- "Caridina nilotica"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Macrozoobenthos"),"scientific_name"] <- "Macrozoobenthos"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Nile tilapia"),"scientific_name"] <- "Oreochromis niloticus"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Other tilapiine"),"scientific_name"] <- "Oreochromis"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Plankt. d.bream"),"scientific_name"] <- "Planktivorous haplochromines"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Benthic prod."),"scientific_name"] <- "Benthic producer"


# Web 60 - article OK
# Maputo Bay
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Sharks"),"scientific_name"] <- "Carcharhinus melanopterus" # Carcharhinus melanopterus
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Scombridae"),"scientific_name"] <- "Scombridae" # Euthynnus affinis, Rastrelliger kanagurta
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Small pelagics"),"scientific_name"] <- "Small pelagic fish" # Hilsa kelee, Pellona ditchela
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Demersal fish"),"scientific_name"] <- "Demersal fish" # Pornadasys maculatus, Otolithes ruber,Johnius dussumieri, Johniops sina
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Crab,clam,shrim"),"scientific_name"] <- "Crustacea and Molluscs" # Scylla serrata, Eumarcia paupercula, Modiolus philippinarum, Penaeus indicus, Metapenaeus monoceros
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Het.benthos"),"scientific_name"] <- "Heterotrophic benthos"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Benthic prod"),"scientific_name"] <- "Benthic producer"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 96 - arti#cle OK
# Tampamachoco Lagoon
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Snapper"),"scientific_name"] <- "Lutjanus griseus"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Jack"),"scientific_name"] <- "Carangidae" #Hemicaranx amblyrrinchus, Caranx hippos, C. latus, C. crysos
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Toadfish"),"scientific_name"] <- "Opsanus beta"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Bay whiff"),"scientific_name"] <- "Pleuronectiforms" # Citharichthys spiloterus, Achirus lineatus
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Snook"),"scientific_name"] <- "Snook" # Centropomus undecimalis, C. paralellus, C. poeyi
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Croaker"),"scientific_name"] <- "Croaker" # Bairdiella chrysura, B. ronchus, Micropogon undulatus
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Portunid crab"),"scientific_name"] <- "Crabs" # Callinectes danae, C. boucorti, C. sapidus
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Catfish"),"scientific_name"] <- "Small catfish" # Arius felis, Cathorops melanopus
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Shrimp"),"scientific_name"] <- "Shrimps" # Penaeus setiferus, P. aztecus
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Anchovy"),"scientific_name"] <- "Anchoa" # Anchoa sp.
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Mojarra"),"scientific_name"] <- "Gerreidae" # Eucinostomus melanopterus, Diapterus rhombeus, D. auratus, Eugerres plumieri
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Goby"),"scientific_name"] <- "Gobiidae" # Bathygobius soporator, Gobionellus boleosoma
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Shhepshead"),"scientific_name"] <- "Sheephead" # Archosargus probatocephalus, Lagodon romboides
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Crustaceans"),"scientific_name"] <- "Crustacea" # Clivanarius vittatus, decapod, amphipod
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "White mullet"),"scientific_name"] <- "Mugil curema"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Spadefish"),"scientific_name"] <- "Chaetodipterus faber"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Oyster"),"scientific_name"] <- "Crassostrea virginica"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Molluscs"),"scientific_name"] <- "Mollusca" # bivalves and gastropods
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Meiofauna"),"scientific_name"] <- "Meiofauna"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Benthis producers"),"scientific_name"] <- "Benthic producer"

# Web 110 - article OK
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Mosses"),"scientific_name"] <- "Mosses"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Grasses/Sedges"),"scientific_name"] <- "Grasses"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Shrubs"),"scientific_name"] <- "Shrubs"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Forbs"),"scientific_name"] <- "Forbs"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Arthropods"),"scientific_name"] <- "Arthropoda"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Ptarmigan" # Lagopus lagopus, Lagopus muta
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Shorebirds"),"scientific_name"] <- "Shorebirds" # Calidris bairdii, C. fusciollis, C. melanotos, Pluvialis dominica, P. squatarola, Phalaropus fulicarius
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Waterfowl"),"scientific_name"] <- "Waterfowl" # Anser caerulescens atlantica, A. albifrons, A. brachyrhynchus, Branta bernicla, B. canadensis, Anas crecca, A. acuta, Cygnus columbianus
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Snow_bunting"),"scientific_name"] <- "Plectrophenax nivalis"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Arctic_wolve"),"scientific_name"] <- "Canis lupus arctos"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus hyperboreus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"

# Web 111 - article OK
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Mosses"),"scientific_name"] <- "Mosses"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Lichens"),"scientific_name"] <- "Lichens"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Grasses/Sedges"),"scientific_name"] <- "Grasses"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Shrubs"),"scientific_name"] <- "Shrubs"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Forbs"),"scientific_name"] <- "Forbs"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Arthropods"),"scientific_name"] <- "Arthropoda"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Geese"),"scientific_name"] <- "Geese" # Anser brachyrhynchus, Branta leucopsis
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Ptarmigan"  # Lagopus lagopus, Lagopus muta
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Passerines"),"scientific_name"] <- "Plectrophenax nivalis"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Shorebirds"),"scientific_name"] <- "Shorebirds"  # Calidris bairdii, C. fusciollis, C. melanotos, Pluvialis dominica, P. squatarola, Phalaropus fulicarius
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus hyperboreus"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Jaegers"),"scientific_name"] <- "Jaegers" # Stercorarius longicaudus, S. parasiticus

# Web 112 - article OK
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Mosses"),"scientific_name"] <- "Mosses"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Lichens"),"scientific_name"] <- "Lichens"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Grasses/Sedges"),"scientific_name"] <- "Grasses"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Shrubs"),"scientific_name"] <- "Shrubs"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Forbs"),"scientific_name"] <- "Forbs"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Arthropods"),"scientific_name"] <- "Arthropoda"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Lemmings"),"scientific_name"] <- "Lemmings" # Lemmus trimucronatus, Dicrostonyx groenlandicus
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Voles"),"scientific_name"] <- "Microtus oeconomus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Waterfowl"),"scientific_name"] <- "Waterfowl" # Anser caerulescens atlantica, A. albifrons, A. brachyrhynchus, Branta bernicla, B. canadensis, Anas crecca, A. acuta, Cygnus columbianus
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Geese"),"scientific_name"] <- "Geese" # Anser brachyrhynchus, Branta leucopsis
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Ptarmigan" # Lagopus lagopus, Lagopus muta
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Passerines"),"scientific_name"] <- "Passerines" # Calcarius lapponicus, Plectrophenax nivalis, Eremophila alpestris, Anthus rubescens
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Shorebirds"),"scientific_name"] <- "Shorebirds" # Calidris bairdii, C. fusciollis, C. melanotos, Pluvialis dominica, P. squatarola, Phalaropus fulicarius
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Red_fox"),"scientific_name"] <- "Vulpes vulpes"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Falcons"),"scientific_name"] <- "Falco" # Falco peregrinus, F. rusticolus
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Wolverine"),"scientific_name"] <- "Gulo gulo"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Gulls"),"scientific_name"] <- "Gulls" # Larus hyperboreus, L. argentatus
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Jaegers"),"scientific_name"] <- "Jaegers" # Stercorarius longicaudus, S. parasiticus

# Web 113 - article OK
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Mosses"),"scientific_name"] <- "Mosses"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Lichens"),"scientific_name"] <- "Lichens"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Grasses/Sedges"),"scientific_name"] <- "Grasses"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Shrubs"),"scientific_name"] <- "Shrubs"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Forbs"),"scientific_name"] <- "Forbs"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Arthropods"),"scientific_name"] <- "Arthropoda"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Geese"),"scientific_name"] <- "Geese" # Anser brachyrhynchus, Branta leucopsis
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Ptarmigan" # Lagopus lagopus, Lagopus muta
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Shorebirds"),"scientific_name"] <- "Shorebirds" # Calidris bairdii, C. fusciollis, C. melanotos, Pluvialis dominica, P. squatarola, Phalaropus fulicarius
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Snow_bunting"),"scientific_name"] <- "Plectrophenax nivalis"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiaca"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"

# Web 114 - article OK
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Mosses"),"scientific_name"] <- "Mosses"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Lichens"),"scientific_name"] <- "Lichens"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Grasses/Sedges"),"scientific_name"] <- "Grasses"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Shrubs"),"scientific_name"] <- "Shrubs"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Forbs"),"scientific_name"] <- "Forbs"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Arthropods"),"scientific_name"] <- "Arthropoda"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Brown_lemmings"),"scientific_name"] <- "Lemmus trimucronatus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Tundra_voles"),"scientific_name"] <- "Microtus oeconomus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Passerines"),"scientific_name"] <- "Passerines"  # Calcarius lapponicus, Plectrophenax nivalis, Eremophila alpestris, Anthus rubescens
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Shorebirds"),"scientific_name"] <- "Shorebirds" # Calidris bairdii, C. fusciollis, C. melanotos, Pluvialis dominica, P. squatarola, Phalaropus fulicarius
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Ptarmigan" # Lagopus lagopus, Lagopus muta
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Waterfowl"),"scientific_name"] <- "Waterfowl" # Anser caerulescens atlantica, A. albifrons, A. brachyrhynchus, Branta bernicla, B. canadensis, Anas crecca, A. acuta, Cygnus columbianus
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Red_fox"),"scientific_name"] <- "Vulpes vulpes"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Wolverine"),"scientific_name"] <- "Gulo gulo"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Grizzly_bear"),"scientific_name"] <- "Ursus horribilis"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiaca"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Peregrine_falco"),"scientific_name"] <- "Falco peregrinus"

# Web 115 - article OK
temp <- Group_name[[115]]
Group_name[[115]][9,] <- temp[10,]
Group_name[[115]][10,] <- temp[9,]
Group_name[[115]][11,] <- temp[15,]
Group_name[[115]][12,] <- temp[11,]
Group_name[[115]][13,] <- temp[12,]
Group_name[[115]][14,] <- temp[13,]
Group_name[[115]][15,] <- temp[16,]
Group_name[[115]][16,] <- temp[17,]
Group_name[[115]][17,] <- temp[14,]

Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Mosses"),"scientific_name"] <- "Mosses"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Forbs"),"scientific_name"] <- "Forbs"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Shrubs"),"scientific_name"] <- "Shrubs"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Sedges/Grasses"),"scientific_name"] <- "Grasses"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Arthropods"),"scientific_name"] <- "Arthropoda"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Snow_goose"),"scientific_name"] <- "Anser caerulescens atlantica"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Brown_lemming"),"scientific_name"] <- "Lemmus trimucronatus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Passerines"),"scientific_name"] <- "Passerines" # Calcarius lapponicus, Plectrophenax nivalis, Eremophila alpestris, Anthus rubescens
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Shorebirds"),"scientific_name"] <- "Shorebirds" # Calidris bairdii, C. fusciollis, C. melanotos, Pluvialis dominica, P. squatarola, Phalaropus fulicarius
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Gulls"),"scientific_name"] <- "Gulls"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Weasel"),"scientific_name"] <- "Mustela erminea"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Arctic_Fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiaca"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Parasitic_jaeger"),"scientific_name"] <- "Stercorarius parasiticus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Peregrine_falcon"),"scientific_name"] <- "Falco peregrinus"

# Web 116 - article OK
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Mosses"),"scientific_name"] <- "Mosses"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Lichens"),"scientific_name"] <- "Lichens"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Grasses/Sedges"),"scientific_name"] <- "Grasses"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Shrubs"),"scientific_name"] <- "Shrubs"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Forbs"),"scientific_name"] <- "Forbs"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Arthropods"),"scientific_name"] <- "Arthropoda"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Lemmings"),"scientific_name"] <- "Lemmings"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Voles"),"scientific_name"] <- "Microtus oeconomus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Waterfowl"),"scientific_name"] <- "Waterfowl" # Anser caerulescens atlantica, A. albifrons, A. brachyrhynchus, Branta bernicla, B. canadensis, Anas crecca, A. acuta, Cygnus columbianus
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Geese"),"scientific_name"] <- "Geese" # Anser brachyrhynchus, Branta leucopsis
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Ptarmigan" # Lagopus lagopus, Lagopus muta
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Passerines"),"scientific_name"] <- "Passerines" # Calcarius lapponicus, Plectrophenax nivalis, Eremophila alpestris, Anthus rubescens
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Shorebirds"),"scientific_name"] <- "Shorebirds" # Calidris bairdii, C. fusciollis, C. melanotos, Pluvialis dominica, P. squatarola, Phalaropus fulicarius
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Red_fox"),"scientific_name"] <- "Vulpes vulpes"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Falcons"),"scientific_name"] <- "Falco peregrinus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiaca"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus hyperboreus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Jaegers"),"scientific_name"] <- "Jaegers" # Stercorarius longicaudus, S. parasiticus
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Brown_bear"),"scientific_name"] <- "Ursus horribilis"

## Chantuto network
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Water birds"),"scientific_name"] <- "Seabirds" # Podicipediformes, Pelecaniformes, Ciconiiformes, Anseriformes, Falconiformes, Gruiformes, Charadriiformes
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Crocodilians"),"scientific_name"] <- "Crocodilians" # Caiman crocodilus, Crocodylus acutus
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Dormitator latifrons"),"scientific_name"] <- "Dormitator latifrons"
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Eleotrids"),"scientific_name"] <- "Eleotridae" # Eleotris picta, Gobiomorus maculatus
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Sciades guatemalensis"),"scientific_name"] <- "Sciades guatemalensis"
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Ariid catfishes"),"scientific_name"] <- "Small catfish" # Cathorops liropus, C. steindachneri, Notarius kessleri
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Jacks"),"scientific_name"] <- "Carangidae" # Caranx caninus, C. sexfasciatus, Hemicaranx zelotes, Oligoplites altus, O. saurus, Selene brevoorti
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Snooks"),"scientific_name"] <- "Snook" # Centropomus medius, C. nigrescens, C robalito, C. viridi
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Anchovies"),"scientific_name"] <- "Anchoa" # Anchoa lucida, A. mundeola, A. starksi, A. ischana, Anchovia macrolepidota
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Sciaenids"),"scientific_name"] <- "Sciaenidae" # Cynoscion albus, Bairdiella armata, Micropogonias altipinnis, Stellifer cf. walkeri
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Gobiids"),"scientific_name"] <- "Gobiidae" # Aboma etheostoma, Ctenogobius sagittula, Gobionellus microdon
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Mugilids"),"scientific_name"] <- "Mugilidae" # Mugil curema, M. cephalus, M. hospes
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Gerreids"),"scientific_name"] <- "Gerreidae" # Diapterus brevirostris, Eucinostomus currani, E. dowii, Gerres simillimus
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Pleuronectiforms"),"scientific_name"] <- "Pleuronectiforms" # Achirus mazatlanus, Citharichthys gilberti, Trinectes fonsecensis
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Poeciliids"),"scientific_name"] <- "Poeciliidae" # Poecilia nelsoni, P. sphenops, Poeciliopsis turrubarensis
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Sphoeroides annulatus"),"scientific_name"] <- "Sphoeroides annulatus"
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Cichlids"),"scientific_name"] <- "Cichlids" # Amphilophus trimaculatus, Astatheros macracanthus, Oreochromis niloticus
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Snappers"),"scientific_name"] <- "Lutjanidae" # Lutjanus argentiventris, Lutjanus colorado
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Atractosteus tropicus"),"scientific_name"] <- "Atractosteus tropicus" # Atractosteus tropicus
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Plactivore fishes"),"scientific_name"] <- "Planktivore fish" # Atherinella guatemalensis, Lile gracilis, L. nigrofasciata, Membras gilberti, Hippocampus ingens, Hyporhamphus snyderi
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Carnivore fishes"),"scientific_name"] <- "Carnivore fish" # Elops affinis, Roeboides bouchellei, Synodus scituliceps
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Omnivore fishes"),"scientific_name"] <- "Omnivore fish" # Anableps dowei, Astyanax aeneus, Kyphosus elegans, Polydactylus approximans, Rhamdia guatemalensis
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Litopenaeus"),"scientific_name"] <- "Litopenaeus" # Litopenaeus stylirostris, L. vannamei, L. occidentalis
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Macrobrachium"),"scientific_name"] <- "Macrobrachium" # Macrobrachium americanum, M. tenellum, Palaemonetes ritteri
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Other shrimps"),"scientific_name"] <- "Shrimps" # Alpheus sp., Farfantepenaeus brevirostris, Hyppolyte williamsi, Sergestes pestafer, Sicyonia aliaffinis
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Callinectes"),"scientific_name"] <- "Crabs" # Callinectes arcuatus, C. toxotes
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Grapsids"),"scientific_name"] <- "Grapsid" # Clibanarius panamensis, Sesarma rhizophorae, Uca sp
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Insects"),"scientific_name"] <- "Insects" # Formicidae, Odonata, unidentified larvae
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Polychaetes"),"scientific_name"] <- "Nereidae" # Polychaeta, Sipunculids
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Infauna"),"scientific_name"] <- "Infauna" # Tanaidacea, Isopoda, Ostracoda, Cumacea, Amphipoda
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Bivalves"),"scientific_name"] <- "Bivalvia" # Bivalvia
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton" # Cnidarians, gastropods and polychaetes larvae, cladocerans, copepods, cirripedia larvae, decapod larvae, fish eggs
Group_name[[117]][which(Group_name[[117]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton" # Diatoms, dinoflagellates, euglenophytes, cyanophytes, chlorophytes, silicoflagellates

## Huizache network
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Sciaenids"),"scientific_name"] <- "Sciaenidae" # Cynoscion xanthulum
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Elopids"),"scientific_name"] <- "Elops affinis" # Elops affinis
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Lutjanids"),"scientific_name"] <- "Lutjanidae" # Lutjanus novemfasciatus, Lutjanus argentiventris
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Carangids"),"scientific_name"] <- "Carangidae" # Caranx hippos
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Centropomids"),"scientific_name"] <- "Snook" # Centropomus robalito, Centropomus nigrescens
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Ariids"),"scientific_name"] <- "Small catfish" # Arius guatemalensis
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Haemulids"),"scientific_name"] <- "Haemulidae" # Anisotremus interreptus, Haemulon sexfasciatum
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Pleuronectoidei"),"scientific_name"] <- "Cynoglossus zanzibarensis" # Cynoglossus zanzibarensis
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Callinectes"),"scientific_name"] <- "Crabs" # Callinectes arcuatus
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Belonoidei"),"scientific_name"] <- "Hemiramphus" # Hemiramphus balao, Hemiramphus brasilienis
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Clupeidei"),"scientific_name"] <- "Clupeidei" # Atherinomuros stipes, Engraulis mordax, Dorosoma petense
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Gerreids"),"scientific_name"] <- "Gerreidae" # Diapterus peruvians
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Poeciliids"),"scientific_name"] <- "Poeciliidae"
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Gobioidei"),"scientific_name"] <- "Dormitator latifrons" # Dormitator latifrons
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Mugilids"),"scientific_name"] <- "Mugilidae" # Mugil cephalus 
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Palaemonids"),"scientific_name"] <- "Macrobrachium" # Macrobranchium sp.
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Penaeids"),"scientific_name"] <- "Litopenaeus" # Penaeus sp.
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Bivalvs"),"scientific_name"] <- "Bivalvia"
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Microcrustaceans"),"scientific_name"] <- "Microcrustacean"
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Chanids"),"scientific_name"] <- "Chanos chanos" # Chanos chanos 
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Polychaets"),"scientific_name"] <- "Nereidae" # Polychaete
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Gastropods"),"scientific_name"] <- "Mollusca" # Gastropoda
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
#Group_name[[?]][which(Group_name[[?]][,"original_name"] == "Macrophytes"),"scientific_name"] <- "Macrophytes"

## Meditarreanea network
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Bluntnose sixgill shark"),"scientific_name"] <- "Hexanchus griseus" # Hexanchus griseus
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Demersal sharks"),"scientific_name"] <- "Demersal shark" # Galeus melastomus, Etmopterus spinax, Dalatias licha
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Monkfish"),"scientific_name"] <- "Lophius piscatorius" # Lophius piscatorius
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Phycis blennoides"),"scientific_name"] <- "Phycis blennoides" # Phycis blennoides
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Lepidion lepidion"),"scientific_name"] <- "Lepidion lepidion" # Lepidion lepidion
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Macrourids"),"scientific_name"] <- "Macrouridae" # Nezumia aequalis, Coelorinchus mediterraneus, Coryphaenoides guentheri, Trachyrincus scabrus
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Mora moro"),"scientific_name"] <- "Mora moro" # Mora moro
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Alepocephalus rostratus"),"scientific_name"] <- "Alepocephalus rostratus"
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Other demersal, small"),"scientific_name"] <- "Demersal fish2" # Cataetyx alleni, Bathypterois mediterraneus, Notacanthus bonaparte, Nettastoma melanurum
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Cephalopods"),"scientific_name"] <- "Medium cephalopod" # Bathypolipus sponsalis, Opistoteuthis calypso, Todarodes sagittatus
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Aristeus antennatus"),"scientific_name"] <- "Aristeus antennatus" # Aristeus antennatus
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Mesopelagic crustacea"),"scientific_name"] <- "Shrimps" # Acantephyra eximia, A. pelagica, Pasiphaea multidentata, P. sivado, Sergestes arcticus, Sergia robusta
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Benthopelagic fish"),"scientific_name"] <- "Benthopelagic fish" # Cyclothone braueri, Argyropelecus hemigymnus, Lampanyctus crocodilus
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Benthic invertebrates, crustacea"),"scientific_name"] <- "Crustacea" # decapod crustaceans: Geryon longipes, Munida tenuimana, Pagurus alatus, Paromola cuvieri and Polycheles typhlops.
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Benthic invertebrates, other"),"scientific_name"] <- "Macrozoobenthos" # non-crustacean megabenthos and macrobenthos invertebrate
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Zooplankton, gelatinous"),"scientific_name"] <- "Zooplankton gelatinous" # Pelagia noctiluca, Nausithoe sp., Salpa sp. and Pyrosoma atlanticum
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Zooplankton, BBL"),"scientific_name"] <- "Zooplankton"
Group_name[[118]][which(Group_name[[118]][,"original_name"] == "Meiobenthos"),"scientific_name"] <- "Meiobenthos"


## Nicoya network
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Microphytobenthos"),"scientific_name"] <- "Phytobenthos"
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Mangroves"),"scientific_name"] <- "Mangroves"
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Shrimp"),"scientific_name"] <- "Shrimps" # Penaeus sp, Trachypenaeus sp., Sicyonya
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Squids"),"scientific_name"] <- "Small cephalopod" # Loliolopsis diomedeae, Lolliguncula panamensis
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Small pelagics"),"scientific_name"] <- "Small pelagic fish" # Neopisthopterus tropicus, Anchoa lucida, 
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Carangids & others"),"scientific_name"] <- "Carangidae" # Caranx caninus, C. otrynter, C. speciosus, C. vinctus, Peprilus medius, P. snyderi, Selene brevortii, S. oerstedii, S, peruviana, Trachinotus paitensis
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Small demersal fish"),"scientific_name"] <- "Small demersal fish" # Sciaenids, Haemulids, Triglids, Gerreids, Serranids,Scorpionids, Gobies
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Flatfish"),"scientific_name"] <- "Pleuronectiforms" # Achirus kluzingeri, Syacium ovale, S. latifrons, Symphurus chahanaudi, S. callopterus, S. elongatus, Cyclopsetta querna, Etropus crossotus, Citharichtys platophrys
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Catfish"),"scientific_name"] <- "Small catfish" # Arius dasycephalus, Arius dovi, Arius platypogon, Arius osculus
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Snappers & Grunts"),"scientific_name"] <- "Snapper Grunts" # Lutjanus guttatus, Haemulon scuderi, Anisotrema dovii, calanus brachisomis
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Lizardfish"),"scientific_name"] <- "Synodus sp" # Synodus scituliceps, Synodus evemanni, Synotus sechurae
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Large Scianids"),"scientific_name"] <- "Large scianidae" # Cynoscion albus, Cynoscion phoxocephalus
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Rays & Sharks"),"scientific_name"] <- "Rays Sharks" # Dasyatiis longus, Urotrygon chilensis, Raya velezi, Rhinobates leucorhynchus, Narcine brasiliensis, Mustelus lunulatus
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Morays & eels"),"scientific_name"] <- "Cynoponctius coniceps" # Cynoponctius coniceps and others
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Endobenthos"),"scientific_name"] <- "Nereidae" # Polychaete
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Epibenthos"),"scientific_name"] <- "Epibenthos" # Gastropoda, Bivalvia, Sea urchins, Sea stars
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Predatory crabs"),"scientific_name"] <- "Crabs" # Callinectes toxotes, Callinectes sapidus, Callinectes arcuatus, Portunus asper, Portunus iridescens, Portunus acuminatus, Euphylax robustus, 
Group_name[[119]][which(Group_name[[119]][,"original_name"] == "Sea/shore birds"),"scientific_name"] <- "Seabirds" # Pelicanus sp., Podiceps dominicus, Sula sp., Phalacrocorax olivaceus, Oceanodroma microsoma, Fregata magnificens, Anhinga anhinga

## Srilanka network
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "Large tunas"),"scientific_name"] <- "Scomberomorus commerson"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "medium tunas"),"scientific_name"] <- "Katsuwonus pelamis"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "small barracudas"),"scientific_name"] <- "Sphyraena obtusata"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "small tunas"),"scientific_name"] <- "Auxis thazard thazard"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "large barracudas"),"scientific_name"] <- "Sphyraena jello"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "wolf herrings"),"scientific_name"] <- "Chirocentrus dorab"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "needlefish"),"scientific_name"] <- "Strongylura leiura"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "other carangids"),"scientific_name"] <- "Carangoides malabaricus"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "pomfrets, torps"),"scientific_name"] <- "Parastromateus niger"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "false trevallie"),"scientific_name"] <- "Lactarius lactarius"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "yellowstripe"),"scientific_name"] <- "Selaroides leptolepis"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "dolphinfishes"),"scientific_name"] <- "Coryphaena hippurus"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "cephalopods"),"scientific_name"] <- "Medium cephalopod"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "sea cat fish"),"scientific_name"] <- "Arius bilineatus"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "ribbonfish"),"scientific_name"] <- "Lepturacanthus savala"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "ponyfishes"),"scientific_name"] <- "Leiognathus brevirostris"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "soles"),"scientific_name"] <- "Euryglossa orientalis"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "indian mackerel"),"scientific_name"] <- "Rastrelliger kanagurta"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "terapontids"),"scientific_name"] <- "Terapon theraps"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "anchovies"),"scientific_name"] <- "Stolephorus heterolobus"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "drums"),"scientific_name"] <- "Otolithes ruber"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "other clupeids"),"scientific_name"] <- "Hilsa kelee"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "sardines"),"scientific_name"] <- "Sardinella gibbosa"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "thrissa spp"),"scientific_name"] <- "Thryssa setirostris"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "herrings"),"scientific_name"] <- "Amblygaster sirm"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "crabs"),"scientific_name"] <- "Crabs"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "flying fishes"),"scientific_name"] <- "Hirundichthys oxycephalus"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "milk fish "),"scientific_name"] <- "Chanos chanos"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "shrimps"),"scientific_name"] <- "Shrimps"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "bigeye scad"),"scientific_name"] <- "Selar crumenophthalmus"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "halfbeaks"),"scientific_name"] <- "Rhynchorhamphus malabaricus"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "mugilids"),"scientific_name"] <- "Mugil cephalus"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "annelids"),"scientific_name"] <- "Polychaeta"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "zoobenthos"),"scientific_name"] <- "Zoobenthos"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "molluscs"),"scientific_name"] <- "Mollusca"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[120]][which(Group_name[[120]][,"original_name"] == "phytobenthos"),"scientific_name"] <- "Phytobenthos"

# Write the list as a .RDS file
saveRDS(Group_name, file = "data/intermediate/Group_name.RDS")
