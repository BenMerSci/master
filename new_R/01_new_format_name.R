# Modify the order of the models so they fit the data
load("data/raw/ecopath/data/Ecopath_models.Rdata")
temp <- Ecopath_models[c(110:116), ]
Ecopath_models[111, ] <- temp[6, ]
Ecopath_models[112, ] <- temp[3, ]
Ecopath_models[113, ] <- temp[7, ]
Ecopath_models[114, ] <- temp[4, ]
Ecopath_models[115, ] <- temp[2, ]
Ecopath_models[116, ] <- temp[5, ]
save(Ecopath_models, file = "data/raw/ecopath/data/Ecopath_models_modif.Rdata")

# Load the name of each "species" in each 116 networks
# Group_name, list of length 116 for 116 networks
load("data/raw/ecopath/data/GroupName.Rdata")

# Transform each list of names into a DataFrame
# Add an empty column to each DataFrame
# original_name is the name initially found in the 116 Ecopath models
# scientific_name are the names that are retrieved from
# each article related to the model
Group_name <- lapply(GroupName, function(x) as.data.frame(x)) |>
             lapply(function(x) tibble::add_column(x, scientific_name = NA)) |>
             lapply(function(x) dplyr::rename(x, original_name = x))

# Web 4 - article OK
# Set bodymasses
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Macrozoobenthos"),"scientific_name"] <- "Macrozoobenthos"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Bent fauna"),"scientific_name"] <- "Meiofauna"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Cephalopods"),"scientific_name"] <- "Cephalopods"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Other small pelagics"),"scientific_name"] <- "Small pelagic fish"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Scads"),"scientific_name"] <- "Decapterus"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Lemuru"),"scientific_name"] <- "Sardinella lemuru"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Medium pelagics"),"scientific_name"] <- "Medium pelagic fish"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Demersal fish"),"scientific_name"] <- "Demersal fish"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Seabirds"),"scientific_name"] <- "Seabirds"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Res dolphins"),"scientific_name"] <- "Small cetacea"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Transient whales"),"scientific_name"] <- "Large cetacea"

# Web 9 - article OK
# 
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Copepoda"),"scientific_name"] <- "Copepoda" # Calanoid
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Worms"),"scientific_name"] <- "Nereidae" # Polychaete
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Molluscs"),"scientific_name"] <- "Mollusca" # Bivalvia & Gastropod
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Microcrustacean"),"scientific_name"] <- "Microcrustacean" # Euphausiacea, Sergestida, Amphipoda, Isopoda. Tanaidacea, Mysidacea. Ostracoda, Stomatopoda, Cumacea
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Decapoda"),"scientific_name"] <- "Decapoda"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "O. chrysoptera"),"scientific_name"] <- "Orthopristis chrysoptera"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "L. rhomboides"),"scientific_name"] <- "Lagodon rhomboides"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "O. oglinum"),"scientific_name"] <- "Opisthonema oglinum"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "H. jaguana"),"scientific_name"] <- "Harengula jaguana"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "A. rhomboidalis"),"scientific_name"] <- "Archosargus rhomboidalis"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Catfish"),"scientific_name"] <- "Arius" # Arius felis, Arius melanopus
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Mojarra"),"scientific_name"] <- "Eucinostomus" # Eucinostomus gula, Eucinostomus argenteus
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "H. plumieri"),"scientific_name"] <- "Haemulon plumieri"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "B. chrysoura"),"scientific_name"] <- "Bairdiella chrysoura"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "A. quadricornis"),"scientific_name"] <- "Acanthostracion quadricornis"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Other fishes"),"scientific_name"] <- "Other fish1" # Engraulidae, Syngnathidae, nguillidae, Synodontidae, Sparidae, Carangidae
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Benthic prod."),"scientific_name"] <- "Benthic producer"

# Web 17 - article OK
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Blue marlin"),"scientific_name"] <- "Makaira mazara"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Other billfish"),"scientific_name"] <- "Billfish" # Other than Blue marlin and Swordfish
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Large sharks"),"scientific_name"] <- "Large shark" # mako shark and white shark
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Blue shark"),"scientific_name"] <- "Prionace glauca"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Brown sharks"),"scientific_name"] <- "Medium shark" # oceanic white tip and silky sharks
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Yellowfin tuna"),"scientific_name"] <- "Thunnus albacares"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Bigeye tuna"),"scientific_name"] <- "Thunnus obesus"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Albacore tuna"),"scientific_name"] <- "Thunnus alalunga"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Mahimahi"),"scientific_name"] <- "Coryphaena hippurus"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Skipjack tuna"),"scientific_name"] <- "Katsuwonus pelamis"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Small scombrids"),"scientific_name"] <- "Auxis" # Auxis sp.
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Flying squid"),"scientific_name"] <- "Ommastephes bartrami"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Lancetfish"),"scientific_name"] <- "Alepisaurus ferox"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Squids"),"scientific_name"] <- "Cephalopods"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Flying fish"),"scientific_name"] <- "Flying fish"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Mesopelagic fishes"),"scientific_name"] <- "Mesopelagic fish" # http://earthguide.ucsd.edu/fishes/environment/0_images/Original/myctophids/salvanes_01.pdf
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Epipelagic fishes"),"scientific_name"] <- "Epipelagic fish"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Epipelagic micronekton"),"scientific_name"] <- "Epipelagic micronekton"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Mseopelagic micronekton"),"scientific_name"] <- "Mesopelagic micronekton"
#Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 45 - article OK
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
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Opsaridium"),"scientific_name"] <- "Opsaridium microcephalus"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Rhamphochromis"),"scientific_name"] <- "Rhamphochromis longiceps"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Diplotaxodon"),"scientific_name"] <- "Diplotaxodon"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Engraulicypris"),"scientific_name"] <- "Engraulicypris sardella"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Chaoborus"),"scientific_name"] <- "Chaoborus edulis"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Haplochromis"),"scientific_name"] <- "Copadichromis azureus"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 50 - article OK
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Large predators"),"scientific_name"] <- "Lates" # L. mariae, L. microlepis and L.angustifrons
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Small predators"),"scientific_name"] <- "Luciolates stappersii"
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Limnothrissa"),"scientific_name"] <- "Limnothrissa miodon"
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Stolothrissa"),"scientific_name"] <- "Stolothrissa tanganyicae"
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 51 - article OK
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Large predators"),"scientific_name"] <- "Lates" # L. mariae, L. microlepis and L.angustifrons
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Small predators"),"scientific_name"] <- "Luciolates stappersii"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Limnothrissa"),"scientific_name"] <- "Limnothrissa miodon"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Stolothrissa"),"scientific_name"] <- "Stolothrissa tanganyicae"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 52 - article OK
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Tigerfish"),"scientific_name"] <- "Hydrocynus forskalii"
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Lates spp."),"scientific_name"] <- "Lates" # L, niloticus and L. longispinis
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Synod. schall"),"scientific_name"] <- "Synodontis schall"
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Small pelagic"),"scientific_name"] <- "Alepisaurus" # A. minutus and A. ferox
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Benthic fish"),"scientific_name"] <- "Benthic fish" # Labeo horie, Barbus bynni, Citharinus citharis and Distichodus niloticus
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 53 - article OK
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Tigerfish"),"scientific_name"] <- "Hydrocynus forskalii"
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Lates spp."),"scientific_name"] <- "Lates" # L, niloticus and L. longispinis
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Synod. schall"),"scientific_name"] <- "Synodontis schall"
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Small pelagic"),"scientific_name"] <- "Alepisaurus" # A. minutus and A. ferox
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Benthic fish"),"scientific_name"] <- "Benthic fish" # Labeo horie, Barbus bynni, Citharinus citharis and Distichodus niloticus
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 54 - article OK
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Nile perch"),"scientific_name"] <- "Lates niloticus"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Predat. d.bream"),"scientific_name"] <- "Predatory haplochromines"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Catfishes"),"scientific_name"] <- "Large catfish" # Bagrus and Clarias
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Snoutf./squeek."),"scientific_name"] <- "Snout" # Synodontis and mormyrids
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Lungfish"),"scientific_name"] <- "Protopterus aethiopicus"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Benthiv.d.bream"),"scientific_name"] <- "Benthivorous haplochromines"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Dagaa"),"scientific_name"] <- "Rastrineobola argentea"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Lake prawn"),"scientific_name"] <- "Caridina nilotica"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Macrozoobenthos"),"scientific_name"] <- "Macrozoobenthos"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Nile tilapia"),"scientific_name"] <- "Oreochromis niloticus"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Other tilapiine"),"scientific_name"] <- "Oreochromis"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Plankt. d.bream"),"scientific_name"] <- "Planktivorous haplochromines"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Benthic prod."),"scientific_name"] <- "Benthic producer"

# Web 55 - article OK
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
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Sharks"),"scientific_name"] <- "Carcharhinus melanopterus"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Scombridae"),"scientific_name"] <- "Scombridae"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Small pelagics"),"scientific_name"] <- "Small pelagic fish" # Hilsa kelee
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Demersal fish"),"scientific_name"] <- "Demersal fish" # Pornadasys maculatus, Otolithes ruber,Johnius dussumieri, Johniops sina
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Crab,clam,shrim"),"scientific_name"] <- "Crustacea and Molluscs"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Het.benthos"),"scientific_name"] <- "Heterotrophic benthos"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Benthic prod"),"scientific_name"] <- "Benthic producer"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 72 - article OK
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Minke whales"),"scientific_name"] <- "Balaenoptera acutorostrata"
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Fin whales"),"scientific_name"] <- "Balaenoptera physalus"
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Humpback whales"),"scientific_name"] <- "Megaptera novaeangliae"
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Brydes whales"),"scientific_name"] <- "Balaenoptera edeni"
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Sei whales"),"scientific_name"] <- "Balaenoptera borealis"
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Baleen whales"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Beaked whales"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Sperm whales"),"scientific_name"] <- "Physeter macrocephalus"
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Killer whales"),"scientific_name"] <- "Orcinus orca"
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Dolphins"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Seabirds"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Large pelagics"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Mesopelagic predators"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Bathydeersal predators"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Sharks"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Rays"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Coastal tunas"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Coastal demersal"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Clupeids"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Other coastal pelagics"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Cephalopods"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Crustaceans"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Benthos"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Benthic producers"),"scientific_name"] <- ""
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
#Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 81 - article OK
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Dicentrarchus labrax(fish)"),"scientific_name"] <- "Dicentrarchus labrax"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Pomatoschistus microps (suprabenthos)"),"scientific_name"] <- "Pomatoschistus microps"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Other fishes"),"scientific_name"] <- "Other fish2" # Trisopterus luscus, Pleuronectes platessa, Clupea harengus, Sprattus sprattus, Solea vulgaris, Pomatoschistus microps
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Platichthys flesus (fish)"),"scientific_name"] <- "Platichthys flesus"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Birds"),"scientific_name"] <- "Seabirds" # Limicolous, Rallidae, Anatidae and Laridae plus two minor groups like Podicipedidae and Phalocrocoracidae. Calidris alpina,Haemotopus ostrealegus,Numenius aquatus,Fulica atra,Anas crecca,Anas plathyrhynchus,Larus ridibundus,Larus argentus, Podiceps cristatus, Phalocrocorax carbo
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Crangon crangon (suprabenthos)"),"scientific_name"] <- "Crangon crangon"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Palaemon longirostris (suprabenthos)"),"scientific_name"] <- "Palaemon longirostris"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Neomysis integer (suprabenthos)"),"scientific_name"] <- "Neomysis integer"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Omnivorous & bentjhic predators"),"scientific_name"] <- "Benthic predators" # Nereis diversicolor, Nephtys hombergii
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton" # marine copepods (Pseudocalanus elongatus, Paracalanus parvus and Temora longicornis) and Ctenophores (Pleurobrachia pileus)
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Benthic suspension feeders"),"scientific_name"] <- "Benthic suspension feeders" # Cerastoderma edule and Abra alba
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Benthic deposit feeders"),"scientific_name"] <- "Benthic deposit feeders" # Pectinaria koreni, Macoma balthica and Owenia fusiformis
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Phytobenthos"),"scientific_name"] <- "Phytobenthos" # Macroalgae, Chlorophytes (green algae) and Pheophytes (brown algae), Rhodophytes (red algae)

## Web 94 - article OK
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton" # ok
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "B. chrysoura"),"scientific_name"] <- "Bairdiella chrysoura" # ok
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Anchovies"),"scientific_name"] <- "Anchovy" # ok
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "D. auratus"),"scientific_name"] <- "Diapterus auratus" # ok
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "M. curema"),"scientific_name"] <- "Mugil curema" # ok
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "O. saurus"),"scientific_name"] <- "Oligoplites saurus" # ok
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Shrimp"),"scientific_name"] <- "Penaeus duorarum" # ok
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Other decapods"),"scientific_name"] <- "Decapods" #ok 
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Other fishes"),"scientific_name"] <- "Other fish"
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Benthos"),"scientific_name"] <- "Benthos" #ok 
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "S. notata"),"scientific_name"] <- "Strongylura notata" #ok 
#Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton" # ok

# Web 96 - article OK
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Snapper"),"scientific_name"] <- "Lutjanus griseus"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Jack"),"scientific_name"] <- "Carangidae" #Hemicaranx amblyrrinchus, Caranx hippos, C. latus, C. crysos
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Toadfish"),"scientific_name"] <- "Opsanus beta"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Bay whiff"),"scientific_name"] <- "Citharichthys spilopterus" # Citharichthys spiloterus, Achirus lineatus
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Snook"),"scientific_name"] <- "Snook" # Centropomus undecimalis, C. paralellus, C. poeyi
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Croaker"),"scientific_name"] <- "Croaker" # Bairdiella chrysura, B. ronchus, Micropogon undulatus
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Portunid crab"),"scientific_name"] <- "Crabs" # Callinectes danae, C. boucorti, C. sapidus
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Catfish"),"scientific_name"] <- "Small catfish" # Arius felis, Cathorops melanopus
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Shrimp"),"scientific_name"] <- "Shrimps" # Penaeus setiferus, P. aztecus
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Anchovy"),"scientific_name"] <- "Anchoa"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Mojarra"),"scientific_name"] <- "Gerreidae" # Eucinostomus melanopterus, Diapterus rhombeus, D. auratus, Eugerres plumieri
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Goby"),"scientific_name"] <- "Goby" # Bathygobius soporator, Gobionellus boleosoma
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Shhepshead"),"scientific_name"] <- "Sheephead" # Archosargus probatocephalus, Lagodon romboides
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Crustaceans"),"scientific_name"] <- "Crustacea" # Clivanarius vittatus, decapod, amphipod
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "White mullet"),"scientific_name"] <- "Mugil curema"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Spadefish"),"scientific_name"] <- "Chaetodipterus faber"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Oyster"),"scientific_name"] <- "Crassostrea virginica"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Molluscs"),"scientific_name"] <- "Mollusca" # bivalves and gastropods
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Meiofauna"),"scientific_name"] <- "Meiofauna"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Benthis producers"),"scientific_name"] <- "Benthic producer"

# Web 100 - article OK
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "P maculatus"),"scientific_name"] <- "Pimelodus maculatus"
##Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hypostomus spp"),"scientific_name"] <- "Hypostomus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Schizodon borelii"),"scientific_name"] <- "Schizodon borelii"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Loricariichthys platymetopon"),"scientific_name"] <- "Loricariichthys platymetopon"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Prochilodus lineatus"),"scientific_name"] <- "Prochilodus lineatus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Cyphocharax modesta"),"scientific_name"] <- "Cyphocharax modesta"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Steindacchnerina insculpta"),"scientific_name"] <- "Steindachnerina insculpta"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Iheringichthys labrosus"),"scientific_name"] <- "Iheringichthys labrosus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Schizodon altoparanae"),"scientific_name"] <- "Schizodon altoparanae"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hypophthalmus edentatus"),"scientific_name"] <- "Hypophthalmus edentatus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Trachydoras paraguayensis"),"scientific_name"] <- "Trachydoras paraguayensis"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hoplosternum littorale"),"scientific_name"] <- "Hoplosternum littorale"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Leporinus friderici"),"scientific_name"] <- "Leporinus friderici"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Leporinus obtusidens"),"scientific_name"] <- "Leporinus obtusidens"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Parauchenipterus galeatus"),"scientific_name"] <- "Parauchenipterus galeatus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Pterodoras granulosus"),"scientific_name"] <- "Pterodoras granulosus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Astyanax altiparanae"),"scientific_name"] <- "Astyanax altiparanae"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Auchenipterus nuchalis"),"scientific_name"] <- "Auchenipterus nuchalis"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Brycon orbignyanus"),"scientific_name"] <- "Brycon orbignyanus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Acestrorhyncus lacustris"),"scientific_name"] <- "Acestrorhynchus lacustris"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hoplias malabaricus"),"scientific_name"] <- "Hoplias malabaricus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Plagioscion squamisissimus"),"scientific_name"] <- "Plagioscion squamisissimus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Pseudoplatystoma corruscans"),"scientific_name"] <- "Pseudoplatystoma corruscans"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Rhaphiodon vulpinis"),"scientific_name"] <- "Rhaphiodon vulpinis"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Serrasalmus marginatus"),"scientific_name"] <- "Serrasalmus marginatus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hemisorubin platyrhynchos"),"scientific_name"] <- "Hemisorubim platyrhynchos"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Salminus brasiliensis"),"scientific_name"] <- "Salminus brasiliensis"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Serrasalmus spiloptera"),"scientific_name"] <- "Serrasalmus spiloptera"

# Web 107 - article OK
#Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Span. mackerel"),"scientific_name"] <- "Scomberomorus maculatus"
#Group_name[[107]][which(Group_name[[107]][,"original_name"] == "King mackerel"),"scientific_name"] <- "Scomberomorus cavalla"
#Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Red grouper"),"scientific_name"] <- "Epinephelus morio"
#Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Red snapper"),"scientific_name"] <- "Lutjanus campechanus"
#Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
#Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 108 - article OK
#Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Grl. Halibut"),"scientific_name"] <- "Reinhardtius hippoglossoides"
##Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Red fish"),"scientific_name"] <- "Sebastes"
#Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Northern shrimp"),"scientific_name"] <- "Pandalus borealis"
#Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Polar cod"),"scientific_name"] <- "Boreogadus suida"
#Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Cod"),"scientific_name"] <- "Gadus morhua"
#Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
#Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 109 - article OK
#Group_name[[109]][which(Group_name[[109]][,"original_name"] == "Red grouper"),"scientific_name"] <- "Epinephelus morio"
#Group_name[[109]][which(Group_name[[109]][,"original_name"] == "King mackerel"),"scientific_name"] <- "Scomberomorus cavalla"
#Group_name[[109]][which(Group_name[[109]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
#Group_name[[109]][which(Group_name[[109]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

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

# Write the list as a .RDS file
saveRDS(Group_name, file = "data/intermediate/Group_name.RDS")
