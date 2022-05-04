# Script to format the 116 network data from Jacquet et al. 2019
# Only keep the interactions that are between species in all the 116 networks

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
load("data/raw/ecopath/data/Group_name.Rdata")

# Transform each list of names into a DataFrame
# Add an empty column to each DataFrame
# original_name is the name initially found in the 116 Ecopath models
# scientific_name are the names that are retrieved from
# each article related to the model
Group_name <- lapply(Group_name, function(x) as.data.frame(x)) |>
             lapply(function(x) tibble::add_column(x, scientific_name = NA)) |>
             lapply(function(x) dplyr::rename(x, original_name = x))


###############################################################################
# Have to modify a lot of names by hand, because there are some for example
# that are written as "herring" when in the original article it was 
# "adult pacific herring" which can be linked to a unique scientific name 
# to retrieve a body mass!
###############################################################################

# Foodweb by foodweb, changing species names.
# Based on correspondance with original article

# Web 1 - article OK
        # Juvenile pacific herring < 18cm
Group_name[[1]][which(Group_name[[1]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea pallasii"
Group_name[[1]][which(Group_name[[1]][,"original_name"] == "Sea otters"),"scientific_name"] <- "Enhydra lutris"
Group_name[[1]][which(Group_name[[1]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[1]][which(Group_name[[1]][,"original_name"] == "Macrozooplankton"),"scientific_name"] <- "Macrozooplankton"

# Web 2 - article OK
Group_name[[2]][which(Group_name[[2]][,"original_name"] == "Totoaba"),"scientific_name"] <- "Totoaba macdonaldi"
Group_name[[2]][which(Group_name[[2]][,"original_name"] == "Vaquita"),"scientific_name"] <- "Phocoena sinus"
Group_name[[2]][which(Group_name[[2]][,"original_name"] == "Lobo marino"),"scientific_name"] <- "Zalophus californianus"
Group_name[[2]][which(Group_name[[2]][,"original_name"] == "C. de roca"),"scientific_name"] <- "Sicyonia penicillata"
Group_name[[2]][which(Group_name[[2]][,"original_name"] == "C. cafe"),"scientific_name"] <- "Farfantepenaeus californiensis"
Group_name[[2]][which(Group_name[[2]][,"original_name"] == "C. azul"),"scientific_name"] <- "Litopenaeus stylirostris"
Group_name[[2]][which(Group_name[[2]][,"original_name"] == "Zooplancton"),"scientific_name"] <- "Zooplankton"
Group_name[[2]][which(Group_name[[2]][,"original_name"] == "Fitoplancton"),"scientific_name"] <- "Phytoplankton"

# Web 3 - article MISSING
        
# Web 4 - article OK
Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Lemuru"),"scientific_name"] <- "Sardinella lemuru"
#Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Scads"),"scientific_name"] <- "Decapterus"
Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[4]][which(Group_name[[4]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 5 - article OK
        # Only functionnal groups

# Web 6 - article OK
Group_name[[6]][which(Group_name[[6]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Temora longicornis"
Group_name[[6]][which(Group_name[[6]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 7 - article OK
        # Only functionnal groups

# Web 8 - article OK
        # Only functionnal groups

# Web 9 - article OK
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
#Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Copepoda"),"scientific_name"] <- "Calanoid"
#Group_name[[9]][which(Group_name[[9]][,"original_name"] == "Worms"),"scientific_name"] <- "Nereidae"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "O. chrysoptera"),"scientific_name"] <- "Orthopristis chrysoptera"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "L. rhomboides"),"scientific_name"] <- "Lagodon rhomboides"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "O. oglinum"),"scientific_name"] <- "Opisthonema oglinum"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "H. jaguana"),"scientific_name"] <- "Harengula jaguana"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "A. rhomboidalis"),"scientific_name"] <- "Archosargus rhomboidalis"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "H. plumieri"),"scientific_name"] <- "Haemulon plumieri"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "B. chrysoura"),"scientific_name"] <- "Bairdiella chrysoura"
Group_name[[9]][which(Group_name[[9]][,"original_name"] == "A. quadricornis"),"scientific_name"] <- "Acanthostracion quadricornis"

# Web 10 - article OK
        # Only functionnal groups
        
# Web 11 - article OK
Group_name[[11]][which(Group_name[[11]][,"original_name"] == "Acanthocybium s"),"scientific_name"] <- "Acanthocybium solandri"
Group_name[[11]][which(Group_name[[11]][,"original_name"] == "Sea Turtles"),"scientific_name"] <- "Caretta caretta"
Group_name[[11]][which(Group_name[[11]][,"original_name"] == "Katsuwonus pela"),"scientific_name"] <- "Katsuwonus pelamis"
Group_name[[11]][which(Group_name[[11]][,"original_name"] == "Thunnus albacar"),"scientific_name"] <- "Thunnus albacares"
Group_name[[11]][which(Group_name[[11]][,"original_name"] == "Small tuna"),"scientific_name"] <- "Euthynnus alletteratus"
Group_name[[11]][which(Group_name[[11]][,"original_name"] == "Decapterus maca"),"scientific_name"] <- "Decapterus macarellus"
Group_name[[11]][which(Group_name[[11]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[11]][which(Group_name[[11]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 12 - article OK
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Minke whales"),"scientific_name"] <- "Balaenoptera acutorostrata"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Fin whales"),"scientific_name"] <- "Balaenoptera physalus"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Humpback whales"),"scientific_name"] <- "Megaptera novaeangliae"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Brydes whales"),"scientific_name"] <- "Balaenoptera edeni"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Sei whales"),"scientific_name"] <- "Balaenoptera borealis"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Blue whales"),"scientific_name"] <- "Balaenoptera musculus"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Sea turtles"),"scientific_name"] <- "Dermochelys coriacea"
Group_name[[12]][which(Group_name[[12]][,"original_name"] == "Dolphinfish"),"scientific_name"] <- "Coryphaena hippurus"

# Web 13 - article OK
        # Only functionnal groups

# Web 14 - article OK
Group_name[[14]][which(Group_name[[14]][,"original_name"] == "Yellowfin"),"scientific_name"] <- "Thunnus albacares"
Group_name[[14]][which(Group_name[[14]][,"original_name"] == "Bluefin"),"scientific_name"] <- "Thunnus thynnus"
Group_name[[14]][which(Group_name[[14]][,"original_name"] == "Skipjack"),"scientific_name"] <- "Katsuwonus pelamis"
Group_name[[14]][which(Group_name[[14]][,"original_name"] == "Albacore"),"scientific_name"] <- "Thunnus alalunga"
Group_name[[14]][which(Group_name[[14]][,"original_name"] == "Bigeye"),"scientific_name"] <- "Thunnus obesus"
Group_name[[14]][which(Group_name[[14]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"

# Web 15 - article OK
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Sea lion"),"scientific_name"] <- "Otaria flavescens"
#Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Chilean hake (j)"),"scientific_name"] <- "Merluccius gayi" # age 0-3
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Chilean hake (a)"),"scientific_name"] <- "Merluccius gayi" # age 0-3
#Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Pilchard (j)"),"scientific_name"] <- "Strangomera bentincki" # age 0
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Pilchard (a)"),"scientific_name"] <- "Strangomera bentincki" # age 1+
#Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Anchovy (j)"),"scientific_name"] <- "Engraulis ringens" # age 0
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Anchovy (a)"),"scientific_name"] <- "Engraulis ringens" # age 1+
#Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Carrot prawn (j)"),"scientific_name"] <- "Pleuroncodes monodon" # age 0
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Carrot prawn (a)"),"scientific_name"] <- "Pleuroncodes monodon" # age 1+
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Yellow prawn"),"scientific_name"] <- "Cervimunida johni"
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Horse mackerel"),"scientific_name"] <- "Trachurus symmetricus murphyi"
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Black conger"),"scientific_name"] <- "Genypterus maculatus"
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Rattail fish"),"scientific_name"] <- "Coelorinchus aconcagua"
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Big0eye flounder"),"scientific_name"] <- "Hippoglossina macrops"
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Cardinal fish"),"scientific_name"] <- "Epigonus crassicaudus"
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Pacific sand perch"),"scientific_name"] <- "Prolatilus jugularis"
#Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Raja"),"scientific_name"] <- "Raja"
#Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Copepods"),"scientific_name"] <- "Copepods"
#Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Euphausiids"),"scientific_name"] <- "Euphausiids"
Group_name[[15]][which(Group_name[[15]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 16 - article OK
        # Only functionnal groups

# Web 17 - article OK
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Blue marlin"),"scientific_name"] <- "Makaira mazara"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Blue shark"),"scientific_name"] <- "Prionace glauca"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Yellowfin tuna"),"scientific_name"] <- "Thunnus albacares"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Bigeye tuna"),"scientific_name"] <- "Thunnus obesus"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Albacore tuna"),"scientific_name"] <- "Thunnus alalunga"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Mahimahi"),"scientific_name"] <- "Coryphaena hippurus"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Skipjack tuna"),"scientific_name"] <- "Katsuwonus pelamis"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Lancetfish"),"scientific_name"] <- "Alepisaurus ferox"
Group_name[[17]][which(Group_name[[17]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 18 - article OK
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Spot"),"scientific_name"] <- "Leiostomus xanthurus"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Striped bass YOY"),"scientific_name"] <- "Morone saxatilis" # young of the year
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Striped bass resident"),"scientific_name"] <- "Morone saxatilis"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Striped bass migratory"),"scientific_name"] <- "Morone saxatilis"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Bluefish YOY"),"scientific_name"] <- "Pomatomus saltatrix" # young of the year
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Bluefish adult"),"scientific_name"] <- "Pomatomus saltatrix"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Weakfish YOY"),"scientific_name"] <- "Cynoscion regalis"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Weakfish Adult"),"scientific_name"] <- "Cynoscion regalis"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Summer flounder"),"scientific_name"] <- "Paralichthys dentatus"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Atl. croaker"),"scientific_name"] <- "Micropogonias undulatus"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Menhaden YOY"),"scientific_name"] <- "Brevoortia tyrannus"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Menhaden adult"),"scientific_name"] <- "Brevoortia tyrannus"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "black drum"),"scientific_name"] <- "Pogonias cromis"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Alewife and herring"),"scientific_name"] <- "Alosa"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "American eel"),"scientific_name"] <- "Anguilla rostrata"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "American shad"),"scientific_name"] <- "Alosa sapidissima" # american/hickory shad?
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "bay anchovy"),"scientific_name"] <- "Anchoa mitchilli"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "White perch YOY"),"scientific_name"] <- "Morone Americana"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "White perch adult"),"scientific_name"] <- "Morone americana"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "gizzard shad"),"scientific_name"] <- "Dorosoma cepedianum"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "blue catfish"),"scientific_name"] <- "Ictalurus furcatus"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "sandbar shark"),"scientific_name"] <- "Carcharhinus plumbeus"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Hard clam"),"scientific_name"] <- "Mercenaria mercenaria"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "ctenophores"),"scientific_name"] <- "Mnemiopsis"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "sea nettles"),"scientific_name"] <- "Chrysaora quinquecirrha"
#Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Blue crab YOY"),"scientific_name"] <- "Callinectes sapidus"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Blue crab adult"),"scientific_name"] <- "Callinectes sapidus"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "oyster"),"scientific_name"] <- "Crassostrea virginica"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "soft clam"),"scientific_name"] <- "Mya arenaria"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Mesozooplankton"),"scientific_name"] <- "Mesozooplankton"
Group_name[[18]][which(Group_name[[18]][,"original_name"] == "Microzooplankton"),"scientific_name"] <- "Microzooplankton"

# Web 19 - article MISSING

# Web 20 - article MISSING

# Web 21 - article OK
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Spotted Dolphin"),"scientific_name"] <- "Stenella attenuata"
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Lg Yellowfin"),"scientific_name"] <- "Thunnus albacares" # >=90cm
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Lg Bigeye"),"scientific_name"] <- "Thunnus obesus" # >=80cm
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Lg Sailfish"),"scientific_name"] <- "Istiophorus platypterus" #>=150cm
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Lg Swordfish"),"scientific_name"] <- "Xiphias gladus" # >=150cm
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Lg Wahoo"),"scientific_name"] <- "Acanthocybium solandri" # >=90cm
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Rays"),"scientific_name"] <- "Manta birostris"
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Skipjack"),"scientific_name"] <- "Katsuwonus pelamis"
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Albacore"),"scientific_name"] <- "Thunnus alalunga"
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Bluefin"),"scientific_name"] <- "Thunnus orientalis"
#Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Sm Yellowfin"),"scientific_name"] <- "Thunnus albacares" # <90cm
#Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Sm Bigeye"),"scientific_name"] <- "Thunnus obesus" # <80cm
#Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Sm Sailfish"),"scientific_name"] <- "Istiophorus platypterus" # <150cm
#Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Sm Swordfish"),"scientific_name"] <- "Xiphias gladus" #< 150cm
#Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Sm Wahoo"),"scientific_name"] <- "Acanthocybium solandri" # <90cm
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Lg Phytoplankton"),"scientific_name"] <- "Large Phytoplankton"
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Microzooplankton"),"scientific_name"] <- "Microzooplankton"
Group_name[[21]][which(Group_name[[21]][,"original_name"] == "Mesozooplankton"),"scientific_name"] <- "Mesozooplankton"

# Web 22 - article OK
Group_name[[22]][which(Group_name[[22]][,"original_name"] == "Anguille"),"scientific_name"] <- "Anguilla anguilla"
#Group_name[[22]][which(Group_name[[22]][,"original_name"] == "Adult mugilids"),"scientific_name"] <- "Muguilidae"
#Group_name[[22]][which(Group_name[[22]][,"original_name"] == "Atherinids"),"scientific_name"] <- "Atherinidae"
Group_name[[22]][which(Group_name[[22]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[22]][which(Group_name[[22]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"


# Web 23 - article MISSING

# Web 24 - article MISSING

# Web 25 - article MISSING

# Web 26 - article MISSING

# Web 27 - article OK
        # Only functionnal groups

# Web 28 - article OK
        # À REVOIR

# Web 29 - article MISSING

# Web 30 - article OK
        # Only functionnal groups

# Web 31 - article OK
Group_name[[31]][which(Group_name[[31]][,"original_name"] == "Epifauna"),"scientific_name"] <- "Panulirus argus"
Group_name[[31]][which(Group_name[[31]][,"original_name"] == "Shrimps"),"scientific_name"] <- "Xiphopenaeus kroyeri"
Group_name[[31]][which(Group_name[[31]][,"original_name"] == "Small pelagic fish"),"scientific_name"] <- "Cetengraulis edentulus"
Group_name[[31]][which(Group_name[[31]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[31]][which(Group_name[[31]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 32 - a2ticle O2
Group_name[[32]][which(Group_name[[32]][,"original_name"] == "Adult capelin (2+)"),"scientific_name"] <- "Mallotus villosus"
Group_name[[32]][which(Group_name[[32]][,"original_name"] == "Polar cod (1+)"),"scientific_name"] <- "Boreogadus saida"
Group_name[[32]][which(Group_name[[32]][,"original_name"] == "Adult cod (4+)"),"scientific_name"] <- "Gadus morhua"
Group_name[[32]][which(Group_name[[32]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[32]][which(Group_name[[32]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 33 - article OK
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Adult saithe"),"scientific_name"] <- "Pollachius virens"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Adult cod (4+)"),"scientific_name"] <- "Gadus morhua"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Minke"),"scientific_name"] <- "Balaenoptera acutorostrata"
#Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Juvenile saithe"),"scientific_name"] <- "Pollachius virens"
#Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Juvenile cod (1-3)"),"scientific_name"] <- "Gadus morhua"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Adult haddock (4+)"),"scientific_name"] <- "Melanogrammus aeglefinus"
#Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Juvenile haddock (1-3)"),"scientific_name"] <- "Melanogrammus aeglefinus"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Polar cod (1+)"),"scientific_name"] <- "Boreogadus saida"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Adult capelin (2+)"),"scientific_name"] <- "Mallotus villosus"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Squid"),"scientific_name"] <- "Gonatus fabricii"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Adult herring (4+)"),"scientific_name"] <- "Clupea harengus"
#Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Juvenile capelin (1)"),"scientific_name"] <- "Mallotus villosus"
#Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Juvenile herring (1-3)"),"scientific_name"] <- "Clupea harengus"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Prawns & shrimps"),"scientific_name"] <- "Pandalus borealis"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Herbivorous zooplankton"),"scientific_name"] <- "Herbivorous zooplankton"
Group_name[[33]][which(Group_name[[33]][,"original_name"] == "Carnivorous zooplankton"),"scientific_name"] <- "Carnivorous zooplankton"

# Web 34 - article OK
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Sciaenidae"),"scientific_name"] <- "Cynoscion xanthulum"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Elopidae"),"scientific_name"] <- "Elops affinis"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Carangidae"),"scientific_name"] <- "Caranx hippos"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Centropomidae"),"scientific_name"] <- "Centropomus robalito"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Ariidae"),"scientific_name"] <- "Arius guatemalensis"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Haemulidae"),"scientific_name"] <- "Anisotremus interreptus"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Callinectes"),"scientific_name"] <- "Callinectes arcuatus"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Gerreidae"),"scientific_name"] <- "Diapterus peruvians"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Gobios"),"scientific_name"] <- "Dormitator latrifons"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Mugilidae"),"scientific_name"] <- "Mugil cephalus"
#Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Palaemonidae"),"scientific_name"] <- "Macrobranchium"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Chanidae"),"scientific_name"] <- "Chanos chanos"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Fitoplancton"),"scientific_name"] <- "Phytoplankton"
Group_name[[34]][which(Group_name[[34]][,"original_name"] == "Zooplancton"),"scientific_name"] <- "Zooplankton"

# Web 35 - article Ok
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Cod"),"scientific_name"] <- "Gadus morhua" # age 3-14
#Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Juvenile cod"),"scientific_name"] <- "Gadus morhua" # age 0-2
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Haddock"),"scientific_name"] <- "Melanogrammus aeglefinus"
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Saithe"),"scientific_name"] <- "Pollachius virens"
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Greenland halibut"),"scientific_name"] <- "Reinhardtius hippoglossoides"
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea harengus"
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Nephrops"),"scientific_name"] <- "Nephrops norvegicus"
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Northern shrimps"),"scientific_name"] <- "Pandalus borealis"
# Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Nekton"),"scientific_name"] <- "Krill" # See article for generic values
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[35]][which(Group_name[[35]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 36 - article OK
Group_name[[36]][which(Group_name[[36]][,"original_name"] == "Dorado"),"scientific_name"] <- "Coryphaena hippurus"
Group_name[[36]][which(Group_name[[36]][,"original_name"] == "Sierra"),"scientific_name"] <- "Scomberomorus sierra"
Group_name[[36]][which(Group_name[[36]][,"original_name"] == "Esc\xf3mbridos"),"scientific_name"] <- "Katsuwonus pelamis" # Otros escombridos
Group_name[[36]][which(Group_name[[36]][,"original_name"] == "Adultos Lutjanu"),"scientific_name"] <- "Lutjanus peru"
#Group_name[[36]][which(Group_name[[36]][,"original_name"] == "Juveniles Lutja"),"scientific_name"] <- "Lutjanus peru"
Group_name[[36]][which(Group_name[[36]][,"original_name"] == "Pulpo"),"scientific_name"] <- "Octopus hubbsorum"
Group_name[[36]][which(Group_name[[36]][,"original_name"] == "Zooplancton"),"scientific_name"] <- "Zooplankton"
Group_name[[36]][which(Group_name[[36]][,"original_name"] == "Fitoplancton"),"scientific_name"] <- "Phytoplankton"

# Web 37 - article OK
        # Only functionnal groups

# Web 38 - article OK
        # Only functionnal groups

# Web 39 - article OK
Group_name[[39]][which(Group_name[[39]][,"original_name"] == "Phyt"),"scientific_name"] <- "Phytoplankton"
Group_name[[39]][which(Group_name[[39]][,"original_name"] == "Bakt"),"scientific_name"] <- "Bakterioplankton"
#Group_name[[39]][which(Group_name[[39]][,"original_name"] == "Moff"),"scientific_name"] <- "Bivalvia"
Group_name[[39]][which(Group_name[[39]][,"original_name"] == "Zoopl"),"scientific_name"] <- "Zooplankton"
#Group_name[[39]][which(Group_name[[39]][,"original_name"] == "Momf"),"scientific_name"] <- "Actinia"

# Web 40 - article OK
 # Waiting on Mario Delos Reyes

# Web 41 - article OK
 # Waiting on Mario Delos Reyes

# Web 42 - article OK
 # Waiting on Mario Delos Reyes

# Web 43 - article OK
#Group_name[[43]][which(Group_name[[43]][,"original_name"] == "Roach fry"),"scientific_name"] <- "Rutilus rutilus"
Group_name[[43]][which(Group_name[[43]][,"original_name"] == "Roach adults"),"scientific_name"] <- "Rutilus rutilus"
Group_name[[43]][which(Group_name[[43]][,"original_name"] == "phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[43]][which(Group_name[[43]][,"original_name"] == "predatory zoo"),"scientific_name"] <- "Predatory zooplankton"
Group_name[[43]][which(Group_name[[43]][,"original_name"] == "herbivorous zoo"),"scientific_name"] <- "Herbivorous zooplankton"

# Web 44 - article OK
Group_name[[44]][which(Group_name[[44]][,"original_name"] == "Sarotherodon"),"scientific_name"] <- "Sarotherodon galilaeus"
Group_name[[44]][which(Group_name[[44]][,"original_name"] == "Alestes macro"),"scientific_name"] <- "Alestes macrolepidotus"
Group_name[[44]][which(Group_name[[44]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[44]][which(Group_name[[44]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 45 - article OK
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "H. squamipinnis"),"scientific_name"] <- "Haplochromis squamipinnis"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Protopterus"),"scientific_name"] <- "Protopterus aethiopicus"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Clarias gariopi"),"scientific_name"] <- "Clarias gariepinus"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "H. angustifrons"),"scientific_name"] <- "Haplochromis angustifrons"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "H. nigripinnis"),"scientific_name"] <- "Haplochromis nigripinnis"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "O. niloticus"),"scientific_name"] <- "Oreochromis niloticus"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "O. leucostictus"),"scientific_name"] <- "Oreochromis leucostictus"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[45]][which(Group_name[[45]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 46 - article OK
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "H. vittatus"),"scientific_name"] <- "Hydrocynus vittatus"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "L. miodon"),"scientific_name"] <- "Limnothrissa miodon"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "S. zambezensis"),"scientific_name"] <- "Synodontis zambezensis"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[46]][which(Group_name[[46]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 47 - article OK
Group_name[[47]][which(Group_name[[47]][,"original_name"] == "Lavnun"),"scientific_name"] <- "Mirogrex terraesanctae"
Group_name[[47]][which(Group_name[[47]][,"original_name"] == "Silver carp"),"scientific_name"] <- "Hypophtalmichthys molitrix"
Group_name[[47]][which(Group_name[[47]][,"original_name"] == "Piscivores"),"scientific_name"] <- "Clarias lazera"
Group_name[[47]][which(Group_name[[47]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[47]][which(Group_name[[47]][,"original_name"] == "Herb. zoop."),"scientific_name"] <- "Herbivorous zooplankton"
Group_name[[47]][which(Group_name[[47]][,"original_name"] == "Carn. zoop."),"scientific_name"] <- "Carnivorous zooplankton"

# Web 48 - article OK
Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Kampango"),"scientific_name"] <- "Bagrus meridionalis"
Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Mpasa"),"scientific_name"] <- "Opsaridium microlepis"
Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Sanjika"),"scientific_name"] <- "Opsaridium microcephalus"
#Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Mcheni"),"scientific_name"] <- "Ramphochromis"
#Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Bombe"),"scientific_name"] <- "Bathyclarias"
Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Usipa"),"scientific_name"] <- "Engraulicypris sardella"
Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Nkhungu"),"scientific_name"] <- "Chaoborus edulis"
Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[48]][which(Group_name[[48]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 49 - article OK
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Opsaridium"),"scientific_name"] <- "Opsaridium microcephalus"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Rhamphochromis"),"scientific_name"] <- "Rhamphochromis longiceps"
#Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Diplotaxodon"),"scientific_name"] <- "Diplotaxodon pallidorsalis"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Engraulicypris"),"scientific_name"] <- "Engraulicypris sardella"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Chaoborus"),"scientific_name"] <- "Chaoborus edulis"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[49]][which(Group_name[[49]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 50 - article OK
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Limnothrissa"),"scientific_name"] <- "Limnothrissa miodon"
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Stolothrissa"),"scientific_name"] <- "Stolothrissa tanganyicae"
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[50]][which(Group_name[[50]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 51 - article OK
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Limnothrissa"),"scientific_name"] <- "Limnothrissa miodon"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Stolothrissa"),"scientific_name"] <- "Stolothrissa tanganyicae"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[51]][which(Group_name[[51]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 52 - article OK
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Synod. schall"),"scientific_name"] <- "Synodontis schall"
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[52]][which(Group_name[[52]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 53 - article OK
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Synod. schall"),"scientific_name"] <- "Synodontis schall"
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[53]][which(Group_name[[53]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 54 - article OK
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Nile perch"),"scientific_name"] <- "Lates niloticus"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Lungfish"),"scientific_name"] <- "Protopterus aethiopicus"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Dagaa"),"scientific_name"] <- "Rastrineobola argentea"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Lake prawn"),"scientific_name"] <- "Caridina nilotica"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Nile tilapia"),"scientific_name"] <- "Oreochromis niloticus"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[54]][which(Group_name[[54]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 55 - article OK
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Nile perch"),"scientific_name"] <- "Lates niloticus"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Lungfish"),"scientific_name"] <- "Protopterus aethiopicus"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Dagaa"),"scientific_name"] <- "Rastrineobola argentea"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Lake prawn"),"scientific_name"] <- "Caridina nilotica"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Nile tilapia"),"scientific_name"] <- "Oreochromis niloticus"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[55]][which(Group_name[[55]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 56 - article OK
        # Only functionnal groups

# Web 57 - article OK
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Adult saithe"),"scientific_name"] <- "Pollachius virens"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Adult cod (4+)"),"scientific_name"] <- "Gadus morhua"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Minke"),"scientific_name"] <- "Balaenoptera acutorostrata"
#Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Juvenile saithe"),"scientific_name"] <- "Pollachius virens"
#Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Juvenile cod (1-3)"),"scientific_name"] <- "Gadus morhua"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Adult haddock (4+)"),"scientific_name"] <- "Melanogrammus aeglefinus"
#Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Juvenile haddock (1-3)"),"scientific_name"] <- "Melanogrammus aeglefinus"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Polar cod (1+)"),"scientific_name"] <- "Boreogadus saida"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Adult capelin (2+)"),"scientific_name"] <- "Mallotus villosus"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Squid"),"scientific_name"] <- "Gonatus fabricii"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Adult herring (4+)"),"scientific_name"] <- "Clupea harengus"
#Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Juvenile capelin (1)"),"scientific_name"] <- "Mallotus villosus"
#Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Juvenile herring (1-3)"),"scientific_name"] <- "Clupea harengus"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Prawns & shrimps"),"scientific_name"] <- "Pandalus borealis"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Herbivorous zooplankton"),"scientific_name"] <- "Herbivorous zooplankton"
Group_name[[57]][which(Group_name[[57]][,"original_name"] == "Carnivorous zooplankton"),"scientific_name"] <- "Carnivorous zooplankton"

# Web 58 - article OK
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Adult saithe"),"scientific_name"] <- "Pollachius virens"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Adult cod (4+)"),"scientific_name"] <- "Gadus morhua"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Minke"),"scientific_name"] <- "Balaenoptera acutorostrata"
#Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Juvenile saithe"),"scientific_name"] <- "Pollachius virens"
#Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Juvenile cod (1-3)"),"scientific_name"] <- "Gadus morhua"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Adult haddock (4+)"),"scientific_name"] <- "Melanogrammus aeglefinus"
#Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Juvenile haddock (1-3)"),"scientific_name"] <- "Melanogrammus aeglefinus"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Polar cod (1+)"),"scientific_name"] <- "Boreogadus saida"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Adult capelin (2+)"),"scientific_name"] <- "Mallotus villosus"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Squid"),"scientific_name"] <- "Gonatus fabricii"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Adult herring (4+)"),"scientific_name"] <- "Clupea harengus"
#Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Juvenile capelin (1)"),"scientific_name"] <- "Mallotus villosus"
#Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Juvenile herring (1-3)"),"scientific_name"] <- "Clupea harengus"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Prawns & shrimps"),"scientific_name"] <- "Pandalus borealis"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
Group_name[[58]][which(Group_name[[58]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"

# Web 59 - article OK
        # Only functionnal groups

# Web 60 - article OK
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Sharks"),"scientific_name"] <- "Carcharhinus melanopterus"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[60]][which(Group_name[[60]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 61 - article OK
        # Only functionnal groups

# Web 62 - article OK
        # Only functionnal groups

# Web 63 - article OK
        # Only functionnal groups

# Web 64 - article OK
        # Only functionnal groups
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Grey seals"),"scientific_name"] <- "Halichoerus grypus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Harp Seals"),"scientific_name"] <- "Pagophilus groenlandicus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Hooded Seals"),"scientific_name"] <- "Cystophora cristata"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Cod + 40cm"),"scientific_name"] <- "Gadus morhua" # > 40cm
#Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Cod  <40 cm"),"scientific_name"] <- "Gadus morhua" # < 40cm
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Aplaice +35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # >  35cm
#Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Aplaice<=35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # <= 35cm
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "G. Halibut +65cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
#Group_name[[66]][which(Group_name[[66]][,"original_name"] == "G. Halibut Juv."),"scientific_name"] <- "Reinhardtius hippoglossoides"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Yellowtail Flounders "),"scientific_name"] <- "Limanda ferruginea"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Witch flounder"),"scientific_name"] <- "Glyptocephalus cynoglossus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Winter flounder"),"scientific_name"] <- "Pseudopleuronectes americanus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Transient Mackerel ( +29cm)"),"scientific_name"] <- "Scomber scombrus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Lumpfish"),"scientific_name"] <- "Cyclopterus lumpus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Greenland cod"),"scientific_name"] <- "Gadus opac"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Salmon"),"scientific_name"] <- "Salmo salar"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Sandlance"),"scientific_name"] <- "Ammodytes dubius"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Arctic cod"),"scientific_name"] <- "Boreogadus saida"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea harengus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Shortfinned squid"),"scientific_name"] <- "Illex illecebrosus"
#Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Arctic Squid"),"scientific_name"] <- "Gonatus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Lobster"),"scientific_name"] <- "Homarus americanus"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "L.Zooplankton"),"scientific_name"] <- "Large Zooplankton"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "S.Zooplankton"),"scientific_name"] <- "Small Zooplankton"
Group_name[[66]][which(Group_name[[66]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 67 - article OK
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Harp Seals"),"scientific_name"] <- "Pagophilus groenlandicus"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Hooded Seals"),"scientific_name"] <- "Cystophora cristata"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Cod > 35cm"),"scientific_name"] <- "Gadus morhua"
#Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Cod <= 35 cm"),"scientific_name"] <- "Gadus morhua"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "G.halibut>40cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
#Group_name[[67]][which(Group_name[[67]][,"original_name"] == "G.halibut<=40cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Aplaice>35cm"),"scientific_name"] <- "Hippoglossoides platessoides"
#Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Aplaice<=35cm"),"scientific_name"] <- "Hippoglossoides platessoides"
#Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Redfish"),"scientific_name"] <- "Sebastes"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Sandlance"),"scientific_name"] <- "Ammodytes dubius"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Arctic cod"),"scientific_name"] <- "Boreogadus saida"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "L.Zooplankton"),"scientific_name"] <- "Large Zooplankton"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "S.Zooplankton"),"scientific_name"] <- "Small Zooplankton"
Group_name[[67]][which(Group_name[[67]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 68 - article OK
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Grey seals"),"scientific_name"] <- "Halichoerus grypus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Harp Seals"),"scientific_name"] <- "Pagophilus groenlandicus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Hooded Seals"),"scientific_name"] <- "Cystophora cristata"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Cod + 40cm"),"scientific_name"] <- "Gadus morhua" # > 40cm
#Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Cod  <40 cm"),"scientific_name"] <- "Gadus morhua" # < 40cm
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Aplaice +35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # >  35cm
#Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Aplaice<=35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # <= 35cm
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "G. Halibut +65cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
#Group_name[[68]][which(Group_name[[68]][,"original_name"] == "G. Halibut Juv."),"scientific_name"] <- "Reinhardtius hippoglossoides"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Yellowtail Flounders "),"scientific_name"] <- "Limanda ferruginea"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Witch flounder"),"scientific_name"] <- "Glyptocephalus cynoglossus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Winter flounder"),"scientific_name"] <- "Pseudopleuronectes americanus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Transient Mackerel ( +29cm)"),"scientific_name"] <- "Scomber scombrus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Lumpfish"),"scientific_name"] <- "Cyclopterus lumpus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Greenland cod"),"scientific_name"] <- "Gadus opac"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Salmon"),"scientific_name"] <- "Salmo salar"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Sandlance"),"scientific_name"] <- "Ammodytes dubius"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Arctic cod"),"scientific_name"] <- "Boreogadus saida"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea harengus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Shortfinned squid"),"scientific_name"] <- "Illex illecebrosus"
#Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Arctic Squid"),"scientific_name"] <- "Gonatus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Lobster"),"scientific_name"] <- "Homarus americanus"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "L.Zooplankton"),"scientific_name"] <- "Large Zooplankton"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "S.Zooplankton"),"scientific_name"] <- "Small Zooplankton"
Group_name[[68]][which(Group_name[[68]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 69 - article OK
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Grey seals"),"scientific_name"] <- "Halichoerus grypus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Harp Seals"),"scientific_name"] <- "Pagophilus groenlandicus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Hooded Seals"),"scientific_name"] <- "Cystophora cristata"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Cod + 40cm"),"scientific_name"] <- "Gadus morhua" # > 40cm
#Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Cod  <40 cm"),"scientific_name"] <- "Gadus morhua" # < 40cm
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Aplaice +35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # >  35cm
#Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Aplaice<=35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # <= 35cm
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "G. Halibut +65cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
#Group_name[[69]][which(Group_name[[69]][,"original_name"] == "G. Halibut Juv."),"scientific_name"] <- "Reinhardtius hippoglossoides"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Yellowtail Flounders "),"scientific_name"] <- "Limanda ferruginea"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Witch flounder"),"scientific_name"] <- "Glyptocephalus cynoglossus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Winter flounder"),"scientific_name"] <- "Pseudopleuronectes americanus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Transient Mackerel ( +29cm)"),"scientific_name"] <- "Scomber scombrus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Lumpfish"),"scientific_name"] <- "Cyclopterus lumpus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Greenland cod"),"scientific_name"] <- "Gadus opac"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Salmon"),"scientific_name"] <- "Salmo salar"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Sandlance"),"scientific_name"] <- "Ammodytes dubius"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Arctic cod"),"scientific_name"] <- "Boreogadus saida"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea harengus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Shortfinned squid"),"scientific_name"] <- "Illex illecebrosus"
#Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Arctic Squid"),"scientific_name"] <- "Gonatus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Lobster"),"scientific_name"] <- "Homarus americanus"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "L.Zooplankton"),"scientific_name"] <- "Large Zooplankton"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "S.Zooplankton"),"scientific_name"] <- "Small Zooplankton"
Group_name[[69]][which(Group_name[[69]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 70 - article OK
Group_name[[70]][which(Group_name[[70]][,"original_name"] == "Yellowfin"),"scientific_name"] <- "Thunnus albacares"
Group_name[[70]][which(Group_name[[70]][,"original_name"] == "Bluefin"),"scientific_name"] <- "Thunnus thynnus"
Group_name[[70]][which(Group_name[[70]][,"original_name"] == "Skipjack"),"scientific_name"] <- "Katsuwonus pelamis"
Group_name[[70]][which(Group_name[[70]][,"original_name"] == "Albacore"),"scientific_name"] <- "Thunnus alalunga"
Group_name[[70]][which(Group_name[[70]][,"original_name"] == "Bigeye"),"scientific_name"] <- "Thunnus obesus"
Group_name[[70]][which(Group_name[[70]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"
Group_name[[70]][which(Group_name[[70]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 71 - article OK
Group_name[[71]][which(Group_name[[71]][,"original_name"] == "Yellowfin"),"scientific_name"] <- "Thunnus albacares"
Group_name[[71]][which(Group_name[[71]][,"original_name"] == "Bluefin"),"scientific_name"] <- "Thunnus thynnus"
Group_name[[71]][which(Group_name[[71]][,"original_name"] == "Skipjack"),"scientific_name"] <- "Katsuwonus pelamis"
Group_name[[71]][which(Group_name[[71]][,"original_name"] == "Albacore"),"scientific_name"] <- "Thunnus alalunga"
Group_name[[71]][which(Group_name[[71]][,"original_name"] == "Bigeye"),"scientific_name"] <- "Thunnus obesus"
Group_name[[71]][which(Group_name[[71]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"
Group_name[[71]][which(Group_name[[71]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 72 - article OK
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Minke whales"),"scientific_name"] <- "Balaenoptera acutorostrata"
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Fin whales"),"scientific_name"] <- "Balaenoptera physalus"
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Humpback whales"),"scientific_name"] <- "Megaptera novaeangliae"
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Brydes whales"),"scientific_name"] <- "Balaenoptera edeni"
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Sei whales"),"scientific_name"] <- "Balaenoptera borealis"
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Sperm whales"),"scientific_name"] <- "Physeter macrocephalus"
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Killer whales"),"scientific_name"] <- "Orcinus orca"
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[72]][which(Group_name[[72]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 73 - article OK
Group_name[[73]][which(Group_name[[73]][,"original_name"] == "Cormorant"),"scientific_name"] <- "Phalacrocorax carbo sinensis"
Group_name[[73]][which(Group_name[[73]][,"original_name"] == "Seabass"),"scientific_name"] <- "Dicentrarchus labrax"
Group_name[[73]][which(Group_name[[73]][,"original_name"] == "Eel"),"scientific_name"] <- "Anguilla anguilla"
Group_name[[73]][which(Group_name[[73]][,"original_name"] == "Seabeam"),"scientific_name"] <- "Sparus aurata"
Group_name[[73]][which(Group_name[[73]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[73]][which(Group_name[[73]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 74 - article OK
        # Only functionnal groups

# Web 75 - article MISSING

# Web 76 - article OK
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Sardine"),"scientific_name"] <- "Sardinops sagax"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Mackerel"),"scientific_name"] <- "Scomber japonicus"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Horse Mackerel"),"scientific_name"] <- "Trachurus murphyi"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Hake"),"scientific_name"] <- "Merluccius gayi"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Anchoveta"),"scientific_name"] <- "Engraulis ringens"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Sea lion"),"scientific_name"] <- "Otaria fluvescens"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Fur seal"),"scientific_name"] <- "Arctocephalus australis"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Bonito"),"scientific_name"] <- "Sarda chiliensis"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Booby"),"scientific_name"] <- "Sula variegata"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Cormorant"),"scientific_name"] <- "Phalacrocorax bougainvillii"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Pelican"),"scientific_name"] <- "Pelecanus thagus"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[76]][which(Group_name[[76]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 77 - article OK
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Sardine"),"scientific_name"] <- "Sardinops sagax"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Mackerel"),"scientific_name"] <- "Scomber japonicus"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Horse Mackerel"),"scientific_name"] <- "Trachurus murphyi"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Hake"),"scientific_name"] <- "Merluccius gayi"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Anchoveta"),"scientific_name"] <- "Engraulis ringens"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Sea lion"),"scientific_name"] <- "Otaria fluvescens"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Fur seal"),"scientific_name"] <- "Arctocephalus australis"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Bonito"),"scientific_name"] <- "Sarda chiliensis"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Booby"),"scientific_name"] <- "Sula variegata"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Cormorant"),"scientific_name"] <- "Phalacrocorax bougainvillii"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Pelican"),"scientific_name"] <- "Pelecanus thagus"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[77]][which(Group_name[[77]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 78 - article OK
Group_name[[78]][which(Group_name[[78]][,"original_name"] == "S. auratus"),"scientific_name"] <- "Sparus auratus"
Group_name[[78]][which(Group_name[[78]][,"original_name"] == "S. cantharus"),"scientific_name"] <- "Spondyliosoma cantarus"
Group_name[[78]][which(Group_name[[78]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[78]][which(Group_name[[78]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 79 - article OK
        # Only functionnal groups

# Web 80 - article OK
        # Only functionnal groups

# Web 81 - article OK
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Dicentrarchus labrax(fish)"),"scientific_name"] <- "Dicentrarchus labrax"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Pomatoschistus microps (suprabenthos)"),"scientific_name"] <- "Pomatoschistus microps"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Platichthys flesus (fish)"),"scientific_name"] <- "Platichthys flesus"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Crangon crangon (suprabenthos)"),"scientific_name"] <- "Crangon crangon"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Palaemon longirostris (suprabenthos)"),"scientific_name"] <- "Palaemon longirostris"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Neomysis integer (suprabenthos)"),"scientific_name"] <- "Neomysis integer"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[81]][which(Group_name[[81]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 82 - article OK
        # Only functionnal groups

# Web 83 - article OK
        # Only functionnal groups

# Web 84 - article OK
        # Only functionnal groups

# Web 85 - article OK
        # Only functionnal groups

# Web 86 - article OK3
        # Only functionnal groups

# Web 87 - article OK
# Group_name[[87]][which(Group_name[[87]][,"original_name"] == "Cutlassfish"),"scientific_name"] <- "Trichiurus lepturus"
# Group_name[[87]][which(Group_name[[87]][,"original_name"] == "Hake"),"scientific_name"] <- "Merluccius hubbsi"
# Group_name[[87]][which(Group_name[[87]][,"original_name"] == ""),"scientific_name"] <- ""
# Group_name[[87]][which(Group_name[[87]][,"original_name"] == ""),"scientific_name"] <- ""
# Group_name[[87]][which(Group_name[[87]][,"original_name"] == ""),"scientific_name"] <- ""

# Web 88 - article MISSING

# Web 89 - article MISSING

# Web 90 - article OK
        # Only functionnal groups

# Web 91 - article OK
Group_name[[91]][which(Group_name[[91]][,"original_name"] == "E Fluviatilis"),"scientific_name"] <- "Ehirava fluviatilis"
#Group_name[[91]][which(Group_name[[91]][,"original_name"] == "Hemiramphus sp"),"scientific_name"] <- "Hemiramphus"
Group_name[[91]][which(Group_name[[91]][,"original_name"] == "P.filamentosus"),"scientific_name"] <- "Puntius filamentosus"
Group_name[[91]][which(Group_name[[91]][,"original_name"] == "O. mossambicus"),"scientific_name"] <- "Oreochromis mossambicus"
Group_name[[91]][which(Group_name[[91]][,"original_name"] == "O. niloticus"),"scientific_name"] <- "Oreochromis niloticus"
Group_name[[91]][which(Group_name[[91]][,"original_name"] == "Amb.melettinus"),"scientific_name"] <- "Amblypharyngodon melettinus"

# Web 92 - article OK
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Halibut"),"scientific_name"] <- "Hippoglossus stenolepis"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Lingcod"),"scientific_name"] <- "Ophiodon elongatus"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Ad hake"),"scientific_name"] <- "Merluccius productus"
#Group_name[[92]][which(Group_name[[92]][,"original_name"] == "<juv hake"),"scientific_name"] <- "Merluccius productus" # juvenile hake
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Ad Res coho"),"scientific_name"] <- "Oncorhynchus kisutch"
#Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Juv Res coho"),"scientific_name"] <- "Oncorhynchus kisutch"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Ad Res chinook"),"scientific_name"] <- "Oncorhynchus tshawytscha"
#Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Juv Res chinook"),"scientific_name"] <- "Oncorhynchus tshawytscha"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Eulachon"),"scientific_name"] <- "Thaleichthys pacificus"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Ad herring"),"scientific_name"] <- "Clupea pallasii"
#Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Juv herring"),"scientific_name"] <- "Clupea pallasii"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Carn zooplankton"),"scientific_name"] <- "Carnivorous zooplankton"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Herb zooplankton"),"scientific_name"] <- "Herbivorous zooplankton"
Group_name[[92]][which(Group_name[[92]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 93 - article OK
        # Only functionnal groups

# Web 94 - article OK
Group_name[[94]][which(Group_name[[94]][,"original_name"] == "B. chrysoura"),"scientific_name"] <- "Bairdiella chrysoura"
Group_name[[94]][which(Group_name[[94]][,"original_name"] == "D. auratus"),"scientific_name"] <- "Diapterus auratus"
Group_name[[94]][which(Group_name[[94]][,"original_name"] == "M. curema"),"scientific_name"] <- "Mugil curema"
Group_name[[94]][which(Group_name[[94]][,"original_name"] == "O. saurus"),"scientific_name"] <- "Oligoplites saurus"
Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Shrimp"),"scientific_name"] <- "Penaeus duorarum"
Group_name[[94]][which(Group_name[[94]][,"original_name"] == "S. notata"),"scientific_name"] <- "Strongylura notata"
Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[94]][which(Group_name[[94]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 95 - article OK
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "0-3 Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "3-8 Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "8-18 Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "18-36 Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "36+ Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "0-6 Mullet"),"scientific_name"] <- "Mugil cephalus"
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "6-18 Mullet"),"scientific_name"] <- "Mugil cephalus"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "18+ Mullet"),"scientific_name"] <- "Mugil cephalus"
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Mackrel 0-3"),"scientific_name"] <- "Scomberomorus"
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Mackrel 3+"),"scientific_name"] <- "Scomberomorus"
#Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Ladyfish 0-10"),"scientific_name"] <- "Elops saurus"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Ladyfish 10+"),"scientific_name"] <- "Elops saurus"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Bay Anchovy"),"scientific_name"] <- "Anchoa mitchilli"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Pin Fish"),"scientific_name"] <- "Lagodon rhomboides"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Silver Perch"),"scientific_name"] <- "Bairdiella chrysoura"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Scaled Sardine"),"scientific_name"] <- "Harengula jaguana"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Menidia (silverside)"),"scientific_name"] <- "Menidia menidia"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Manhaden"),"scientific_name"] <- "Brevoortia patronus"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Blue Crab"),"scientific_name"] <- "Callinectes sapidus"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Pigfish"),"scientific_name"] <- "Orthopristis chrysoptera"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Macro Zooplankton"),"scientific_name"] <- "Macrozooplankton"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Micro Zoolplankton"),"scientific_name"] <- "Microzooplankton"
Group_name[[95]][which(Group_name[[95]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 96 - article OK
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Snapper"),"scientific_name"] <- "Lutjanus griseus"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Toadfish"),"scientific_name"] <- "Opsanus beta"
#Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Anchovy"),"scientific_name"] <- "Anchoa"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "White mullet"),"scientific_name"] <- "Mugil curema"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Spadefish"),"scientific_name"] <- "Chaetodipterus faber"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Oyster"),"scientific_name"] <- "Crassostrea virginica"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[96]][which(Group_name[[96]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 97 - article OK
        # Only functionnal groups

# Web 98 - article OK
        # Only functionnal groups

# Web 99 - article OK
        # Only functionnal groups

# Web 100 - article OK
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "P maculatus"),"scientific_name"] <- "Pimelodus maculatus"
#Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hypostomus spp"),"scientific_name"] <- "Hypostomus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Schizodon borelii"),"scientific_name"] <- "Schizodon borelii"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Loricariichthys platymetopon"),"scientific_name"] <- "Loricariichthys platymetopon"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Prochilodus lineatus"),"scientific_name"] <- "Prochilodus lineatus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Cyphocharax modesta"),"scientific_name"] <- "Cyphocharax modesta"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Steindacchnerina insculpta"),"scientific_name"] <- "Steindachnerina insculpta"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Iheringichthys labrosus"),"scientific_name"] <- "Iheringichthys labrosus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Schizodon altoparanae"),"scientific_name"] <- "Schizodon altoparanae"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hypophthalmus edentatus"),"scientific_name"] <- "Hypophthalmus edentatus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Trachydoras paraguayensis"),"scientific_name"] <- "Trachydoras paraguayensis"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hoplosternum littorale"),"scientific_name"] <- "Hoplosternum littorale"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Leporinus friderici"),"scientific_name"] <- "Leporinus friderici"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Leporinus obtusidens"),"scientific_name"] <- "Leporinus obtusidens"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Parauchenipterus galeatus"),"scientific_name"] <- "Parauchenipterus galeatus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Pterodoras granulosus"),"scientific_name"] <- "Pterodoras granulosus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Astyanax altiparanae"),"scientific_name"] <- "Astyanax altiparanae"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Auchenipterus nuchalis"),"scientific_name"] <- "Auchenipterus nuchalis"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Brycon orbignyanus"),"scientific_name"] <- "Brycon orbignyanus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Acestrorhyncus lacustris"),"scientific_name"] <- "Acestrorhynchus lacustris"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hoplias malabaricus"),"scientific_name"] <- "Hoplias malabaricus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Plagioscion squamisissimus"),"scientific_name"] <- "Plagioscion squamisissimus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Pseudoplatystoma corruscans"),"scientific_name"] <- "Pseudoplatystoma corruscans"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Rhaphiodon vulpinis"),"scientific_name"] <- "Rhaphiodon vulpinis"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Serrasalmus marginatus"),"scientific_name"] <- "Serrasalmus marginatus"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Hemisorubin platyrhynchos"),"scientific_name"] <- "Hemisorubim platyrhynchos"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Salminus brasiliensis"),"scientific_name"] <- "Salminus brasiliensis"
Group_name[[100]][which(Group_name[[100]][,"original_name"] == "Serrasalmus spiloptera"),"scientific_name"] <- "Serrasalmus spiloptera"

# Web 101 - article OK
        # Only functionnal groups

# Web 102 - article OK
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Cod 4+"),"scientific_name"] <- "Gadus morhua"
#Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Cod juv"),"scientific_name"] <- "Gadus morhua"
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Grl. halibut 5+"),"scientific_name"] <- "Reinhardtius hippoglossoides"
#Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Grl. halibut juv"),"scientific_name"] <- "Reinhardtius hippoglossoides"
#Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Redfish larger than 14cm"),"scientific_name"] <- "Sebastes" # > 14cm
#Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Redfish juv less 15cm"),"scientific_name"] <- "Sebastes" # < 15cm
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Polar cod"),"scientific_name"] <- "Boreogadus saida"
Group_name[[102]][which(Group_name[[102]][,"original_name"] == " Thorny ray"),"scientific_name"] <- "Raja radiata"
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Long rough Dab"),"scientific_name"] <- "Hippoglossoides platessoides"
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Northern shrimp"),"scientific_name"] <- "Pandalus borealis"
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Squids"),"scientific_name"] <- "Gonatus fabricii"
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Large Zooplankton"),"scientific_name"] <- "Large Zooplankton"
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Small Zooplankton"),"scientific_name"] <- "Small Zooplankton"
Group_name[[102]][which(Group_name[[102]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 103 - article OK
        # Only functionnal groups

# Web 104 - article OK
        # Only functionnal groups

# Web 105 - article OK
Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Lingcod"),"scientific_name"] <- "Ophiodon elongatus"
#Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Juv Lingcod"),"scientific_name"] <- "Ophiodon elongatus"
Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Adult Pacific Cod"),"scientific_name"] <- "Gadus macrocephalus"
#Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Juv. Pacific Cod"),"scientific_name"] <- "Gadus macrocephalus"
Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Adult Herring"),"scientific_name"] <- "Clupea pallasii"
#Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Juv. Herring"),"scientific_name"] <- "Clupea pallasii"
Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Eulachon"),"scientific_name"] <- "Thaleichthys pacificus"
Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Pink Shrimp"),"scientific_name"] <- "Pandalus jordani"
Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Herb. Zoolplankton"),"scientific_name"] <- "Herbivorous zooplankton"
Group_name[[105]][which(Group_name[[105]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 106 - article OK
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Sperm whales"),"scientific_name"] <- "Physeter macrocephalus"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Steller sea lions"),"scientific_name"] <- "Eumetopias jubatus"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Adult pollock"),"scientific_name"] <- "Theragra chalcogramma"
#Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Juvenile pollock"),"scientific_name"] <- "Theragra chalcogramma"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Pacific cod"),"scientific_name"] <- "Gadus macrocephalus"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "P.halibut"),"scientific_name"] <- "Hippoglossus stenolepis"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Arrowtooth flounder"),"scientific_name"] <- "Atheresthes stomias"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Greenland turbot"),"scientific_name"] <- "Reinhardtius hippoglossoides"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Tanner crab"),"scientific_name"] <- "Chionoecetes bairdi"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Snow crab"),"scientific_name"] <- "Chionoecetes opilio"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Pacific herring"),"scientific_name"] <- "Clupea pallasii"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Jellyfish"),"scientific_name"] <- "Chrysaora melenaster" # most abundant
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Large Zooplankton"),"scientific_name"] <- "Large Zooplankton"
Group_name[[106]][which(Group_name[[106]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 107 - article OK
Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Span. mackerel"),"scientific_name"] <- "Scomberomorus maculatus"
Group_name[[107]][which(Group_name[[107]][,"original_name"] == "King mackerel"),"scientific_name"] <- "Scomberomorus cavalla"
Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Red grouper"),"scientific_name"] <- "Epinephelus morio"
Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Red snapper"),"scientific_name"] <- "Lutjanus campechanus"
Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[107]][which(Group_name[[107]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 108 - article OK
Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Grl. Halibut"),"scientific_name"] <- "Reinhardtius hippoglossoides"
#Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Red fish"),"scientific_name"] <- "Sebastes"
Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Northern shrimp"),"scientific_name"] <- "Pandalus borealis"
Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Polar cod"),"scientific_name"] <- "Boreogadus suida"
Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Cod"),"scientific_name"] <- "Gadus morhua"
Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[108]][which(Group_name[[108]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 109 - article OK
Group_name[[109]][which(Group_name[[109]][,"original_name"] == "Red grouper"),"scientific_name"] <- "Epinephelus morio"
Group_name[[109]][which(Group_name[[109]][,"original_name"] == "King mackerel"),"scientific_name"] <- "Scomberomorus cavalla"
Group_name[[109]][which(Group_name[[109]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
Group_name[[109]][which(Group_name[[109]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 110 - article OK
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Lagopus muta"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Snow_bunting"),"scientific_name"] <- "Plectrophenax nivalis"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Arctic_wolve"),"scientific_name"] <- "Canis lupus arctos"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus hyperboreus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"
Group_name[[110]][which(Group_name[[110]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"

# Web 111 - article OK
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus hyperboreus"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Passerines"),"scientific_name"] <- "Plectrophenax nivalis"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[111]][which(Group_name[[111]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Lagopus muta"

# Web 112 - article OK
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Voles"),"scientific_name"] <- "Microtus oeconomus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Red_fox"),"scientific_name"] <- "Vulpes vulpes"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
Group_name[[112]][which(Group_name[[112]][,"original_name"] == "Wolverine"),"scientific_name"] <- "Gulo gulo"

# Web 113 - article OK
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Lagopus muta"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Snow_bunting"),"scientific_name"] <- "Plectrophenax nivalis"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiaca"
Group_name[[113]][which(Group_name[[113]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"

# Web 114 - article OK
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Brown_lemmings"),"scientific_name"] <- "Lemmus trimucronatus"
Group_name[[114]][which(Group_name[[114]][,"original_name"] == "Tundra_voles"),"scientific_name"] <- "Microtus oeconomus"
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


Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Snow_goose"),"scientific_name"] <- "Chen caerulescens"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Brown_lemming"),"scientific_name"] <- "Lemmus trimucronatus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Arctic_Fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiaca"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Parasitic_jaeger"),"scientific_name"] <- "Stercorarius parasiticus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Peregrine_falcon"),"scientific_name"] <- "Falco peregrinus"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Weasel"),"scientific_name"] <- "Mustela erminea"
Group_name[[115]][which(Group_name[[115]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus hyperboreus"

# Web 116 - article OK
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Voles"),"scientific_name"] <- "Microtus oeconomus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Red_fox"),"scientific_name"] <- "Vulpes vulpes"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Falcons"),"scientific_name"] <- "Falco peregrinus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiaca"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Brown_bear"),"scientific_name"] <- "Ursus horribilis"
Group_name[[116]][which(Group_name[[116]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus hyperboreus"

# Write the list as a .RDS file
saveRDS(Group_name, file = "data/intermediate/Group_name.RDS")
