# Script to format the 116 network data from Jacquet et al. 2019
# Only keep the interactions that are between species in all the 116 networks

# Libraries needed
library(tibble)
library(dplyr)

# Load the models' names
load("data_retrieval/ecopath/Data_ncomms12573/Ecopath_models.Rdata") # Ecopath_models, 116 rows for 116 networks
# Load the name of each "species" in each 116 networks
load("data_retrieval/ecopath/Data_ncomms12573/GroupName.Rdata") # GroupName, list of length 116 for 116 networks

# Transform each list of names into a DataFrame
# Add an empty column to each DataFrame
# original_name is the name initially found in the 116 Ecopath models
# scientific_name are the names that are retrieved from each article related to the model
GroupName <- lapply(GroupName, function(x) as.data.frame(x)) |>
             lapply(function(x) tibble::add_column(x, scientific_name = NA)) |>
             lapply(function(x) dplyr::rename(x, original_name = x))


###################################################################################################

# Have to modify a lot of names by hand, because there are some for example that are written as "herring" when in the original
# article it was "adult pacific herring" which can be linked to a unique scientific name to retrieve a body mass!

##################################################################################################

# Web by web, changing species names.

# Web 1 - article OK
        # Juvenile pacific herring < 18cm
GroupName[[1]][which(GroupName[[1]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea pallasi"
GroupName[[1]][which(GroupName[[1]][,"original_name"] == "Sea otters"),"scientific_name"] <- "Enhydra lutris"
GroupName[[1]][which(GroupName[[1]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "phytoplankton"
GroupName[[1]][which(GroupName[[1]][,"original_name"] == "Macrozooplankton"),"scientific_name"] <- "macrozooplankton"

# Web 2 - article OK
GroupName[[2]][which(GroupName[[2]][,"original_name"] == "Totoaba"),"scientific_name"] <- "Totoaba macdonaldi"
GroupName[[2]][which(GroupName[[2]][,"original_name"] == "Vaquita"),"scientific_name"] <- "Phocoena sinus"
GroupName[[2]][which(GroupName[[2]][,"original_name"] == "Lobo marino"),"scientific_name"] <- "Zalophus californianus"
GroupName[[2]][which(GroupName[[2]][,"original_name"] == "C. de roca"),"scientific_name"] <- "Sicyonia penicillata"
GroupName[[2]][which(GroupName[[2]][,"original_name"] == "C. cafe"),"scientific_name"] <- "Farfantepeneaeus californiensis"
GroupName[[2]][which(GroupName[[2]][,"original_name"] == "C. azul"),"scientific_name"] <- "Litopenaeus stylirostris"
GroupName[[2]][which(GroupName[[2]][,"original_name"] == "Zooplancton"),"scientific_name"] <- "Zooplancton"
GroupName[[2]][which(GroupName[[2]][,"original_name"] == "Fitoplancton"),"scientific_name"] <- "Phytoplancton"

# Web 3 - article MISSING
        
# Web 4 - article OK
GroupName[[4]][which(GroupName[[4]][,"original_name"] == "Lemuru"),"scientific_name"] <- "Sardinella lemuru"
GroupName[[4]][which(GroupName[[4]][,"original_name"] == "Scads"),"scientific_name"] <- "Decapterus"
GroupName[[4]][which(GroupName[[4]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[4]][which(GroupName[[4]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 5 - article OK
        # Only functionnal groups

# Web 6 - article OK
GroupName[[6]][which(GroupName[[6]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Temora longicornis"
GroupName[[6]][which(GroupName[[6]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 7 - article OK
        # Only functionnal groups

# Web 8 - article OK
        # Only functionnal groups

# Web 9 - article OK
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "Copepoda"),"scientific_name"] <- "Calanoid"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "Worms"),"scientific_name"] <- "Nereidae"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "O. chrysoptera"),"scientific_name"] <- "Orthopristis chrysoptera"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "L. rhomboides"),"scientific_name"] <- "Lagodon rhomboides"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "O. oglinum"),"scientific_name"] <- "Opisthonema oglinum"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "H. jaguana"),"scientific_name"] <- "Harengula jaguana"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "A. rhomboidalis"),"scientific_name"] <- "Archosargus rhomboidalis"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "H. plumieri"),"scientific_name"] <- "Haemulon plumieri"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "B. chrysoura"),"scientific_name"] <- "Bairdiella chrysoura"
GroupName[[9]][which(GroupName[[9]][,"original_name"] == "A. quadricornis"),"scientific_name"] <- "Acanthostracion quadricornis"

# Web 10 - article OK
        # Only functionnal groups
        
# Web 11 - article OK
GroupName[[11]][which(GroupName[[11]][,"original_name"] == "Acanthocybium s"),"scientific_name"] <- "Acanthocybium solandri"
GroupName[[11]][which(GroupName[[11]][,"original_name"] == "Sea Turtles"),"scientific_name"] <- "Caretta caretta"
GroupName[[11]][which(GroupName[[11]][,"original_name"] == "Katsuwonus pela"),"scientific_name"] <- "Katsuwonus pelamis"
GroupName[[11]][which(GroupName[[11]][,"original_name"] == "Thunnus albacar"),"scientific_name"] <- "Thunnus albacares"
GroupName[[11]][which(GroupName[[11]][,"original_name"] == "Small tuna"),"scientific_name"] <- "Euthynnus alletteratus"
GroupName[[11]][which(GroupName[[11]][,"original_name"] == "Decapterus maca"),"scientific_name"] <- "Decapterus macarellus"
GroupName[[11]][which(GroupName[[11]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[11]][which(GroupName[[11]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 12 - article OK
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Minke whales"),"scientific_name"] <- "Balaenoptera acutorostrata"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Fin whales"),"scientific_name"] <- "Balaenoptera physalus"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Humpback whales"),"scientific_name"] <- "Megaptera novaeangliae"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Brydes whales"),"scientific_name"] <- "Balaenoptera brydei"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Sei whales"),"scientific_name"] <- "Balaenoptera borealis"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Blue whales"),"scientific_name"] <- "Balaenoptera musculus"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Sea turtles"),"scientific_name"] <- "Dermochelys coriacea"
GroupName[[12]][which(GroupName[[12]][,"original_name"] == "Dolphinfish"),"scientific_name"] <- "Coryphaena hippurus"

# Web 13 - article OK
        # Only functionnal groups

# Web 14 - article OK
GroupName[[14]][which(GroupName[[14]][,"original_name"] == "Yellowfin"),"scientific_name"] <- "Thunnus albacares"
GroupName[[14]][which(GroupName[[14]][,"original_name"] == "Bluefin"),"scientific_name"] <- "Thunnus thynnus"
GroupName[[14]][which(GroupName[[14]][,"original_name"] == "Skipjack"),"scientific_name"] <- "Katsuwonus pelamis"
GroupName[[14]][which(GroupName[[14]][,"original_name"] == "Albacore"),"scientific_name"] <- "Thunnus alalunga"
GroupName[[14]][which(GroupName[[14]][,"original_name"] == "Bigeye"),"scientific_name"] <- "Thunnus obesus"
GroupName[[14]][which(GroupName[[14]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"

# Web 15 - article OK
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Sea lion"),"scientific_name"] <- "Sea lion"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Chilean hake (j)"),"scientific_name"] <- "Merluccius gayi" # age 0-3
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Chilean hake (a)"),"scientific_name"] <- "Merluccius gayi" # age 0-3
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Pilchard (j)"),"scientific_name"] <- "Strangomera bentincki" # age 0
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Pilchard (a)"),"scientific_name"] <- "Strangomera bentincki" # age 1+
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Anchovy (j)"),"scientific_name"] <- "Engraulis ringens" # age 0
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Anchovy (a)"),"scientific_name"] <- "Engraulis ringens" # age 1+
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Carrot prawn (j)"),"scientific_name"] <- "Pleuroncodes monodon" # age 0
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Carrot prawn (a)"),"scientific_name"] <- "Pleuroncodes monodon" # age 1+
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Yellow prawn"),"scientific_name"] <- "Cervimunida johni"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Horse mackerel"),"scientific_name"] <- "Trachurus symmetricus murphyi"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Black conger"),"scientific_name"] <- "Genypterus maculatus"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Rattail fish"),"scientific_name"] <- "Coelorhyncus aconcagua"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Big0eye flounder"),"scientific_name"] <- "Hipoglossina macrops"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Cardinal fish"),"scientific_name"] <- "Epigonus crassicaudus"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Pacific sand perch"),"scientific_name"] <- "Prolatilus jugularis"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Raja"),"scientific_name"] <- "Raja"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Copepods"),"scientific_name"] <- "Copepods"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Euphausiids"),"scientific_name"] <- "Euphausiids"
GroupName[[15]][which(GroupName[[15]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 16 - article OK
        # Only functionnal groups

# Web 17 - article OK
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Blue marlin"),"scientific_name"] <- "Makaira mazara"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Blue shark"),"scientific_name"] <- "Prionace glauca"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Yellowfin tuna"),"scientific_name"] <- "Thunnus albacares"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Bigeye tuna"),"scientific_name"] <- "Thunnus obesus"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Albacore tuna"),"scientific_name"] <- "Thunnus alalunga"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Mahimahi"),"scientific_name"] <- "Coryphaena hippurus"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Skipjack tuna"),"scientific_name"] <- "Katsuwonus pelamis"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Lancetfish"),"scientific_name"] <- "Alepisaurus ferox"
GroupName[[17]][which(GroupName[[17]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Alepisaurus ferox"

# Web 18 - article OK
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Spot"),"scientific_name"] <- "Leiostomus xanthurus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Striped bass YOY"),"scientific_name"] <- "Morone saxatilis" # young of the year
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Striped bass resident"),"scientific_name"] <- "Morone saxatilis"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Striped bass migratory"),"scientific_name"] <- "Morone saxatilis"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Bluefish YOY"),"scientific_name"] <- "Pomatomus saltatrix" # young of the year
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Bluefish adult"),"scientific_name"] <- "Pomatomus saltatrix"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Weakfish YOY"),"scientific_name"] <- "Cynoscion regalis"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Weakfish Adult"),"scientific_name"] <- "Cynoscion regalis"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Summer flounder"),"scientific_name"] <- "Paralichthys dentatus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Atl. croaker"),"scientific_name"] <- "Micropogonias undulatus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Menhaden YOY"),"scientific_name"] <- "Brevoortia tyrannus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Menhaden adult"),"scientific_name"] <- "Brevoortia tyrannus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "black drum"),"scientific_name"] <- "Pogonius cromis"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Alewife and herring"),"scientific_name"] <- "Alosa"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "American eel"),"scientific_name"] <- "Anguilla rostrata"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "American shad"),"scientific_name"] <- "Alosa sapidissima" # american/hickory shad?
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "bay anchovy"),"scientific_name"] <- "Anchoa mitchilli"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "White perch YOY"),"scientific_name"] <- "Morone Americana"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "White perch adult"),"scientific_name"] <- "Morone Americana"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "gizzard shad"),"scientific_name"] <- "Dorosoma cepedianum"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "blue catfish"),"scientific_name"] <- "Ictalurus furcatus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "sandbar shark"),"scientific_name"] <- "Carcharhinus plumbeus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Hard clam"),"scientific_name"] <- "Mercenaria mercenaria"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "ctenophores"),"scientific_name"] <- "Mnemiopsis"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "sea nettles"),"scientific_name"] <- "Chrysaora quinquecirrha"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Blue crab YOY"),"scientific_name"] <- "Callinectes sapidus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Blue crab adult"),"scientific_name"] <- "Callinectes sapidus"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "oyster"),"scientific_name"] <- "Crassostrea virginica"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "soft clam"),"scientific_name"] <- "Mya arenaria"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Mesozooplankton"),"scientific_name"] <- "Mesozooplankton"
GroupName[[18]][which(GroupName[[18]][,"original_name"] == "Microzooplankton"),"scientific_name"] <- "Microzooplankton"

# Web 19 - article MISSING

# Web 20 - article MISSING

# Web 21 - article OK
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Spotted Dolphin"),"scientific_name"] <- "Stenella attenuata"
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Lg Yellowfin"),"scientific_name"] <- "Thunnus albacares" # >=90cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Lg Bigeye"),"scientific_name"] <- "Thunnus obesus" # >=80cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Lg Sailfish"),"scientific_name"] <- "Istiophorus platypterus" #>=150cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Lg Swordfish"),"scientific_name"] <- "Xiphias gladus" # >=150cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Lg Wahoo"),"scientific_name"] <- "Acanthocybium solandri" # >=90cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Rays"),"scientific_name"] <- "Manta birostris"
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Skipjack"),"scientific_name"] <- "Katsuwonus pelamis"
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Albacore"),"scientific_name"] <- "Thunnus alalunga"
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Bluefin"),"scientific_name"] <- "Thunnus orientalis"
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Sm Yellowfin"),"scientific_name"] <- "Thunnus albacares" # <90cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Sm Bigeye"),"scientific_name"] <- "Thunnus obesus" # <80cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Sm Sailfish"),"scientific_name"] <- "Istiophorus platypterus" # <150cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Sm Swordfish"),"scientific_name"] <- "Xiphias gladus" #< 150cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Sm Wahoo"),"scientific_name"] <- "Acanthocybium solandri" # <90cm
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Lg Phytoplankton"),"scientific_name"] <- "Large Phytoplankton"
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Microzooplankton"),"scientific_name"] <- "Microzooplankton"
GroupName[[21]][which(GroupName[[21]][,"original_name"] == "Mesozooplankton"),"scientific_name"] <- "Mesozooplankton"

# Web 22 - article OK
GroupName[[22]][which(GroupName[[22]][,"original_name"] == "Anguille"),"scientific_name"] <- "Anguilla anguilla"
GroupName[[22]][which(GroupName[[22]][,"original_name"] == "Adult mugilids"),"scientific_name"] <- "Muguilidae"
GroupName[[22]][which(GroupName[[22]][,"original_name"] == "Atherinids"),"scientific_name"] <- "Atherinidae"
GroupName[[22]][which(GroupName[[22]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
GroupName[[22]][which(GroupName[[22]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"


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
GroupName[[31]][which(GroupName[[31]][,"original_name"] == "Epifauna"),"scientific_name"] <- "Panulirus argus"
GroupName[[31]][which(GroupName[[31]][,"original_name"] == "Shrimps"),"scientific_name"] <- "Xiphopenaeus kroyeri"
GroupName[[31]][which(GroupName[[31]][,"original_name"] == "Small pelagic fish"),"scientific_name"] <- "Cetengraulis edentulus"
GroupName[[31]][which(GroupName[[31]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[31]][which(GroupName[[31]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 32 - a2ticle O2
GroupName[[32]][which(GroupName[[32]][,"original_name"] == "Adult capelin (2+)"),"scientific_name"] <- "Mallotus villosus"
GroupName[[32]][which(GroupName[[32]][,"original_name"] == "Polar cod (1+)"),"scientific_name"] <- "Boreogadus saida"
GroupName[[32]][which(GroupName[[32]][,"original_name"] == "Adult cod (4+)"),"scientific_name"] <- "Gadus morhua"
GroupName[[32]][which(GroupName[[32]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[32]][which(GroupName[[32]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 33 - article OK
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Adult saithe"),"scientific_name"] <- "Pollachius virens"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Adult cod (4+)"),"scientific_name"] <- "Gadus morhua"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Minke"),"scientific_name"] <- "Balaenoptera acutorostrata"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Juvenile saithe"),"scientific_name"] <- "Pollachius virens"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Juvenile cod (1-3)"),"scientific_name"] <- "Gadus morhua"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Adult haddock (4+)"),"scientific_name"] <- "Melanogrammus aeglefinus"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Juvenile haddock (1-3)"),"scientific_name"] <- "Melanogrammus aeglefinus"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Polar cod (1+)"),"scientific_name"] <- "Boreogadus saida"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Adult capelin (2+)"),"scientific_name"] <- "Mallotus villosus"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Squid"),"scientific_name"] <- "Gonatus fabricii"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Adult herring (4+)"),"scientific_name"] <- "Clupea harengus"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Juvenile capelin (1)"),"scientific_name"] <- "Mallotus villosus"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Juvenile herring (1-3)"),"scientific_name"] <- "Clupea harengus"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Prawns & shrimps"),"scientific_name"] <- "Pandalus borealis"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Herbivorous zooplankton"),"scientific_name"] <- "Herbivorous zooplankton"
GroupName[[33]][which(GroupName[[33]][,"original_name"] == "Carnivorous zooplankton"),"scientific_name"] <- "Carnivorous zooplankton"

# Web 34 - article OK
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Sciaenidae"),"scientific_name"] <- "Cynoscion xanthulum"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Elopidae"),"scientific_name"] <- "Elops affinis"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Carangidae"),"scientific_name"] <- "Caranx hippos"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Centropomidae"),"scientific_name"] <- "Centropomus robalito"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Ariidae"),"scientific_name"] <- "Arius guatemalensis"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Haemulidae"),"scientific_name"] <- "Anisotremus interreptus"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Callinectes"),"scientific_name"] <- "Callinectes arcuatus"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Gerreidae"),"scientific_name"] <- "Diapterus peruvians"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Gobios"),"scientific_name"] <- "Dormitator latrifons"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Mugilidae"),"scientific_name"] <- "Mugil cephalus"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Palaemonidae"),"scientific_name"] <- "Macrobranchium"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Chanidae"),"scientific_name"] <- "Chanos chanos"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Fitoplancton"),"scientific_name"] <- "Phytoplankton"
GroupName[[34]][which(GroupName[[34]][,"original_name"] == "Zooplancton"),"scientific_name"] <- "Zooplankton"

# Web 35 - article Ok
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Cod"),"scientific_name"] <- "Gadus morhua" # age 3-14
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Juvenile cod"),"scientific_name"] <- "Gadus morhua" # age 0-2
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Haddock"),"scientific_name"] <- "Melanogrammus aeglefinus"
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Saithe"),"scientific_name"] <- "Pollachius virens"
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Greenland halibut"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea harengus"
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Nephrops"),"scientific_name"] <- "Nephrops norvegicus"
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Northern shrimps"),"scientific_name"] <- "Pandalus borealis"
# GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Nekton"),"scientific_name"] <- "Krill" # See article for generic values
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[35]][which(GroupName[[35]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 36 - article OK
GroupName[[36]][which(GroupName[[36]][,"original_name"] == "Dorado"),"scientific_name"] <- "Coryphaena hippurus"
GroupName[[36]][which(GroupName[[36]][,"original_name"] == "Sierra"),"scientific_name"] <- "Scomberomorus sierra"
GroupName[[36]][which(GroupName[[36]][,"original_name"] == "Esc\xf3mbridos"),"scientific_name"] <- "Katsuwonus pelamis" # Otros escombridos
GroupName[[36]][which(GroupName[[36]][,"original_name"] == "Adultos Lutjanu"),"scientific_name"] <- "Lutjanus peru"
GroupName[[36]][which(GroupName[[36]][,"original_name"] == "Juveniles Lutja"),"scientific_name"] <- "Lutjanus peru"
GroupName[[36]][which(GroupName[[36]][,"original_name"] == "Pulpo"),"scientific_name"] <- "Octopus hubbsorum"
GroupName[[36]][which(GroupName[[36]][,"original_name"] == "Zooplancton"),"scientific_name"] <- "Zooplankton"
GroupName[[36]][which(GroupName[[36]][,"original_name"] == "Fitoplancton"),"scientific_name"] <- "Phytoplankton"

# Web 37 - article OK
        # Only functionnal groups

# Web 38 - article OK
        # Only functionnal groups

# Web 39 - article OK
GroupName[[39]][which(GroupName[[39]][,"original_name"] == "Phyt"),"scientific_name"] <- "Phytoplankton"
GroupName[[39]][which(GroupName[[39]][,"original_name"] == "Bakt"),"scientific_name"] <- "Bakterioplankton"
GroupName[[39]][which(GroupName[[39]][,"original_name"] == "Moff"),"scientific_name"] <- "Bivalvia"
GroupName[[39]][which(GroupName[[39]][,"original_name"] == "Zoopl"),"scientific_name"] <- "Zooplankton"
GroupName[[39]][which(GroupName[[39]][,"original_name"] == "Momf"),"scientific_name"] <- "Actinia"

# Web 40 - article OK
 # Waiting on Mario Delos Reyes

# Web 41 - article OK
 # Waiting on Mario Delos Reyes

# Web 42 - article OK
 # Waiting on Mario Delos Reyes

# Web 43 - article OK
GroupName[[43]][which(GroupName[[43]][,"original_name"] == "Roach fry"),"scientific_name"] <- "Rutilus rutilus"
GroupName[[43]][which(GroupName[[43]][,"original_name"] == "Roach adults"),"scientific_name"] <- "Rutilus rutilus"
GroupName[[43]][which(GroupName[[43]][,"original_name"] == "phytoplankton"),"scientific_name"] <- "phytoplankton"
GroupName[[43]][which(GroupName[[43]][,"original_name"] == "predatory zoo"),"scientific_name"] <- "predatory zooplankton"
GroupName[[43]][which(GroupName[[43]][,"original_name"] == "herbivorous zoo"),"scientific_name"] <- "herbivorous zooplankton"

# Web 44 - article OK
GroupName[[44]][which(GroupName[[44]][,"original_name"] == "Sarotherodon"),"scientific_name"] <- "Sarotherodon galilaeus"
GroupName[[44]][which(GroupName[[44]][,"original_name"] == "Alestes macro"),"scientific_name"] <- "Alestes macrolepidotus"
GroupName[[44]][which(GroupName[[44]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[44]][which(GroupName[[44]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 45 - article OK
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "H. squamipinnis"),"scientific_name"] <- "Haplochromis squamipinnis"
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "Protopterus"),"scientific_name"] <- "Protopterus aethiopicus"
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "Clarias gariopi"),"scientific_name"] <- "Clarias gariepinus"
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "H. angustifrons"),"scientific_name"] <- "Haplochromis angustifrons"
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "H. nigripinnis"),"scientific_name"] <- "Haplochromis nigripinnis"
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "O. niloticus"),"scientific_name"] <- "Oreochromis niloticus"
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "O. leucostictus"),"scientific_name"] <- "Oreochromis leucostictus"
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[45]][which(GroupName[[45]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 46 - article OK
GroupName[[46]][which(GroupName[[46]][,"original_name"] == "H. vittatus"),"scientific_name"] <- "Hydrocynus vittatus"
GroupName[[46]][which(GroupName[[46]][,"original_name"] == "L. miodon"),"scientific_name"] <- "Limnothrissa miodon"
GroupName[[46]][which(GroupName[[46]][,"original_name"] == "S. zambezensis"),"scientific_name"] <- "Synodontis zambezensis"
GroupName[[46]][which(GroupName[[46]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[46]][which(GroupName[[46]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 47 - article OK
GroupName[[47]][which(GroupName[[47]][,"original_name"] == "Lavnun"),"scientific_name"] <- "Mirogrex terraesanctae"
GroupName[[47]][which(GroupName[[47]][,"original_name"] == "Silver carp"),"scientific_name"] <- "Hypophtalmichthys molitrix"
GroupName[[47]][which(GroupName[[47]][,"original_name"] == "Piscivores"),"scientific_name"] <- "Clarias lazera"
GroupName[[47][which(GroupName[[47]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
GroupName[[47][which(GroupName[[47]][,"original_name"] == "Herb. zoop."),"scientific_name"] <- "Herbivorous zooplankton"
GroupName[[47][which(GroupName[[47]][,"original_name"] == "Carn. zoop."),"scientific_name"] <- "Carnivorous zooplankton"

# Web 48 - article OK
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Kampango"),"scientific_name"] <- "Bagrus meridionalis"
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Mpasa"),"scientific_name"] <- "Opsaridium microlepis"
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Sanjika"),"scientific_name"] <- "Opsaridium microcephalus"
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Mcheni"),"scientific_name"] <- "Ramphochromis"
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Bombe"),"scientific_name"] <- "Bathyclarias"
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Usipa"),"scientific_name"] <- "Engraulicypris sardella"
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Nkhungu"),"scientific_name"] <- "Chaoborus edulis"
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[48]][which(GroupName[[48]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 49 - article OK
GroupName[[49]][which(GroupName[[49]][,"original_name"] == "Opsaridium"),"scientific_name"] <- "Opsaridium microcephalus"
GroupName[[49]][which(GroupName[[49]][,"original_name"] == "Rhamphochromis"),"scientific_name"] <- "Rhamphochromis longiceps"
GroupName[[49]][which(GroupName[[49]][,"original_name"] == "Diplotaxodon"),"scientific_name"] <- "Diplotaxodon pallidorsalis"
GroupName[[49]][which(GroupName[[49]][,"original_name"] == "Engraulicypris"),"scientific_name"] <- "Engraulicypris edulis"
GroupName[[49]][which(GroupName[[49]][,"original_name"] == "Chaoborus"),"scientific_name"] <- "Chaoborus edulis"
GroupName[[49]][which(GroupName[[49]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[49]][which(GroupName[[49]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 50 - article OK
GroupName[[50]][which(GroupName[[50]][,"original_name"] == "Limnothrissa"),"scientific_name"] <- "Limnothrissa miodon"
GroupName[[50]][which(GroupName[[50]][,"original_name"] == "Stolothrissa"),"scientific_name"] <- "Stolothrissa tanganyicae"
GroupName[[50]][which(GroupName[[50]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[50]][which(GroupName[[50]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 51 - article OK
GroupName[[51]][which(GroupName[[51]][,"original_name"] == "Limnothrissa"),"scientific_name"] <- "Limnothrissa miodon"
GroupName[[51]][which(GroupName[[51]][,"original_name"] == "Stolothrissa"),"scientific_name"] <- "Stolothrissa tanganyicae"
GroupName[[51]][which(GroupName[[51]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[51]][which(GroupName[[51]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 52 - article OK
GroupName[[52]][which(GroupName[[52]][,"original_name"] == "Synod. schall"),"scientific_name"] <- "Synodontis schall"
GroupName[[52]][which(GroupName[[52]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[52]][which(GroupName[[52]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 53 - article OK
GroupName[[53]][which(GroupName[[53]][,"original_name"] == "Synod. schall"),"scientific_name"] <- "Synodontis schall"
GroupName[[53]][which(GroupName[[53]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[53]][which(GroupName[[53]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 54 - article OK
GroupName[[54]][which(GroupName[[54]][,"original_name"] == "Nile perch"),"scientific_name"] <- "Lates niloticus"
GroupName[[54]][which(GroupName[[54]][,"original_name"] == "Lungfish"),"scientific_name"] <- "Protopterus aethiopicus"
GroupName[[54]][which(GroupName[[54]][,"original_name"] == "Dagaa"),"scientific_name"] <- "Rastrineobola argentea"
GroupName[[54]][which(GroupName[[54]][,"original_name"] == "Lake prawn"),"scientific_name"] <- "Caridina nilotica"
GroupName[[54]][which(GroupName[[54]][,"original_name"] == "Nile tilapia"),"scientific_name"] <- "Oreochromis niloticus"
GroupName[[54]][which(GroupName[[54]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[54]][which(GroupName[[54]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 55 - article OK
GroupName[[55]][which(GroupName[[55]][,"original_name"] == "Nile perch"),"scientific_name"] <- "Lates niloticus"
GroupName[[55]][which(GroupName[[55]][,"original_name"] == "Lungfish"),"scientific_name"] <- "Protopterus aethiopicus"
GroupName[[55]][which(GroupName[[55]][,"original_name"] == "Dagaa"),"scientific_name"] <- "Rastrineobola argentea"
GroupName[[55]][which(GroupName[[55]][,"original_name"] == "Lake prawn"),"scientific_name"] <- "Caridina nilotica"
GroupName[[55]][which(GroupName[[55]][,"original_name"] == "Nile tilapia"),"scientific_name"] <- "Oreochromis niloticus"
GroupName[[55]][which(GroupName[[55]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[55]][which(GroupName[[55]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 56 - article OK
        # Only functionnal groups

# Web 57 - article OK
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Adult saithe"),"scientific_name"] <- "Pollachius virens"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Adult cod (4+)"),"scientific_name"] <- "Gadus morhua"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Minke"),"scientific_name"] <- "Balaenoptera acutorostrata"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Juvenile saithe"),"scientific_name"] <- "Pollachius virens"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Juvenile cod (1-3)"),"scientific_name"] <- "Gadus morhua"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Adult haddock (4+)"),"scientific_name"] <- "Melanogrammus aeglefinus"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Juvenile haddock (1-3)"),"scientific_name"] <- "Melanogrammus aeglefinus"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Polar cod (1+)"),"scientific_name"] <- "Boreogadus saida"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Adult capelin (2+)"),"scientific_name"] <- "Mallotus villosus"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Squid"),"scientific_name"] <- "Gonatus fabricii"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Adult herring (4+)"),"scientific_name"] <- "Clupea harengus"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Juvenile capelin (1)"),"scientific_name"] <- "Mallotus villosus"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Juvenile herring (1-3)"),"scientific_name"] <- "Clupea harengus"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Prawns & shrimps"),"scientific_name"] <- "Pandalus borealis"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Herbivorous zooplankton"),"scientific_name"] <- "Herbivorous zooplankton"
GroupName[[57]][which(GroupName[[57]][,"original_name"] == "Carnivorous zooplankton"),"scientific_name"] <- "Carnivorous zooplankton"

# Web 58 - article OK
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Adult saithe"),"scientific_name"] <- "Pollachius virens"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Adult cod (4+)"),"scientific_name"] <- "Gadus morhua"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Minke"),"scientific_name"] <- "Balaenoptera acutorostrata"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Juvenile saithe"),"scientific_name"] <- "Pollachius virens"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Juvenile cod (1-3)"),"scientific_name"] <- "Gadus morhua"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Adult haddock (4+)"),"scientific_name"] <- "Melanogrammus aeglefinus"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Juvenile haddock (1-3)"),"scientific_name"] <- "Melanogrammus aeglefinus"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Polar cod (1+)"),"scientific_name"] <- "Boreogadus saida"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Adult capelin (2+)"),"scientific_name"] <- "Mallotus villosus"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Squid"),"scientific_name"] <- "Gonatus fabricii"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Adult herring (4+)"),"scientific_name"] <- "Clupea harengus"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Juvenile capelin (1)"),"scientific_name"] <- "Mallotus villosus"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Juvenile herring (1-3)"),"scientific_name"] <- "Clupea harengus"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Prawns & shrimps"),"scientific_name"] <- "Pandalus borealis"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[58]][which(GroupName[[58]][,"original_name"] == ""),"scientific_name"] <- ""

# Web 59 - article OK
        # Only functionnal groups

# Web 60 - article OK
GroupName[[60]][which(GroupName[[60]][,"original_name"] == "Sharks"),"scientific_name"] <- "Carcharhinus melanopterus"
GroupName[[60]][which(GroupName[[60]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[60]][which(GroupName[[60]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 61 - article OK
        # Only functionnal groups

# Web 62 - article OK
        # Only functionnal groups

# Web 63 - article OK
        # Only functionnal groups

# Web 64 - article OK
        # Only functionnal groups

# Web 65 - article OK
        # Only functionnal groups

# Web 66 - article OK
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Walrus"),"scientific_name"] <- "Odobenus rosmarus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Grey seals"),"scientific_name"] <- "Halichoerus grypus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Harp Seals"),"scientific_name"] <- "Phoca groenlandica"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Hooded Seals"),"scientific_name"] <- "Cystophora cristata"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Cod + 40cm"),"scientific_name"] <- "Gadus morhua" # > 40cm
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Cod  <40 cm"),"scientific_name"] <- "Gadus morhua" # < 40cm
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Aplaice +35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # >  35cm
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Aplaice<=35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # <= 35cm
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "G. Halibut +65cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "G. Halibut Juv."),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Yellowtail Flounders "),"scientific_name"] <- "Limanda ferruginea"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Witch flounder"),"scientific_name"] <- "Glyptodephalus cynoglossus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Winter flounder"),"scientific_name"] <- "Pseudopleuronectes americanus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Transient Mackerel ( +29cm)"),"scientific_name"] <- "Scomber scombrus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Lumpfish"),"scientific_name"] <- "Cyclopterus lumpus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Greenland cod"),"scientific_name"] <- "Gadus opac"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Salmon"),"scientific_name"] <- "Salmo salar"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Sandlance"),"scientific_name"] <- "Ammodytes dubius"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Arctic cod"),"scientific_name"] <- "Boreogadus saida"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea harengus harengus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Shortfinned squid"),"scientific_name"] <- "Illex illecebrosus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Arctic Squid"),"scientific_name"] <- "Gonatus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Lobster"),"scientific_name"] <- "Lomarus americanus"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "L.Zooplankton"),"scientific_name"] <- "Large Zooplankton"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "S.Zooplankton"),"scientific_name"] <- "Small Zooplankton"
GroupName[[66]][which(GroupName[[66]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 67 - article OK
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Harp Seals"),"scientific_name"] <- "Phoca groenlandica"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Hooded Seals"),"scientific_name"] <- "Cystophora cristata"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Cod > 35cm"),"scientific_name"] <- "Gadus morhua"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Cod <= 35 cm"),"scientific_name"] <- "Gadus morhua"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "G.halibut>40cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "G.halibut<=40cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Aplaice>35cm"),"scientific_name"] <- "Hippoglossoides platessoides"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Aplaice<=35cm"),"scientific_name"] <- "Hippoglossoides platessoides"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Redfish"),"scientific_name"] <- "Sebastes"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Sandlance"),"scientific_name"] <- "Ammodytes dubius"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Arctic cod"),"scientific_name"] <- "Boreogadus saida"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "L.Zooplankton"),"scientific_name"] <- "Large Zooplankton"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "S.Zooplankton"),"scientific_name"] <- "Small Zooplankton"
GroupName[[67]][which(GroupName[[67]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 68 - article OK
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Grey seals"),"scientific_name"] <- "Halichoerus grypus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Harp Seals"),"scientific_name"] <- "Phoca groenlandica"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Hooded Seals"),"scientific_name"] <- "Cystophora cristata"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Cod + 40cm"),"scientific_name"] <- "Gadus morhua" # > 40cm
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Cod  <40 cm"),"scientific_name"] <- "Gadus morhua" # < 40cm
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Aplaice +35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # >  35cm
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Aplaice<=35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # <= 35cm
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "G. Halibut +65cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "G. Halibut Juv."),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Yellowtail Flounders "),"scientific_name"] <- "Limanda ferruginea"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Witch flounder"),"scientific_name"] <- "Glyptodephalus cynoglossus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Winter flounder"),"scientific_name"] <- "Pseudopleuronectes americanus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Transient Mackerel ( +29cm)"),"scientific_name"] <- "Scomber scombrus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Lumpfish"),"scientific_name"] <- "Cyclopterus lumpus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Greenland cod"),"scientific_name"] <- "Gadus opac"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Salmon"),"scientific_name"] <- "Salmo salar"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Sandlance"),"scientific_name"] <- "Ammodytes dubius"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Arctic cod"),"scientific_name"] <- "Boreogadus saida"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea harengus harengus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Shortfinned squid"),"scientific_name"] <- "Illex illecebrosus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Arctic Squid"),"scientific_name"] <- "Gonatus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Lobster"),"scientific_name"] <- "Lomarus americanus"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "L.Zooplankton"),"scientific_name"] <- "Large Zooplankton"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "S.Zooplankton"),"scientific_name"] <- "Small Zooplankton"
GroupName[[68]][which(GroupName[[68]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 69 - article OK
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Grey seals"),"scientific_name"] <- "Halichoerus grypus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Harp Seals"),"scientific_name"] <- "Phoca groenlandica"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Hooded Seals"),"scientific_name"] <- "Cystophora cristata"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Cod + 40cm"),"scientific_name"] <- "Gadus morhua" # > 40cm
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Cod  <40 cm"),"scientific_name"] <- "Gadus morhua" # < 40cm
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Aplaice +35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # >  35cm
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Aplaice<=35cm"),"scientific_name"] <- "Hippoglossoides platessoides" # <= 35cm
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "G. Halibut +65cm"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "G. Halibut Juv."),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Yellowtail Flounders "),"scientific_name"] <- "Limanda ferruginea"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Witch flounder"),"scientific_name"] <- "Glyptodephalus cynoglossus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Winter flounder"),"scientific_name"] <- "Pseudopleuronectes americanus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Transient Mackerel ( +29cm)"),"scientific_name"] <- "Scomber scombrus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Lumpfish"),"scientific_name"] <- "Cyclopterus lumpus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Greenland cod"),"scientific_name"] <- "Gadus opac"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Salmon"),"scientific_name"] <- "Salmo salar"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Capelin"),"scientific_name"] <- "Mallotus villosus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Sandlance"),"scientific_name"] <- "Ammodytes dubius"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Arctic cod"),"scientific_name"] <- "Boreogadus saida"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Herring"),"scientific_name"] <- "Clupea harengus harengus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Shortfinned squid"),"scientific_name"] <- "Illex illecebrosus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Arctic Squid"),"scientific_name"] <- "Gonatus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Lobster"),"scientific_name"] <- "Lomarus americanus"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "L.Zooplankton"),"scientific_name"] <- "Large Zooplankton"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "S.Zooplankton"),"scientific_name"] <- "Small Zooplankton"
GroupName[[69]][which(GroupName[[69]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 70 - article OK
GroupName[[70]][which(GroupName[[70]][,"original_name"] == "Yellowfin"),"scientific_name"] <- "Thunnus albacares"
GroupName[[70]][which(GroupName[[70]][,"original_name"] == "Bluefin"),"scientific_name"] <- "Thunnus thynnus"
GroupName[[70]][which(GroupName[[70]][,"original_name"] == "Skipjack"),"scientific_name"] <- "Katsuwonus pelamis"
GroupName[[70]][which(GroupName[[70]][,"original_name"] == "Albacore"),"scientific_name"] <- "Thunnus alalunga"
GroupName[[70]][which(GroupName[[70]][,"original_name"] == "Bigeye"),"scientific_name"] <- "Thunnus obesus"
GroupName[[70]][which(GroupName[[70]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"
GroupName[[70]][which(GroupName[[70]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 71 - article OK
GroupName[[71]][which(GroupName[[71]][,"original_name"] == "Yellowfin"),"scientific_name"] <- "Thunnus albacares"
GroupName[[71]][which(GroupName[[71]][,"original_name"] == "Bluefin"),"scientific_name"] <- "Thunnus thynnus"
GroupName[[71]][which(GroupName[[71]][,"original_name"] == "Skipjack"),"scientific_name"] <- "Katsuwonus pelamis"
GroupName[[71]][which(GroupName[[71]][,"original_name"] == "Albacore"),"scientific_name"] <- "Thunnus alalunga"
GroupName[[71]][which(GroupName[[71]][,"original_name"] == "Bigeye"),"scientific_name"] <- "Thunnus obesus"
GroupName[[71]][which(GroupName[[71]][,"original_name"] == "Swordfish"),"scientific_name"] <- "Xiphias gladius"
GroupName[[71]][which(GroupName[[71]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 72 - article OK
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Minke whales"),"scientific_name"] <- "Balaenoptera acutorostrata"
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Fin whales"),"scientific_name"] <- "Balaenoptera physalus"
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Humpback whales"),"scientific_name"] <- "Megaptera novaeangliae"
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Brydes whales"),"scientific_name"] <- "Balaenoptera brydei"
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Sei whales"),"scientific_name"] <- "Balaenoptera borealis"
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Sperm whales"),"scientific_name"] <- "Physeter macrocephalus"
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Killer whales"),"scientific_name"] <- "Orcinus orca"
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[72]][which(GroupName[[72]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 73 - article OK
GroupName[[73]][which(GroupName[[73]][,"original_name"] == "Cormorant"),"scientific_name"] <- "CormorantPhalacrocorax carbo sinensis"
GroupName[[73]][which(GroupName[[73]][,"original_name"] == "Seabass"),"scientific_name"] <- "Dicentrarchus labrax"
GroupName[[73]][which(GroupName[[73]][,"original_name"] == "Eel"),"scientific_name"] <- "Anguilla anguilla"
GroupName[[73]][which(GroupName[[73]][,"original_name"] == "Seabeam"),"scientific_name"] <- "Sparus aurata"
GroupName[[73]][which(GroupName[[73]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[73]][which(GroupName[[73]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 74 - article OK
        # Only functionnal groups

# Web 75 - article MISSING

# Web 76 - article OK
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Sardine"),"scientific_name"] <- "Sardinipos sagax"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Mackerel"),"scientific_name"] <- "Scomber japonicus"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Horse Mackerel"),"scientific_name"] <- "Trachurus murphyi"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Hake"),"scientific_name"] <- "Merluccius gayi"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Anchoveta"),"scientific_name"] <- "Engraulis ringens"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Sea lion"),"scientific_name"] <- "Otaria fluvescens"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Fur seal"),"scientific_name"] <- "Arctocephalus australis"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Bonito"),"scientific_name"] <- "Sarda chiliensis"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Booby"),"scientific_name"] <- "Sula variegata"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Cormorant"),"scientific_name"] <- "Phalacrocorax bougainvillii"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Pelican"),"scientific_name"] <- "Pelecanus thagus"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[76]][which(GroupName[[76]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 77 - article OK
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Sardine"),"scientific_name"] <- "Sardinipos sagax"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Mackerel"),"scientific_name"] <- "Scomber japonicus"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Horse Mackerel"),"scientific_name"] <- "Trachurus murphyi"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Hake"),"scientific_name"] <- "Merluccius gayi"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Anchoveta"),"scientific_name"] <- "Engraulis ringens"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Sea lion"),"scientific_name"] <- "Otaria fluvescens"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Fur seal"),"scientific_name"] <- "Arctocephalus australis"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Bonito"),"scientific_name"] <- "Sarda chiliensis"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Booby"),"scientific_name"] <- "Sula variegata"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Cormorant"),"scientific_name"] <- "Phalacrocorax bougainvillii"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Pelican"),"scientific_name"] <- "Pelecanus thagus"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[77]][which(GroupName[[77]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 78 - article OK
GroupName[[78]][which(GroupName[[78]][,"original_name"] == "S. auratus"),"scientific_name"] <- "Sparus auratus"
GroupName[[78]][which(GroupName[[78]][,"original_name"] == "S. cantharus"),"scientific_name"] <- "Spondyliossoma cantharus"
GroupName[[78]][which(GroupName[[78]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[78]][which(GroupName[[78]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 79 - article OK
        # Only functionnal groups

# Web 80 - article OK
        # Only functionnal groups

# Web 81 - article OK
GroupName[[81]][which(GroupName[[81]][,"original_name"] == "Dicentrarchus labrax(fish)"),"scientific_name"] <- "Dicentrarchus labrax"
GroupName[[81]][which(GroupName[[81]][,"original_name"] == "Pomatoschistus microps (suprabenthos)"),"scientific_name"] <- "Pomatoschistus microps"
GroupName[[81]][which(GroupName[[81]][,"original_name"] == "Platichthys flesus (fish)"),"scientific_name"] <- "Platichthys flesus"
GroupName[[81]][which(GroupName[[81]][,"original_name"] == "Crangon crangon (suprabenthos)"),"scientific_name"] <- "Crangon crangon"
GroupName[[81]][which(GroupName[[81]][,"original_name"] == "Palaemon longirostris (suprabenthos)"),"scientific_name"] <- "Palaemon longirostris"
GroupName[[81]][which(GroupName[[81]][,"original_name"] == "Neomysis integer (suprabenthos)"),"scientific_name"] <- "Neomysis integer"
GroupName[[81]][which(GroupName[[81]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[81]][which(GroupName[[81]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

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
# GroupName[[87]][which(GroupName[[87]][,"original_name"] == "Cutlassfish"),"scientific_name"] <- "Trichiurus lepturus"
# GroupName[[87]][which(GroupName[[87]][,"original_name"] == "Hake"),"scientific_name"] <- "Merluccius hubbsi"
# GroupName[[87]][which(GroupName[[87]][,"original_name"] == ""),"scientific_name"] <- ""
# GroupName[[87]][which(GroupName[[87]][,"original_name"] == ""),"scientific_name"] <- ""
# GroupName[[87]][which(GroupName[[87]][,"original_name"] == ""),"scientific_name"] <- ""

# Web 88 - article MISSING

# Web 89 - article MISSING

# Web 90 - article OK
        # Only functionnal groups

# Web 91 - article OK
GroupName[[91]][which(GroupName[[91]][,"original_name"] == "E Fluviatilis"),"scientific_name"] <- "Ehirava fluviatilis"
GroupName[[91]][which(GroupName[[91]][,"original_name"] == "Hemiramphus sp"),"scientific_name"] <- "Hemiramphus"
GroupName[[91]][which(GroupName[[91]][,"original_name"] == "P.filamentosus"),"scientific_name"] <- "Puntius filamentosus"
GroupName[[91]][which(GroupName[[91]][,"original_name"] == "O. mossambicus"),"scientific_name"] <- "Oreochromis mossambicus"
GroupName[[91]][which(GroupName[[91]][,"original_name"] == "O. niloticus"),"scientific_name"] <- "Oreochromis niloticus"
GroupName[[91]][which(GroupName[[91]][,"original_name"] == "Amb.melettinus"),"scientific_name"] <- "Amblypharyngodon melettinus"

# Web 92 - article OK
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Halibut"),"scientific_name"] <- "Hippoglossus stenolepis"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Lingcod"),"scientific_name"] <- "Ophiodon elongatus"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Ad hake"),"scientific_name"] <- "Merluccius productus"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "<juv hake"),"scientific_name"] <- "Merluccius productus" # juvenile hake
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Ad Res coho"),"scientific_name"] <- "Oncorhynchus kisutch"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Juv Res coho"),"scientific_name"] <- ""
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Ad Res chinook"),"scientific_name"] <- "Oncorhynchus tshawytsc"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Juv Res chinook"),"scientific_name"] <- "Oncorhynchus tshawytsc"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Eulachon"),"scientific_name"] <- "Thaleichthys pacificus"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Ad herring"),"scientific_name"] <- "Clupea pallasii"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Juv herring"),"scientific_name"] <- "Clupea pallasii"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Carn zooplankton"),"scientific_name"] <- "Carnivorous zooplankton"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Herb zooplankton"),"scientific_name"] <- "Herbivorous zooplankton"
GroupName[[92]][which(GroupName[[92]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 93 - article OK
        # Only functionnal groups

# Web 94 - article OK
GroupName[[94]][which(GroupName[[94]][,"original_name"] == "B. chrysoura"),"scientific_name"] <- "Bairdiella chrysoura"
GroupName[[94]][which(GroupName[[94]][,"original_name"] == "D. auratus"),"scientific_name"] <- "Diapterus auratus"
GroupName[[94]][which(GroupName[[94]][,"original_name"] == "M. curema"),"scientific_name"] <- "Mugil curema"
GroupName[[94]][which(GroupName[[94]][,"original_name"] == "O. saurus"),"scientific_name"] <- "Oligoplitus saurus"
GroupName[[94]][which(GroupName[[94]][,"original_name"] == "Shrimp"),"scientific_name"] <- "Penaeus duorarum"
GroupName[[94]][which(GroupName[[94]][,"original_name"] == "S. notata"),"scientific_name"] <- "Strongylura notata"
GroupName[[94]][which(GroupName[[94]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[94]][which(GroupName[[94]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 95 - article OK
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "0-3 Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "3-8 Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "8-18 Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "18-36 Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "36+ Red Drum"),"scientific_name"] <- "Sciaenops ocellatus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "0-6 Mullet"),"scientific_name"] <- "Mugil cephalus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "6-18 Mullet"),"scientific_name"] <- "Mugil cephalus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "18+ Mullet"),"scientific_name"] <- "Mugil cephalus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Mackrel 0-3"),"scientific_name"] <- "Scomberomorus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Mackrel 3+"),"scientific_name"] <- "Scomberomorus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Ladyfish 0-10"),"scientific_name"] <- "Elops saurus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Ladyfish 10+"),"scientific_name"] <- "Elops saurus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Bay Anchovy"),"scientific_name"] <- "Anchoa mitchilli"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Pin Fish"),"scientific_name"] <- "Lagodon rhomboides"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Silver Perch"),"scientific_name"] <- "Bairdiella chrysoura"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Scaled Sardine"),"scientific_name"] <- "Harengula jaguana"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Menidia (silverside)"),"scientific_name"] <- "Menidia menidia"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Manhaden"),"scientific_name"] <- "Brevoortia patronus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Blue Crab"),"scientific_name"] <- "Callinectes sapidus"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Pigfish"),"scientific_name"] <- "Orthopristis chrysoptera"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Macro Zooplankton"),"scientific_name"] <- "Macro Zooplankton"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Micro Zoolplankton"),"scientific_name"] <- "Micro Zoolplankton"
GroupName[[95]][which(GroupName[[95]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 96 - article OK
GroupName[[96]][which(GroupName[[96]][,"original_name"] == "Snapper"),"scientific_name"] <- "Lutjanus griseus"
GroupName[[96]][which(GroupName[[96]][,"original_name"] == "Toadfish"),"scientific_name"] <- "Opsanus beta"
GroupName[[96]][which(GroupName[[96]][,"original_name"] == "Anchovy"),"scientific_name"] <- "Anchoa"
GroupName[[96]][which(GroupName[[96]][,"original_name"] == "White mullet"),"scientific_name"] <- "Mugil curema"
GroupName[[96]][which(GroupName[[96]][,"original_name"] == "Spadefish"),"scientific_name"] <- "Chaetodipterus faber"
GroupName[[96]][which(GroupName[[96]][,"original_name"] == "Oyster"),"scientific_name"] <- "Crassostrea virginica"
GroupName[[96]][which(GroupName[[96]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[96]][which(GroupName[[96]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 97 - article OK
        # Only functionnal groups

# Web 98 - article OK
        # Only functionnal groups

# Web 99 - article OK
        # Only functionnal groups

# Web 100 - article OK
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "P maculatus"),"scientific_name"] <- "Pimelodus maculatus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Hypostomus spp"),"scientific_name"] <- "Hypostomus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Schizodon borelii"),"scientific_name"] <- "Schizodon borelii"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Loricariichthys platymetopon"),"scientific_name"] <- "Loricariichthys platymetopon"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Prochilodus lineatus"),"scientific_name"] <- "Prochilodus lineatus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Cyphocharax modesta"),"scientific_name"] <- "Cyphocharax modesta"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Steindacchnerina insculpta"),"scientific_name"] <- "Steindacchnerina insculpta"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Iheringichthys labrosus"),"scientific_name"] <- "Iheringichthys labrosus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Schizodon altoparanae"),"scientific_name"] <- "Schizodon altoparanae"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Hypophthalmus edentatus"),"scientific_name"] <- "Hypophthalmus edentatus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Trachydoras paraguayensis"),"scientific_name"] <- "Trachydoras paraguayensis"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Hoplosternum littorale"),"scientific_name"] <- "Hoplosternum littorale"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Leporinus friderici"),"scientific_name"] <- "Leporinus friderici"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Leporinus obtusidens"),"scientific_name"] <- "Leporinus obtusidens"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Parauchenipterus galeatus"),"scientific_name"] <- "Parauchenipterus galeatus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Pterodoras granulosus"),"scientific_name"] <- "Pterodoras granulosus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Astyanax altiparanae"),"scientific_name"] <- "Astyanax altiparanae"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Auchenipterus nuchalis"),"scientific_name"] <- "Auchenipterus nuchalis"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Brycon orbignyanus"),"scientific_name"] <- "Brycon orbignyanus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Acestrorhyncus lacustris"),"scientific_name"] <- "Acestrorhyncus lacustris"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Hoplias malabaricus"),"scientific_name"] <- "Hoplias malabaricus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Plagioscion squamisissimus"),"scientific_name"] <- "Plagioscion squamisissimus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Pseudoplatystoma corruscans"),"scientific_name"] <- "Pseudoplatystoma corruscans"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Rhaphiodon vulpinis"),"scientific_name"] <- "Rhaphiodon vulpinis"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Serrasalmus marginatus"),"scientific_name"] <- "Serrasalmus marginatus"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Hemisorubin platyrhynchos"),"scientific_name"] <- "Hemisorubin platyrhynchos"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Salminus brasiliensis"),"scientific_name"] <- "Salminus brasiliensis"
GroupName[[100]][which(GroupName[[100]][,"original_name"] == "Serrasalmus spiloptera"),"scientific_name"] <- "Serrasalmus spiloptera"

# Web 101 - article OK
        # Only functionnal groups

# Web 102 - article OK
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Cod 4+"),"scientific_name"] <- "Gadus morhua"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Cod juv"),"scientific_name"] <- "Gadus morhua"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Grl. halibut 5+"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Grl. halibut juv"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Redfish larger than 14cm"),"scientific_name"] <- "Sebastes" # > 14cm
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Redfish juv less 15cm"),"scientific_name"] <- "Sebastes" # < 15cm
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Polar cod"),"scientific_name"] <- "Boreogadus saida"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == " Thorny ray"),"scientific_name"] <- "Raja radiata"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Long rough Dab"),"scientific_name"] <- "Hippoglossoides platessoides"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Northern shrimp"),"scientific_name"] <- "Pandalus borealis"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Squids"),"scientific_name"] <- "Gonatus fabricii"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Large Zooplankton"),"scientific_name"] <- "Large Zooplankton"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Small Zooplankton"),"scientific_name"] <- "Small Zooplankton"
GroupName[[102]][which(GroupName[[102]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 103 - article OK
        # Only functionnal groups

# Web 104 - article OK
        # Only functionnal groups

# Web 105 - article OK
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Lingcod"),"scientific_name"] <- "Ophiodon elongatus"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Juv Lingcod"),"scientific_name"] <- "Ophiodon elongatus"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Dogfish"),"scientific_name"] <- "Squalus acanthias"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Adult Pacific Cod"),"scientific_name"] <- "Gadus macrocephalus"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Juv. Pacific Cod"),"scientific_name"] <- "Gadus macrocephalus"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Adult Herring"),"scientific_name"] <- "Clupea pallasii"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Juv. Herring"),"scientific_name"] <- "Clupea pallasii"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Eulachon"),"scientific_name"] <- "Thaleichthys pacificus"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Pink Shrimp"),"scientific_name"] <- "Pandalus jordani"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Herb. Zoolplankton"),"scientific_name"] <- "Herbivorous Zoolplankton"
GroupName[[105]][which(GroupName[[105]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 106 - article OK
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Sperm whales"),"scientific_name"] <- "Physeter macrocephalus"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Steller sea lions"),"scientific_name"] <- "Eumetopias jubatus"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Adult pollock"),"scientific_name"] <- "Theragra chalcogramma"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Juvenile pollock"),"scientific_name"] <- "Theragra chalcogramma"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Pacific cod"),"scientific_name"] <- "Gadus macrocephalus"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "P.halibut"),"scientific_name"] <- "Hippoglossus stenolepis"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Arrowtooth flounder"),"scientific_name"] <- "Atheresthes stomias"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Greenland turbot"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Tanner crab"),"scientific_name"] <- "Chionoecetes bairdi"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Snow crab"),"scientific_name"] <- "Chionoecetes opilio"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Pacific herring"),"scientific_name"] <- "Clupea pallasi"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Jellyfish"),"scientific_name"] <- "Chrysaora melenaster" # most abundant
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Large Zooplankton"),"scientific_name"] <- "Large Zooplankton"
GroupName[[106]][which(GroupName[[106]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 107 - article OK
GroupName[[107]][which(GroupName[[107]][,"original_name"] == "Span. mackerel"),"scientific_name"] <- "Scomberomorus maculatus"
GroupName[[107]][which(GroupName[[107]][,"original_name"] == "King mackerel"),"scientific_name"] <- "Scomberomorus cavalla"
GroupName[[107]][which(GroupName[[107]][,"original_name"] == "Red grouper"),"scientific_name"] <- "Epinephelus morio"
GroupName[[107]][which(GroupName[[107]][,"original_name"] == "Red snapper"),"scientific_name"] <- "Lutjanus campechanus"
GroupName[[107]][which(GroupName[[107]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[107]][which(GroupName[[107]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 108 - article OK
GroupName[[108]][which(GroupName[[108]][,"original_name"] == "Grl. Halibut"),"scientific_name"] <- "Reinhardtius hippoglossoides"
GroupName[[108]][which(GroupName[[108]][,"original_name"] == "Red fish"),"scientific_name"] <- "Sebastes"
GroupName[[108]][which(GroupName[[108]][,"original_name"] == "Northern shrimp"),"scientific_name"] <- "Pandalus borealis"
GroupName[[108]][which(GroupName[[108]][,"original_name"] == "Polar cod"),"scientific_name"] <- "Boreogadus suida"
GroupName[[108]][which(GroupName[[108]][,"original_name"] == "Cod"),"scientific_name"] <- "Gadus morhua"
GroupName[[108]][which(GroupName[[108]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[108]][which(GroupName[[108]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 109 - article OK
GroupName[[109]][which(GroupName[[109]][,"original_name"] == "Red grouper"),"scientific_name"] <- "Epinephelus morio"
GroupName[[109]][which(GroupName[[109]][,"original_name"] == "King mackerel"),"scientific_name"] <- "Scomberomorus cavalla"
GroupName[[109]][which(GroupName[[109]][,"original_name"] == "Zooplankton"),"scientific_name"] <- "Zooplankton"
GroupName[[109]][which(GroupName[[109]][,"original_name"] == "Phytoplankton"),"scientific_name"] <- "Phytoplankton"

# Web 110 - article OK
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Lagopus"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Snow_bunting"),"scientific_name"] <- "Plectrophenax nivalis"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Arctic_wolve"),"scientific_name"] <- "Canis lupus arctos"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"
GroupName[[110]][which(GroupName[[110]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"

# Web 111 - article OK
GroupName[[111]][which(GroupName[[111]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
GroupName[[111]][which(GroupName[[111]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus"
GroupName[[111]][which(GroupName[[111]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"

# Web 112 - article OK
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Voles"),"scientific_name"] <- "Microtus oeconomus"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Lagopus"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Red_fox"),"scientific_name"] <- "Vulpes vulpes"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Falcons"),"scientific_name"] <- "Falco"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Wolverine"),"scientific_name"] <- "Gulo gulo"
GroupName[[112]][which(GroupName[[112]][,"original_name"] == "Gulls"),"scientific_name"] <- "Larus"

# Web 113 - article OK
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Lagopus"
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Snow_bunting"),"scientific_name"] <- "Plectrophenax nivalis"
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiacus"
GroupName[[113]][which(GroupName[[113]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"

# Web 114 - article OK
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Muskoxen"),"scientific_name"] <- "Ovibos moschatus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Brown_lemmings"),"scientific_name"] <- "Lemnus trimucronatus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Voles"),"scientific_name"] <- "Microtus oeconomus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Lagopus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Red_fox"),"scientific_name"] <- "Vulpes vulpes"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Wolverine"),"scientific_name"] <- "Gulo gulo"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Grizzly_bear"),"scientific_name"] <- "Ursus horribilis"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiacus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
GroupName[[114]][which(GroupName[[114]][,"original_name"] == "Peregrine_falcon"),"scientific_name"] <- "Falco peregrinus"

# Web 115 - article OK
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Snow_goose"),"scientific_name"] <- "Anser caerulescens atlantica"
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Brown_lemmings"),"scientific_name"] <- "Lemnus trimucronatus"
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Collared_lemming"),"scientific_name"] <- "Dicrostonyx groenlandicus"
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiacus"
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Long-tailed_jaeger"),"scientific_name"] <- "Stercorarius longicaudus"
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Parasitic_jaeger"),"scientific_name"] <- "Stercorarius parasiticus"
GroupName[[115]][which(GroupName[[115]][,"original_name"] == "Peregrine_falcon"),"scientific_name"] <- "Falco peregrinus"

# Web 116 - article OK
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Voles"),"scientific_name"] <- "Microtus oeconomus"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Reindeer"),"scientific_name"] <- "Rangifer tarandus"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Arctic_hare"),"scientific_name"] <- "Lepus arcticus"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Ptarmigan"),"scientific_name"] <- "Lagopus"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Ermine"),"scientific_name"] <- "Mustela erminea"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Arctic_fox"),"scientific_name"] <- "Vulpes lagopus"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Red_fox"),"scientific_name"] <- "Vulpes vulpes"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Falcons"),"scientific_name"] <- "Falco"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Rough-legged_hawk"),"scientific_name"] <- "Buteo lagopus"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Snowy_owl"),"scientific_name"] <- "Bubo scandiacus"
GroupName[[116]][which(GroupName[[116]][,"original_name"] == "Brown_bear"),"scientific_name"] <- "Ursus horribilis"

# Write the list as a .Rdata file
saveRDS(GroupName, file = "data/list_names.RDS")
