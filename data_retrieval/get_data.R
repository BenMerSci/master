library(tabulizer)
library(stringr)

chesapeake_file <- list.files(pattern = "chesapeake")

chesapeake_1 <- extract_tables(chesapeake_file, pages = 13, output = "data.frame")
chesapeake_2 <- extract_tables(chesapeake_file, pages = 14, output = "data.frame")

test <- extract_tables(chesapeake_file, method = "stream", pages = 14, area = list(c(65.29870, 64.65827, 403.79221, 544.53543)), output = "data.frame")


benguela_file <- list.files(pattern = "benguela")
benguela_1 <- extract_tables(benguela_file, pages = 21, output = "data.frame")
test <- as.matrix(benguela_1)
test <- gsub("9=9", 0, benguela_1)
test <- str_replace(test, "9=9", "0")

#########################

library(RCurl)
library(XML)
library(plyr)



#To obtain the list of available model
h=basicTextGatherer()
curlPerform(url = 'http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php',writefunction=h$update)

data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)
liste_mod<-ldply(xmlToList(data),data.frame)
        
#liste_mod contains a list and decription of available models in EcoBase      
temp <- liste_mod[liste_mod$model.whole_food_web == "true",]
temp <- liste_mod[liste_mod$model.model_name == "Chesapeake",]

h=basicTextGatherer()
mymodel<-229
curlPerform(url = paste('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php?no_model=',mymodel,sep=''),writefunction=h$update,verbose=TRUE)


data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)

input1<-xpathSApply(data,'//group',function(x) xmlToList(x))


h=basicTextGatherer()
mymodel<-200

curlPerform(url = paste('http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php?no_model=',mymodel,sep=''),writefunction=h$update,verbose=TRUE)


data<-xmlTreeParse(h$value(),useInternalNodes=TRUE)

output1<-xpathSApply(data,'//group',function(x) xmlToList(x))

