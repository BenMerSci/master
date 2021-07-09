# Get scientific_name for each dataframe in the list based on their common_name
temp <- lapply(temp, function(x) {
        x$scientific_name <- as.character(taxize::comm2sci(x$common_name), db = "ncbi", key = Sys.getenv("ENTREZ_KEY"))
        return(x)
})

# If common_name is empty, fill it with original_name, for each data frame in the list
temp <- lapply(temp, function(x) {
        x[which(x$scientific_name %in% "character(0)"),"scientific_name"] <- x[which(x$scientific_name %in% "character(0)"), "common_name"]
        return(x)
})
