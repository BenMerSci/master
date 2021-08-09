# Get the environment data
data/enviro_df.RDS : R/environment_traits.R data/ecopath_metadata.RDS data_retrieval/ecopath/Data_ncomms12573/Ecopath_models.Rdata  
	Rscript R/environment_traits.R

# Script to get the ecopath 
data/ecopath_metadata.RDS : R/get_ecopathdv.R data/name_match.csv data/resolved_inter_table.RDS
	Rscript R/get_ecopathdb.R

# Resolve the taxonomy
 data/resolved_inter_table.RDS data/resolved_names.RDS : R/resolve_taxonomy.R data/inter_table.RDS
	Rscript R/resolve_taxonomy.R

# Produce the basic interaction table
data/inter_table.RDS : R/format_matrix.R data_retrieval/ecopath/Data_ncomms12573/DIET.Rdata data/list_names.RDS
	Rscript R/format_matrix.R

# Name correction
data/list_names.RDS : R/format_name.R data_retrieval/ecopath/Data_ncomms12573/GroupName.Rdata
	Rscript R/format_name.R