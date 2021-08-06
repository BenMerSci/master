# Resolve the taxonomy
 data/resolved_inter_table.RDS data/ data/resolved_names.RDS : R/resolve_taxonomy.R data/inter_table.RDS
	Rscript R/resolve_taxonomy.R

# Produce the basic interaction table
data/inter_table.RDS : R/format_matrix.R data_retrieval/ecopath/Data_ncomms12573/DIET.Rdata data/list_names.RDS
	Rscript R/format_matrix.R

# Name correction
data/list_names.RDS : R/format_name.R data_retrieval/ecopath/Data_ncomms12573/GroupName.Rdata
	Rscript R/format_name.R