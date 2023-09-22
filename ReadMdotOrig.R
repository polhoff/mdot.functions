
ReadMdotMeta <- function ()
	{
	indir <- paste ( dirtop, 'Rpackage/mdot/', sep = '' )
	
	meta_data_mdot = read.csv( paste ( indir, 'meta_data_mdot.csv', sep = ''), stringsAsFactors = F)
	meta_data_mdot$start_date <- as.Date (meta_data_mdot$start_date)
	meta_data_mdot$end_date <- as.Date (meta_data_mdot$end_date)
	
	setwd (dirdmp)
	save (meta_data_mdot, file = 'meta_data_mdot.rda')
	}

#ReadMdotMeta ()
#library(parker)
#x1 <- GetIndata('Nadder_GN1_2014_08_20_md', 'mdot.data')
#x1 <- GetIndata('Sem_AS1_2014_08_21_md', 'mdot.data')
#x1 <- GetIndata('Nadder_GN1_2014_08_20_md', 'mdot.data')
#x1 <- GetIndata('Nadder_GN1_2014_08_20_md', 'mdot.data')
#x1 <- GetIndata('Nadder_GN1_2014_08_20_md', 'mdot.data')
#x1 <- GetIndata('Nadder_GN1_2014_08_20_md', 'mdot.data')
#ReadMdotMeta ()
