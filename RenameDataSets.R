
RenameDataSets01 <- function ()
	{

	rename_matrix <- matrix (c(
	'Minidot_02', 'Avon', 'GA2', 
	'Minidot_03', 'Wylye', 'CW2',
	'Minidot_04', 'Sem', 'AS2',
	'Minidot_05', 'Nadder', 'GN1',
	'Minidot_06', 'Ebble', 'CE1',
	'Minidot_07', 'Sem', 'AS1',
	'Minidot_08', 'Avon', 'GA2'),
	 ncol = 3, byrow = TRUE )


	#for ( i in 1:dim(rename_matrix)[1] )
	for ( i in 2:(dim(rename_matrix)[1]-1) )
	#for ( i in 1)
		{
		
		do.call ( data, list (rename_matrix[i,1]))
		x1 <- get (rename_matrix[i,1])$date1[1]
		print (x1)
		x1 <- gsub ( '-', '_', x1)
		c_name <- paste ( rename_matrix[i,2], '_', rename_matrix[i,3], '_', x1, '_md', sep = '' )
		print (c_name)
		assign (c_name, get ( rename_matrix[i,1]), envir = .GlobalEnv)
		setwd (dirdmp)
		#save (list  = get(c_name), file = paste (c_name, '.rda', sep = ''))
		#save ( list (get(c_name)), file = paste (c_name, '.rda', sep = ''))
		save ( list  = c_name, file = paste (c_name, '.rda', sep = ''))
		}

	
	
	for ( i in 1)
		{
		
		do.call ( data, list (rename_matrix[i,1]))
		x1 <- get (rename_matrix[i,1])$date1[1]
		x1 <- gsub ( '-', '_', x1)
		print (x1)
		c_name <- paste ( rename_matrix[i,2], '_', rename_matrix[i,3], '_ER_Ks', sep = '' )
		print (c_name)
		assign (c_name, get ( rename_matrix[i,1]), envir = .GlobalEnv)
		setwd (dirdmp)
		save ( list  = c_name, file = paste (c_name, '.rda', sep = ''))
		}
	}

#library(mdot)
#RenameDataSets01()


#trash:///
#Семен Злотников «Пришёл мужчина к женщине»




















































RenameDataSets <- function ()
	{

	rename_matrix <- matrix (c(
	'Minidot_02', 'Avon', 'GA2', 
	'Minidot_03', 'Wylye', 'CW2',
	'Minidot_04', 'Sem', 'AS2',
	'Minidot_05', 'Nadder', 'GN1',
	'Minidot_06', 'Ebble', 'CE1',
	'Minidot_07', 'Sem', 'AS1',
	'Minidot_08', 'Avon', 'GA2'),
	 ncol = 3, byrow = TRUE )


	#for ( i in 1:dim(rename_matrix)[1] )
	for ( i in 2:(dim(rename_matrix)[1]-1) )
	#for ( i in 1)
		{
		
		do.call ( data, list (rename_matrix[i,1]))
		x1 <- get (rename_matrix[i,1])$date1[1]
		print (x1)
		c_name <- paste ( rename_matrix[i,2], '_', rename_matrix[i,3], '_', x1, '_md', sep = '' )
		print (c_name)
		assign (c_name, get ( rename_matrix[i,1]), envir = .GlobalEnv)
		setwd (dirdmp)
		#save (list  = get(c_name), file = paste (c_name, '.rda', sep = ''))
		#save ( list (get(c_name)), file = paste (c_name, '.rda', sep = ''))
		save ( list  = c_name, file = paste (c_name, '.rda', sep = ''))
		}

	
	
	for ( i in 1)
		{
		
		do.call ( data, list (rename_matrix[i,1]))
		x1 <- get (rename_matrix[i,1])$date1[1]
		print (x1)
		c_name <- paste ( rename_matrix[i,2], '_', rename_matrix[i,3], '_ER_Ks', sep = '' )
		print (c_name)
		assign (c_name, get ( rename_matrix[i,1]), envir = .GlobalEnv)
		setwd (dirdmp)
		save ( list  = c_name, file = paste (c_name, '.rda', sep = ''))
		}
	}

#library(mdot)
#RenameDataSets()


#trash:///
#Семен Злотников «Пришёл мужчина к женщине»
