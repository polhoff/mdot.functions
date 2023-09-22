

QualityFilterFnc <- function (indata, n_diff = 0.1,n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1, n_deviation = 0.01, varnames = c('DO','DO_diff','DO_diff_diff','mv_avg_diff','st.dev'))
	{
	
	pick_vars <- names(indata) %in% varnames
	data_sub <- indata[,pick_vars]
	
	mv_avg <- filter (data_sub[,1], rep(1/10,10), sides=2)

	
	qc1 <- rep(0, dim(data_sub)[1])
	qc2 <- rep(0, dim(data_sub)[1])
	qc3 <- rep(0, dim(data_sub)[1])
	qc4 <- rep(0, dim(data_sub)[1])
	qc5 <- rep(0, dim(data_sub)[1])
	
	
	qc1[abs(data_sub$DO_diff) < n_diff] <- 1
	qc2[abs(data_sub$DO_diff_diff) < n_diff_diff] <- 1
	qc3[abs(data_sub$mv_avg_diff) < n_mv_diff] <- 1
	qc4[abs(data_sub$st.dev) < n_sd_diff] <- 1
	qc5[abs(data_sub[,1] - mv_avg) < n_deviation] <- 1
	
	#qc1 <- as.integer(abs(data_sub$DO_diff) < n_diff)
	#qc2 <- as.integer(abs(data_sub$DO_diff_diff) < n_diff_diff)
	#qc3 <- as.integer(abs(data_sub$mv_avg_diff) < n_mv_diff)
	#qc4 <- as.integer(abs(data_sub$st.dev) < n_sd_diff)
	#qc5 <- as.integer(abs(data_sub[,1] - mv_avg) < n_deviation)

	qualityFilter <- qc1 * 1 + qc2 * 2 + qc3 * 4 + qc4 * 8 + qc5 * 16
	
	#avoids having to filter by > -1, which is conceptually clumsy
	qualityFilter01 <- qualityFilter + 1

	return(qualityFilter01)
	}



#x <- QualityFilterFnc()
#x <- QualityFilterFnc(indata=indata)
#QualityFilterFnc (indata=indata, n_diff = 0.1, n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1)
#x <- QualityFilterFnc (indata=indata, n_diff = 0.00001, n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1)
























































QualityFilterFncOrig <- function (indata, n_diff = 0.1,n_diff_diff = 0.1,n_mv_diff = 0.1,n_sd_diff = 0.1, varnames = c('DO','DO_diff','DO_diff_diff','mv_avg_diff','st.dev'))
	{
	
	pick_vars <- names(indata) %in% varnames
	data_sub <- indata[,pick_vars]
	
	qc1 <- rep(0, dim(data_sub)[1])
	qc2 <- rep(0, dim(data_sub)[1])
	qc3 <- rep(0, dim(data_sub)[1])
	qc4 <- rep(0, dim(data_sub)[1])
	
	
	qc1[abs(data_sub$DO_diff) < n_diff] <- 1
	qc2[abs(data_sub$DO_diff_diff) < n_diff_diff] <- 1
	qc3[abs(data_sub$mv_avg_diff) < n_mv_diff] <- 1
	qc4[abs(data_sub$st.dev) < n_sd_diff] <- 1
	

	qualityFilter <- qc1 * 1 + qc2 * 2 + qc3 * 4 + qc4 * 8
	qualityFilter01 <- qualityFilter + 1

	return(qualityFilter01)
	}



#x <- QualityFilterFnc()
#x <- QualityFilterFnc(indata=indata)
#QualityFilterFnc (indata=indata, n_diff = 0.1, n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1)
#x <- QualityFilterFnc (indata=indata, n_diff = 0.00001, n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1)














































#quality filter with 5 variables

QualityFilterFncVar05 <- function (indata, n_diff = 0.1,n_diff_diff = 0.1,n_mv_diff = 0.1,n_sd_diff = 0.1, n_deviation = 0.01, varnames = c('DO','DO_diff','DO_diff_diff','mv_avg_diff','st.dev'))
	{
	
	pick_vars <- names(indata) %in% varnames
	data_sub <- indata[,pick_vars]

	mv_avg <- filter (data_sub[,1], rep(1/10,10), sides=2)
	
	qc1 <- rep(0, dim(data_sub)[1])
	qc2 <- rep(0, dim(data_sub)[1])
	qc3 <- rep(0, dim(data_sub)[1])
	qc4 <- rep(0, dim(data_sub)[1])
	qc5 <- rep(0, dim(data_sub)[1])
	
	
	qc1[abs(data_sub$DO_diff) < n_diff] <- 1
	qc2[abs(data_sub$DO_diff_diff) < n_diff_diff] <- 1
	qc3[abs(data_sub$mv_avg_diff) < n_mv_diff] <- 1
	qc4[abs(data_sub$st.dev) < n_sd_diff] <- 1
	qc5[abs(data_sub[,1] - mv_avg) < n_deviation] <- 1
	

	qualityFilter <- qc1 * 1 + qc2 * 2 + qc3 * 4 + qc4 * 8 + qc5 * 16
	qualityFilter01 <- qualityFilter + 1

	return(qualityFilter01)
	}



#x <- QualityFilterFnc()
#x <- QualityFilterFnc(indata=indata)
#QualityFilterFnc (indata=indata, n_diff = 0.1, n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1)
#x <- QualityFilterFnc (indata=indata, n_diff = 0.00001, n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1)




























































QualityFilterFnc01 <- function (data1, n_crit1 = n_diff, n_crit2 = n_diff_diff, n_mv_sample = 10, n_crit3 = n_mv_diff, n_crit4 = n_sd_diff, n_crit5 = n_deviation, varnames = c('DO','DO_diff','DO_diff_diff','mv_avg_diff','st.dev'), n_return = 1)
	{
	
	pick_vars <- names(data1) %in% varnames
	data1 <- data1[,pick_vars]
	
	mv_avg <- filter (data1[,1], rep(1/n_mv_sample,n_mv_sample), sides=2)

	
	qc1 <- rep(NA, dim(data1)[1])
	qc2 <- rep(NA, dim(data1)[1])
	qc3 <- rep(NA, dim(data1)[1])
	qc4 <- rep(NA, dim(data1)[1])
	qc5 <- rep(NA, dim(data1)[1])
	
	
	qc1 <- as.integer(abs(data1$DO_diff) < n_crit1)
	qc2 <- as.integer(abs(data1$DO_diff_diff) < n_crit2)
	qc3 <- as.integer(abs(data1$mv_avg_diff) < n_crit3)
	qc4 <- as.integer(abs(data1$st.dev) < n_crit4)
	qc5 <- as.integer(abs(data1[,1] - mv_avg) < n_crit5)
	

	qualityFilter <- qc1 * 1 + qc2 * 2 + qc3 * 4 + qc4 * 8 + qc5 * 16
	#print ( table (qualityFilter))

	
	#qualityFilter <- as.factor(qualityFilter, levels = 1:32, labels = c('x00000x',	'x00001x',	'x00010x',	'x00011x',	'x00100x',	'x00101x',	'x00110x',	'x00111x',	'x01000x',	'x01001x',	'x01010x',	'x01011x',	'x01100x',	'x01101x',	'x01110x',	'x01111x',	'x10000x',	'x10001x',	'x10010x',	'x10011x',	'x10100x',	'x10101x',	'x10110x',	'x10111x',	'x11000x',	'x11001x',	'x11010x',	'x11011x',	'x11100x',	'x11101x',	'x11110x',	'x11111x'))
	

	#print ( table (qualityFilter))

	
	#qualityFilterBin <- paste('x', qc5, qc4, qc3, qc2, qc1, 'x', sep = '')
	#print ( table (qualityFilter))

	#.....so that qualityFilter01 > 0 would select all data points
	#avoids having to filter by > -1, which is conceptually clumsy
	qualityFilter01 <- qualityFilter + 1
	
	if(n_return == 1)
		{
		return(qualityFilter01)
		}
	
	if(n_return == 2)
		{
		return(qualityFilterBin)
		}
		
	}



#x <- QualityFilterFnc()
#x <- QualityFilterFnc(indata=indata)
#QualityFilterFnc (indata=indata, n_diff = 0.1, n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1)
#x <- QualityFilterFnc (indata=indata, n_diff = 0.00001, n_diff_diff = 0.1,n_mv_sample = 10,n_mv_diff = 0.1,n_sd_diff = 0.1)





