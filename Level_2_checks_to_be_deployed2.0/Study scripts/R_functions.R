
#####################################################################################################################################################
## CONCEPTION LEVEL 2 CHECKS - R FUNCTIONS
## 24-06-2021
#####################################################################################################################################################

	## General: The functions below can be called to perform the level 2 checks (eight in total) in the CONCEPTION project.
	## With the exception of check 2.8, two functions are used per check: An 'inner' function, in which the actual check is performed
	## and an 'outer' function, which loops over the tables/columns, each time calling the inner function.

	## Updates in version of 26-03-2021:
	## - Note: At this point, there is not yet a plot in the markdown reports, this is to be done later.
	## - Adjustment compared to previous version:
	## 	- All: Explicitly set person_id to character to avoid it being automatically set to numeric when the ID field has only numbers
	## 	- All: All output tables (Results, Results_aggr and their copies with small counts hidden) are now generated at once.
	## 	- All: Some small changes in the script (e.g. object names and added descriptions) to improve clarity
	## 	- 2.1: Can now handle missing day and/or month of birth 
	## 	- 2.2: Can now handle missing day and/or month of death
	## 	- 2.3: Can now handle missing OP end dates (ie if OP end date is NA but the start date is available, the OP is considered to be still ongoing)
	## 	- 2.4: Possibly contained a bug when running the code in chunks. The function used to count how many unique person IDs were not in the PERSONS table
	##		but now it counts how many rows in the data have a patient ID that is not in the PERSONS table. This makes it easier to run the code in chunks
	##		and also makes it consistent with the warnings (which already counted rows).
	## 	- 2.5: Small change in order (in current version: first transform to date, then check if NA instead of vice versa)
	## 	- 2.6: - Previous version contained a bug when transforming results to class date (Note: This does affect the results in the simulated data(!)). Check if this affected the results
	##	    - Small change in order (in current version: first transform to date, then check if NA instead of vice versa)
	## 	- 2.7: No changes other than the small changes mentioned above
	## 	- 2.8: - Missing day / month of birth are imputed with july 2nd or with 15 and july, see code for details.
	## 	    - Note: The documentation is unclear as to the direction of the relation:
	##			Is 'person_id' the 'meaning_of_relationship' of 'related_id', or is 'related_id' the 'meaning of relationship' of 'person_id'
	## 			I assumed the latter, because this is consistent with the dummy data.
	##
	## Updates of 04-04-2021 version compared to version of 26-03-2021:
	## - Checks 2.1 to 2.4 now include the SURVEY_ID table. 2.5 to 2.7 do not, because there is no visit_occurrence_id in the SURVEY_ID table.
	## - 2.3: When end date is missing, the program now uses the creation date from CDM source, not the system date.
	## - Percentages are now included in ouotput tables (note: these are calculated later, in the markdown files, not in this script).
	##
	## Updates of 16-04-2021 version compared to version of 04-04-2021:
	## - In the outer functions of the checks, character values equal to ' ' or '' are set to NA. The command contains a call to any(), but the call didn't specify na.rm = T,
	##	causing a 'missing value where TRUE/FALSE needed'-error in some cases. This is now fixed.
	## - Adjusted folder names (+ corresponding adjustment in to_run.R)
	## - Adjusted bug for setting totals <5 to '<5'
	##
	## Updates of 19-04-2021 version compared to version of 16-04-2021:
	## - Added graphs in the .rmd files
	## - Changed output format of check 2.8 to data.frame (instead of data.table) for consistency with other functions.
	##
	## Updates of 23-04-2021 versin compared to version of 19-04-2021:
	## - Fixed typo in one of the .rmd files
	## - In the .rmd files, added suprressWarnigns() when calculating percentages (to avoid warning message when a '<5' is in the data which of course cant be set to numeric). 	
	##
	## Updates of 14-05-2021 version compared to version of 23-02-2021:
	## - The import of the CDM source is relocated to to_run file. In addition, the CDM source is exported to the output as is. 
	##
	## Updates of 24-06-2021 version compared to version of 14-05-2021:
	## - In 2.1, 2.2, 2.3: Added so_date from SURVEY_OBSERVATIONS
	## - In 2.4: Added SURVEY_OBSERVATIONS
	## - Note: SURVEY_OBSERVATIONS is not included in 2.5, 2.6 and 2.7 because it does not contain a visit occurrence ID
	## - In the to_run file, there are now checks included to see if PERSONS, PERSON_RELATIONSHIPS, VISIT_OCCURRENCE and OBSERVATION_PERIODS. If one of these files
	##	is missing, the checks that require them will be skipped and in that case a warning mssage will be prompted.


	## Packages and settings
	if(!require('data.table')) install.packages('data.table', repos = 'https://cloud.r-project.org')
	library('data.table')
	
	## Set the size of the data chunks used when running the code (to avoid memory issues with large data files)
	NB <- 1000000	

	## (for during development:)
	## dir.data <- 'Y:/Studies/ConcePTION/B_Documentation/2 Protocol_DSMB_Monitoring/WP 7.6 Data Characterization SAP/tmp_RB/Level 2 checks 20210624/ConcePTION/CDMInstances/Data Characterisation'
	## dir.data <- 'Y:/Studies/ConcePTION/B_Documentation/2 Protocol_DSMB_Monitoring/WP 7.6 Data Characterization SAP/tmp_RB/Level 2 checks 20210624/ConcePTION/CDMInstances/ConcePTION'

##################################################################################################################################################
## CHECK 2.1
##################################################################################################################################################

	## Description: This function checks, for specified date values in specified tables (see construction of object 'm' for details), whether
	## those date values occur before the date of birth of that person as specified in the PERSONS table. If the day of birth or the month of birth
	## is missing (but the year of birth is not missing), the day/month are conservatively set to 1. 

	##################################################################################################################################################
	## 2.1. 'Inner' function
	##################################################################################################################################################

	## Inner function check 2.1
	fun_check_2.1 <- function(d.PERSONS, d.DATES, v.DATES, v.MEANING){

		## INPUT CHECKS
		w1 <- w2 <- NA

		## Variables required
		var.x <- c('person_id', v.DATES, v.MEANING)
		var.y <- c('person_id', 'day_of_birth', 'month_of_birth', 'year_of_birth')

		## Check
		if(any(!(var.y %in% colnames(d.PERSONS))) | any(!(var.x %in% colnames(d.DATES)))){
			w1 <- 'At least one required column is either not in the data or is named incorrectly. Execution halted.'
			return(list(Warnings = w1, Results = data.frame(Meaning = NA, issues = NA, total = NA)))
			}

		## n.pre
		n.pre <- nrow(d.DATES)

		## Set person_id variables to character in both datasets
		d.PERSONS[['person_id']] <- as.character(d.PERSONS[['person_id']])
		d.DATES[['person_id']] <- as.character(d.DATES[['person_id']])

		## If meaning == NA, replace with (NOT ENTERED)
		d.DATES[,(v.MEANING) := ifelse(is.na(get(v.MEANING)), '(NOT ENTERED)', get(v.MEANING))]
		uniq.meaning <- unique(d.DATES[[v.MEANING]])

		## Remove missing values
		d.DATES <- d.DATES[!is.na(get(v.DATES)) & !is.na(person_id),]
		d.PERSONS <- d.PERSONS[!is.na(person_id),]

		## Left join d.PERSONS to d.DATES
		d.DATES <- merge(d.DATES[, ..var.x], d.PERSONS[, ..var.y], by = 'person_id', all.x = T)

		## If year_of_birth is available, but day_of_birth is missing, set day_of_birth to 1 (i.e. a conservative imputation for this check)
		d.DATES$day_of_birth[!is.na(d.DATES$year_of_birth) & is.na(d.DATES$day_of_birth)] <- 1

		## If year_of_birth is available, but month_of_birth is missing, set month_of_birth to 1 (i.e. a conservative imputation for this check)
		d.DATES$month_of_birth[!is.na(d.DATES$year_of_birth) & is.na(d.DATES$month_of_birth)] <- 1

		## Create date_birth
		d.DATES[, date_birth := paste(year_of_birth, month_of_birth, day_of_birth, sep = '-')] 
		d.DATES[, c('year_of_birth', 'month_of_birth', 'day_of_birth') := NULL]
		d.DATES[,date_birth := as.Date(as.character(d.DATES[['date_birth']]), format = '%Y-%m-%d')]

		## Set date values to class date, if not already done
		if(class(d.DATES[[v.DATES]]) != 'Date') d.DATES[,(v.DATES) := as.Date(as.character(d.DATES[[v.DATES]]), format = '%Y%m%d')]
		
		## Remove rows with missing values
		d.DATES <- d.DATES[complete.cases(d.DATES),]
		
		## n.post
		n.post <- nrow(d.DATES)

		## Warning
		if(n.pre != n.post) w2 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')
		
		## issues per meaning
		Results <- d.DATES[, .(issues = sum(get(v.DATES) < date_birth), total = .N), by = c(v.MEANING)]
		colnames(Results)[colnames(Results) == v.MEANING] <- 'meaning'
		
		## If Results is empty, replace by NAs
		if(nrow(Results) == 0){
			if(length(uniq.meaning) == 0) uniq.meaning <- '(NOT ENTERED)'
			Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = 0, total = 0))
			}
		
		## Return output
		return(list(Warnings = data.table(n.pre = n.pre, n.post = n.post), Results = Results))

		## End function
		}

	##################################################################################################################################################
	## 2.1. 'Outer' function
	##################################################################################################################################################

	## Outer function check 2.1
	run_check_2.1 <- function(dir.data, nb_row = NB){
	
		## If dir.data doesn't end with /, add it
		if(substr(dir.data, nchar(dir.data), nchar(dir.data)) != '/') dir.data <- paste0(dir.data, '/')

		## Get all .csv files in dir.data
		files <- dir(dir.data, pattern = '.csv')

		## Stop if PERSONS.csv is not available
		if(!any(grepl('PERSONS', files))) stop('PERSONS.csv is not in dir.data. Execution halted')

		## Combine info in one matrix: list all EVENTS, MEDICINES, PROCEDURES, VACCINES and MEDICAL_OBSERVATIONS tables (d), variables of interest (v) and the name of the 'meaning' column
		m <- rbind(
			expand.grid(TABLE = 'EVENTS', d.DATES = files[grepl('^EVENTS', files)], 					v.DATES = c('start_date_record'), 				v.MEANING = 'meaning_of_event', 		stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICINES', d.DATES = files[grepl('^MEDICINES', files)], 				v.DATES = c('date_dispensing', 'date_prescription'), 	v.MEANING = 'meaning_of_drug_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'PROCEDURES', d.DATES = files[grepl('^PROCEDURES', files)], 				v.DATES = c('procedure_date'), 				v.MEANING = 'meaning_of_procedure', 	stringsAsFactors = F),
			expand.grid(TABLE = 'VACCINES', d.DATES = files[grepl('^VACCINES', files)], 					v.DATES = c('vx_record_date', 'vx_admin_date'), 	v.MEANING = 'meaning_of_vx_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICAL_OBSERVATIONS', d.DATES = files[grepl('^MEDICAL_OBSERVATIONS', files)], 	v.DATES = c('mo_date'), 					v.MEANING = 'mo_meaning', 			stringsAsFactors = F),
			expand.grid(TABLE = 'SURVEY_ID', d.DATES = files[grepl('^SURVEY_ID', files)], 				v.DATES = c('survey_date'), 					v.MEANING = 'survey_meaning', 		stringsAsFactors = F),
			expand.grid(TABLE = 'SURVEY_OBSERVATIONS', d.DATES = files[grepl('^SURVEY_OBSERVATIONS', files)], 	v.DATES = c('so_date'), 					v.MEANING = 'so_meaning', 			stringsAsFactors = F)
			)

		## Output objects
		Warnings <- vector('list', length = nrow(m))
		Results <- vector('list', length = nrow(m))

		## Import d.PERSONS (+ basic check if input is correctly formatted). Replace character vallues '' and ' ' by NA
		d.PERSONS <- IMPORT_PATTERN(pat = "PERSONS", dir = dir.data, colls = c('person_id', 'day_of_birth', 'month_of_birth', 'year_of_birth'))
		d.PERSONS.colnames <- colnames(d.PERSONS)
		lapply(c('day_of_birth', 'month_of_birth', 'year_of_birth'), function(x) d.PERSONS <- d.PERSONS[, eval(x) := as.numeric(get(x))])
		if(ncol(d.PERSONS) == 1 | length(colnames(d.PERSONS)) != length(d.PERSONS.colnames)) stop('Problem with importing PERSONS.csv, please check if formatted correctly.') 
		
		#Removed because this is done within import_pattern
		###
		#which.char <- which(sapply(1:ncol(d.PERSONS), FUN = function(i) class(d.PERSONS[[i]]) == 'character'))
		#for(i in which.char) if(any(d.PERSONS[[i]] == ' ' | d.PERSONS[[i]] == '', na.rm = T)) d.PERSONS[[i]][!is.na(d.PERSONS[[i]]) & (d.PERSONS[[i]] == ' ' | d.PERSONS[[i]] == '')] <- NA
		#rm(which.char)
    ###
		
		## If d.PERSONS is empty, give an error
		if(nrow(d.PERSONS) == 0) stop('PERSONS.csv is empty.')
	
		## Start for loop
		for(i in 1:nrow(m)){

			## Read data file i + some basic checks
			d.DATES <- fread(paste0(dir.data, m$d.DATES[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)
			d.DATES.colnames <- colnames(fread(paste0(dir.data, m$d.DATES[i]), sep = ',', nrows = 0, stringsAsFactors = FALSE))
			if(ncol(d.DATES) == 1 | length(colnames(d.DATES)) != length(d.DATES.colnames)) stop(paste0('Problem with importing ', m$d.DATES[i], ', please check if formatted correctly.'))
			if(nrow(d.DATES) == 0) next 	## If the data file is empty - continue to next		
		
			## Split data in 'numchunks' chunks to limit memory issues
			numchunks <- ceiling(nrow(d.DATES) / nb_row)
			all.Results <- list()		## Temporarily store output		
			all.Warnings <- list()		## Temporarily store output

			## Start for loop per chunk
			for(k in 1:numchunks){
				  
				## Select chunk
				if(k < numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):(k * nb_row),]
				if(k == numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):nrow(d.DATES),]
			
				## Formatting empty strings
				which.char <- which(sapply(1:ncol(tmp.d.DATES), FUN = function(j) class(tmp.d.DATES[[j]]) == 'character'))
				for(j in which.char) if(any(tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '', na.rm = T)) tmp.d.DATES[[j]][!is.na(tmp.d.DATES[[j]]) & (tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '')] <- NA
			
				## Run inner function on chunk
				tmp.Out <- fun_check_2.1(d.PERSONS = d.PERSONS, d.DATES = tmp.d.DATES, v.DATES = m$v.DATES[i], v.MEANING = m$v.MEANING[i])
				all.Results[[k]] <- as.data.table(tmp.Out["Results"])
				all.Warnings[[k]] <- as.data.table(tmp.Out["Warnings"])
				
				## Remove objects  
				rm(tmp.d.DATES, tmp.Out) 
				}			

			## Combine the chunk-specific results and warnings
			all.Results <- do.call(rbind, all.Results)
			all.Results <- all.Results[, lapply(.SD, sum), .SDcols = c('Results.issues', 'Results.total'), by = Results.meaning] 
			colnames(all.Results) <- c('meaning', 'issues', 'total') 
			all.Warnings <- do.call(rbind, all.Warnings)
			all.Warnings <- all.Warnings[, lapply(.SD, sum), .SDcols = c('Warnings.n.pre', 'Warnings.n.post')] 
			all.Warnings <- paste('Unable to perform check for', all.Warnings[['Warnings.n.pre']] - all.Warnings[['Warnings.n.post']], 'out of', all.Warnings[['Warnings.n.pre']], 'rows.')
				
			## Store output
			tmp <- list(Results = all.Results, Warnings = all.Warnings)
			Warnings[[i]] <- as.data.frame(cbind(m[i, c('TABLE', 'd.DATES', 'v.DATES')], Warning = as.character(tmp$Warnings), stringsAsFactors = FALSE))
			Results[[i]] <- as.data.frame(cbind(m[i,][rep(seq_len(nrow(m[i,])), each = nrow(tmp$Results)),], tmp$Results, stringsAsFactors = FALSE))
	
			## Remove objects and close loop
			rm(d.DATES.colnames, d.DATES, tmp, which.char, all.Results, all.Warnings)
			}

		## Combine all results
		Warnings <- do.call(rbind, Warnings)
		Warnings <- Warnings[!is.na(Warnings$Warning),]
		Warnings <- Warnings[!grepl('Unable to perform check for 0 out of ', Warnings$Warning, fixed = TRUE),]	## Exclude non-issues
		Results <- do.call(rbind, Results)
		
		## Combine per combination of TABLE, v.DATES and Meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 3) colnames(tmp) <- c('TABLE', 'v.DATES', 'Meaning')
		if(ncol(tmp) != 3) stop('Incorrect number of columns when combining results.')
		Results_Comb <- cbind(tmp, Results_Comb)
		Results_Comb$id.tmp <- NULL
		rm(tmp1, tmp2, tmp)
		
		## Also aggregate over meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb_aggr <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb_aggr$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 2) colnames(tmp) <- c('TABLE', 'v.DATES')
		if(ncol(tmp) != 2) stop('Incorrect number of columns when combining results.')
		Results_Comb_aggr <- cbind(tmp, Results_Comb_aggr)
		Results_Comb_aggr$id.tmp <- NULL

		## If a count is lower than 5 but > 0, replace by '<5'
		Results_Comb_HideSmallCounts <- Results_Comb
		Results_Comb_HideSmallCounts$Issues[Results_Comb_HideSmallCounts$Issues < 5 & Results_Comb_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Total[Results_Comb_HideSmallCounts$Total < 5 & Results_Comb_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Issues <- as.character(Results_Comb_HideSmallCounts$Issues)
		Results_Comb_HideSmallCounts$Total <- as.character(Results_Comb_HideSmallCounts$Total)
		
		## Same for aggregated results
		Results_Comb_aggr_HideSmallCounts <- Results_Comb_aggr
		Results_Comb_aggr_HideSmallCounts$Issues[Results_Comb_aggr_HideSmallCounts$Issues < 5 & Results_Comb_aggr_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Total[Results_Comb_aggr_HideSmallCounts$Total < 5 & Results_Comb_aggr_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Issues <- as.character(Results_Comb_aggr_HideSmallCounts$Issues)
		Results_Comb_aggr_HideSmallCounts$Total <- as.character(Results_Comb_aggr_HideSmallCounts$Total)

		## Always set both to character for consistency
		Results_Comb$Issues <- as.character(Results_Comb$Issues)
		Results_Comb$Total <- as.character(Results_Comb$Total)
		Results_Comb_aggr$Issues <- as.character(Results_Comb_aggr$Issues)
		Results_Comb_aggr$Total <- as.character(Results_Comb_aggr$Total)

		## Return output and end function
		return(list(Warnings = Warnings, Results = Results_Comb, Results_HideSmallCounts = Results_Comb_HideSmallCounts, Results_aggr = Results_Comb_aggr, Results_aggr_HideSmallCounts = Results_Comb_aggr_HideSmallCounts))
		}


##################################################################################################################################################
## CHECK 2.2
##################################################################################################################################################

	## Description: This function checks, for specified date values in specified tables (see construction of object 'm' for details), whether
	## those date values occur after the date of death of that person as specified in the PERSONS table. 
	## If no information is given concerning the date of death, the person is assumed to be still alive. If the day of death or the month of death
	## is missing (but the year of death is not missing), the day/month are conservatively set to the last day of the month/last month of year (see
	## code below for details).


	##################################################################################################################################################
	## 2.2. 'Inner' function
	##################################################################################################################################################

	## Inner function check 2.2
	fun_check_2.2 <- function(d.PERSONS, d.DATES, v.DATES, v.MEANING){
		
		## INPUT CHECKS
		w1 <- w2 <- NA
		
		## Variables required
		var.x <- c('person_id', v.DATES, v.MEANING)
		var.y <- c('person_id', 'day_of_death', 'month_of_death', 'year_of_death')
		
		## Check
		if(any(!(var.y %in% colnames(d.PERSONS))) | any(!(var.x %in% colnames(d.DATES)))){
			w1 <- 'At least one required column is either not in the data or is named incorrectly. Execution halted.'
			return(list(Warnings = w1, Results = data.frame(Meaning = NA, issues = NA, total = NA)))
			}
		
		## n.pre
		n.pre <- nrow(d.DATES)
		
		## Set person_id variables to character in both datasets
		d.PERSONS[['person_id']] <- as.character(d.PERSONS[['person_id']])
		d.DATES[['person_id']] <- as.character(d.DATES[['person_id']])

		## If meaning == NA, replace with (NOT ENTERED)
		d.DATES[,(v.MEANING) := ifelse(is.na(get(v.MEANING)), '(NOT ENTERED)', get(v.MEANING))]
		uniq.meaning <- unique(d.DATES[[v.MEANING]])
		
		## Remove missing values
		d.DATES <- d.DATES[!is.na(get(v.DATES)) & !is.na(person_id),]
		d.PERSONS <- d.PERSONS[!is.na(person_id),]
		
		## Left join d.PERSONS to d.DATES
		d.DATES <- merge(d.DATES[, ..var.x], d.PERSONS[, ..var.y], by = 'person_id', all.x = T)

		## If year_of_death is available and so is day_of_death, but month_of_death is missing: Impute month 12 (december)
		d.DATES$month_of_death[!is.na(d.DATES$year_of_death) & is.na(d.DATES$month_of_death) & !is.na(d.DATES$day_of_death)] <- 12
		
		## If year_of_death is available but day_of_death and month_of_death are missing: Impute 31 dec
		d.DATES$month_of_death[!is.na(d.DATES$year_of_death) & is.na(d.DATES$month_of_death) & is.na(d.DATES$day_of_death)] <- 12
		d.DATES$day_of_death[!is.na(d.DATES$year_of_death) & is.na(d.DATES$month_of_death) & is.na(d.DATES$day_of_death)] <- 31
		
		## If year_of_death is available and so is month_of_death but day_of_death is missing: Impute last day of month
		d.DATES$day_of_death[!is.na(d.DATES$year_of_death) & !is.na(d.DATES$month_of_death) & is.na(d.DATES$day_of_death) & d.DATES$month_of_death %in% c(1, 3, 5, 7, 8, 10, 12)] <- 31
		d.DATES$day_of_death[!is.na(d.DATES$year_of_death) & !is.na(d.DATES$month_of_death) & is.na(d.DATES$day_of_death) & d.DATES$month_of_death %in% c(4, 6, 9, 11)] <- 30
		leapyr.check <- function(yr) (yr %% 4 == 0 & yr %% 100 != 0) | yr %% 400 == 0
		d.DATES$day_of_death[!is.na(d.DATES$year_of_death) & !is.na(d.DATES$month_of_death) & is.na(d.DATES$day_of_death) & d.DATES$month_of_death == 2 & leapyr.check(d.DATES$year_of_death)] <- 29
		d.DATES$day_of_death[!is.na(d.DATES$year_of_death) & !is.na(d.DATES$month_of_death) & is.na(d.DATES$day_of_death) & d.DATES$month_of_death == 2 & !leapyr.check(d.DATES$year_of_death)] <- 28
						
		## Create date_death
		d.DATES[, date_death := paste(year_of_death, month_of_death, day_of_death, sep = '-')] 
		d.DATES[, c('year_of_death', 'month_of_death', 'day_of_death') := NULL]
		
		## Set date values to class date, if not already done
		if(class(d.DATES[[v.DATES]]) != 'Date') d.DATES[,(v.DATES) := as.Date(as.character(d.DATES[[v.DATES]]), format = '%Y%m%d')]
		if(class(d.DATES[['date_death']]) != 'Date') d.DATES[,date_death := as.Date(as.character(d.DATES[['date_death']]), format = '%Y%m%d')]
		
		## Remove rows with missing values (except for date_death - if NA, it is assumed the person is alive)
		d.DATES <- d.DATES[!is.na(person_id) & !is.na(d.DATES[[v.DATES]]) & !is.na(d.DATES[[v.MEANING]]),]
		
		## n.post
		n.post <- nrow(d.DATES)
		
		## Warning
		if(n.pre != n.post) w2 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')
		
		## Count issues per meaning
		Results <- d.DATES[, .(issues = sum(!is.na(date_death) & get(v.DATES) > date_death), total = .N), by = c(v.MEANING)]
		colnames(Results)[colnames(Results) == v.MEANING] <- 'meaning'
		
		## If Results is empty, replace by NAs
		if(nrow(Results) == 0){
			if(length(uniq.meaning) == 0) uniq.meaning <- '(NOT ENTERED)'
			Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = 0, total = 0))
			}
		
		## Return output
		return(list(Warnings = data.table(n.pre = n.pre, n.post = n.post), Results = Results))
		}
	

	##################################################################################################################################################
	## 2.2. 'Outer' function
	##################################################################################################################################################

	## Outer function check 2.2
	run_check_2.2 <- function(dir.data, nb_row = NB){
	
		## If dir.data doesn't end with /, add it
		if(substr(dir.data, nchar(dir.data), nchar(dir.data)) != '/') dir.data <- paste0(dir.data, '/')

		## Get all .csv files in dir.data
		files <- dir(dir.data, pattern = '.csv')

		## Stop if PERSONS.csv is not available
		if(!any(grepl('PERSONS', files))) stop('PERSONS.csv is not in dir.data. Execution halted')
		
		
		## Combine info in one matrix: list all EVENTS, MEDICINES, PROCEDURES, VACCINES and MEDICAL_OBSERVATIONS tables (d), variables of interest (v) and the name of the 'meaning' column
		m <- rbind(
			expand.grid(TABLE = 'EVENTS', d.DATES = files[grepl('^EVENTS', files)], 					v.DATES = c('start_date_record'), 				v.MEANING = 'meaning_of_event', 		stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICINES', d.DATES = files[grepl('^MEDICINES', files)], 				v.DATES = c('date_dispensing', 'date_prescription'), 	v.MEANING = 'meaning_of_drug_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'PROCEDURES', d.DATES = files[grepl('^PROCEDURES', files)], 				v.DATES = c('procedure_date'), 				v.MEANING = 'meaning_of_procedure', 	stringsAsFactors = F),
			expand.grid(TABLE = 'VACCINES', d.DATES = files[grepl('^VACCINES', files)], 					v.DATES = c('vx_record_date', 'vx_admin_date'), 	v.MEANING = 'meaning_of_vx_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICAL_OBSERVATIONS', d.DATES = files[grepl('^MEDICAL_OBSERVATIONS', files)], 	v.DATES = c('mo_date'), 					v.MEANING = 'mo_meaning', 			stringsAsFactors = F),
			expand.grid(TABLE = 'SURVEY_ID', d.DATES = files[grepl('^SURVEY_ID', files)], 				v.DATES = c('survey_date'), 					v.MEANING = 'survey_meaning', 		stringsAsFactors = F),
			expand.grid(TABLE = 'SURVEY_OBSERVATIONS', d.DATES = files[grepl('^SURVEY_OBSERVATIONS', files)], 	v.DATES = c('so_date'), 					v.MEANING = 'so_meaning', 			stringsAsFactors = F)
			)

		## Output objects
		Warnings <- vector('list', length = nrow(m))
		Results <- vector('list', length = nrow(m))

		## Import d.PERSONS (+ basic check if input is correctly formatted). Replace character vallues '' and ' ' by NA
		d.PERSONS <- IMPORT_PATTERN(pat = "PERSONS", dir = dir.data, colls = c('person_id', 'day_of_death', 'month_of_death', 'year_of_death'))
		d.PERSONS.colnames <- colnames(d.PERSONS)	
		lapply(c('day_of_death', 'month_of_death', 'year_of_death'), function(x) d.PERSONS <- d.PERSONS[, eval(x) := as.numeric(get(x))])
		
		if(ncol(d.PERSONS) == 1 | length(colnames(d.PERSONS)) != length(d.PERSONS.colnames)) stop('Problem with importing PERSONS.csv, please check if formatted correctly.') 
		#which.char <- which(sapply(1:ncol(d.PERSONS), FUN = function(i) class(d.PERSONS[[i]]) == 'character'))
		#for(i in which.char) if(any(d.PERSONS[[i]] == ' ' | d.PERSONS[[i]] == '', na.rm = T)) d.PERSONS[[i]][!is.na(d.PERSONS[[i]]) & (d.PERSONS[[i]] == ' ' | d.PERSONS[[i]] == '')] <- NA
		#rm(which.char)

		## If d.PERSONS is empty, give an error
		if(nrow(d.PERSONS) == 0) stop('PERSONS.csv is empty.')
	
		## Start for loop
		for(i in 1:nrow(m)){

			## Read data file i + some basic checks
			d.DATES <- fread(paste0(dir.data, m$d.DATES[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)
			d.DATES.colnames <- colnames(fread(paste0(dir.data, m$d.DATES[i]), sep = ',', nrows = 0, stringsAsFactors = FALSE))
			if(ncol(d.DATES) == 1 | length(colnames(d.DATES)) != length(d.DATES.colnames)) stop(paste0('Problem with importing ', m$d.DATES[i], ', please check if formatted correctly.'))
			if(nrow(d.DATES) == 0) next 	## If the data file is empty - continue to next		
		
			## Split data in 'numchunks' chunks to limit memory issues
			numchunks <- ceiling(nrow(d.DATES) / nb_row)
			all.Results <- list()		## Temporarily store output		
			all.Warnings <- list()		## Temporarily store output

			## Start for loop per chunk
			for(k in 1:numchunks){
				  
				## Select chunk
				if(k < numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):(k * nb_row),]
				if(k == numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):nrow(d.DATES),]
			
				## Formatting empty strings
				which.char <- which(sapply(1:ncol(tmp.d.DATES), FUN = function(j) class(tmp.d.DATES[[j]]) == 'character'))
				for(j in which.char) if(any(tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '', na.rm = T)) tmp.d.DATES[[j]][!is.na(tmp.d.DATES[[j]]) & (tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '')] <- NA
			
				## Run inner function on chunk
				tmp.Out <- fun_check_2.2(d.PERSONS = d.PERSONS, d.DATES = tmp.d.DATES, v.DATES = m$v.DATES[i], v.MEANING = m$v.MEANING[i])
				all.Results[[k]] <- as.data.table(tmp.Out["Results"])
				all.Warnings[[k]] <- as.data.table(tmp.Out["Warnings"])
				
				## Remove objects  
				rm(tmp.d.DATES, tmp.Out) 
				}			

			## Combine the chunk-specific results and warnings
			all.Results <- do.call(rbind, all.Results)
			all.Results <- all.Results[, lapply(.SD, sum), .SDcols = c('Results.issues', 'Results.total'), by = Results.meaning] 
			colnames(all.Results) <- c('meaning', 'issues', 'total') 
			all.Warnings <- do.call(rbind, all.Warnings)
			all.Warnings <- all.Warnings[, lapply(.SD, sum), .SDcols = c('Warnings.n.pre', 'Warnings.n.post')] 
			all.Warnings <- paste('Unable to perform check for', all.Warnings[['Warnings.n.pre']] - all.Warnings[['Warnings.n.post']], 'out of', all.Warnings[['Warnings.n.pre']], 'rows.')
				
			## Store output
			tmp <- list(Results = all.Results, Warnings = all.Warnings)
			Warnings[[i]] <- as.data.frame(cbind(m[i, c('TABLE', 'd.DATES', 'v.DATES')], Warning = as.character(tmp$Warnings), stringsAsFactors = FALSE))
			Results[[i]] <- as.data.frame(cbind(m[i,][rep(seq_len(nrow(m[i,])), each = nrow(tmp$Results)),], tmp$Results, stringsAsFactors = FALSE))
	
			## Remove objects and close loop
			rm(d.DATES.colnames, d.DATES, tmp, which.char, all.Results, all.Warnings)
			}

		## Combine all results
		Warnings <- do.call(rbind, Warnings)
		Warnings <- Warnings[!is.na(Warnings$Warning),]
		Warnings <- Warnings[!grepl('Unable to perform check for 0 out of ', Warnings$Warning, fixed = TRUE),]	## Exclude non-issues
		Results <- do.call(rbind, Results)
		
		## Combine per combination of TABLE, v.DATES and Meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 3) colnames(tmp) <- c('TABLE', 'v.DATES', 'Meaning')
		if(ncol(tmp) != 3) stop('Incorrect number of columns when combining results.')
		Results_Comb <- cbind(tmp, Results_Comb)
		Results_Comb$id.tmp <- NULL
		rm(tmp1, tmp2, tmp)
		
		## Also aggregate over meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb_aggr <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb_aggr$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 2) colnames(tmp) <- c('TABLE', 'v.DATES')
		if(ncol(tmp) != 2) stop('Incorrect number of columns when combining results.')
		Results_Comb_aggr <- cbind(tmp, Results_Comb_aggr)
		Results_Comb_aggr$id.tmp <- NULL

		## If a count is lower than 5 but > 0, replace by '<5'
		Results_Comb_HideSmallCounts <- Results_Comb
		Results_Comb_HideSmallCounts$Issues[Results_Comb_HideSmallCounts$Issues < 5 & Results_Comb_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Total[Results_Comb_HideSmallCounts$Total < 5 & Results_Comb_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Issues <- as.character(Results_Comb_HideSmallCounts$Issues)
		Results_Comb_HideSmallCounts$Total <- as.character(Results_Comb_HideSmallCounts$Total)
		
		## Same for aggregated results
		Results_Comb_aggr_HideSmallCounts <- Results_Comb_aggr
		Results_Comb_aggr_HideSmallCounts$Issues[Results_Comb_aggr_HideSmallCounts$Issues < 5 & Results_Comb_aggr_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Total[Results_Comb_aggr_HideSmallCounts$Total < 5 & Results_Comb_aggr_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Issues <- as.character(Results_Comb_aggr_HideSmallCounts$Issues)
		Results_Comb_aggr_HideSmallCounts$Total <- as.character(Results_Comb_aggr_HideSmallCounts$Total)

		## Always set both to character for consistency
		Results_Comb$Issues <- as.character(Results_Comb$Issues)
		Results_Comb$Total <- as.character(Results_Comb$Total)
		Results_Comb_aggr$Issues <- as.character(Results_Comb_aggr$Issues)
		Results_Comb_aggr$Total <- as.character(Results_Comb_aggr$Total)

		## Return output and end function
		return(list(Warnings = Warnings, Results = Results_Comb, Results_HideSmallCounts = Results_Comb_HideSmallCounts, Results_aggr = Results_Comb_aggr, Results_aggr_HideSmallCounts = Results_Comb_aggr_HideSmallCounts))
		}



##################################################################################################################################################
## CHECK 2.3
##################################################################################################################################################

	## Description: This function checks, for specified date values in specified tables (see construction of object 'm' for details), whether
	## those date values occur within one of the observation periods as specified in the OBSERVATION_PERIODS table. 
	## If the OP start date is available but the end date is missing, it is assumed that the OP is still on-going (the end date is then replaced
	## by the creation_date)

	##################################################################################################################################################
	## 2.3. 'Inner' function
	##################################################################################################################################################

	## Inner function check 2.3
	fun_check_2.3 <- function(d.OBSERVATION_PERIODS, d.DATES, v.DATES, v.MEANING){
		
		## INPUT CHECKS
		w1 <- w2 <- NA
		
		## Variables required
		var.x <- c('person_id', v.DATES, v.MEANING)
		var.y <- c('person_id', 'op_start_date', 'op_end_date')
		
		## Check
		if(any(!(var.y %in% colnames(d.OBSERVATION_PERIODS))) | any(!(var.x %in% colnames(d.DATES)))){
			w1 <- 'At least one required column is either not in the data or is named incorrectly. Execution halted.'
			return(list(Warnings = w1, Results = data.frame(Meaning = NA, issues = NA, total = NA)))
			}
		
		## n.pre
		n.pre <- nrow(d.DATES)
	
		## Set person_id variables to character in both datasets
		d.OBSERVATION_PERIODS[['person_id']] <- as.character(d.OBSERVATION_PERIODS[['person_id']])
		d.DATES[['person_id']] <- as.character(d.DATES[['person_id']])

		## If meaning == NA, replace with (NOT ENTERED)
		d.DATES[,(v.MEANING) := ifelse(is.na(get(v.MEANING)), '(NOT ENTERED)', get(v.MEANING))]
		uniq.meaning <- unique(d.DATES[[v.MEANING]])
		
		## Remove missing values
		d.DATES <- d.DATES[!is.na(get(v.DATES)) & !is.na(person_id),]
		d.OBSERVATION_PERIODS <- d.OBSERVATION_PERIODS[!is.na(person_id),]
		
		## If no data at all:
		if(nrow(d.DATES) == 0 | nrow(d.OBSERVATION_PERIODS) == 0){
			n.post <- 0
			w2 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')
			if(length(uniq.meaning) == 0) uniq.meaning <- '(NOT ENTERED)'
			Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = 0, total = 0))
			}

		## otherwise:
		if(nrow(d.DATES) > 0 & nrow(d.OBSERVATION_PERIODS) > 0){
				
			## add id2
			d.DATES[,id2 := paste(get(v.MEANING), person_id, 1:.N, sep = '_-_'), by = person_id]
			var.x <- c(var.x, 'id2')
			d.COMB <- merge(d.DATES[,..var.x], d.OBSERVATION_PERIODS[,..var.y], by = 'person_id', all.x = T, all.y = F, allow.cartesian = TRUE)

			## If	op_start_date is available but op_end_date is NA, the OP is still ongoing -> set to today's date
			if(class(d.COMB[['op_end_date']]) == 'character') 	d.COMB$op_end_date[!is.na(d.COMB$op_start_date) & is.na(d.COMB$op_end_date)] <- gsub('-', '', as.character(creation_date))
			if(class(d.COMB[['op_end_date']]) == 'integer') 	d.COMB$op_end_date[!is.na(d.COMB$op_start_date) & is.na(d.COMB$op_end_date)] <- as.integer(gsub('-', '', as.character(creation_date)))
			if(class(d.COMB[['op_end_date']]) == 'Date') 		d.COMB$op_end_date[!is.na(d.COMB$op_start_date) & is.na(d.COMB$op_end_date)] <- creation_date

			## Set date values to class date, if not already done
			if(class(d.COMB[[v.DATES]]) != 'Date') 		d.COMB[,(v.DATES) := as.Date(as.character(d.COMB[[v.DATES]]), format = '%Y%m%d')]
			if(class(d.COMB[['op_start_date']]) != 'Date') 	d.COMB[,op_start_date := as.Date(as.character(d.COMB[['op_start_date']]), format = '%Y%m%d')]
			if(class(d.COMB[['op_end_date']]) != 'Date') 	d.COMB[,op_end_date := as.Date(as.character(d.COMB[['op_end_date']]), format = '%Y%m%d')]
	
			## Check if date falls in OP
			d.COMB[,ind := get(v.DATES) >= op_start_date & get(v.DATES) <= op_end_date]
				
			## Now check if at least one of the ind values per id2 = TRUE
			d.COMB <- d.COMB[, .(sumind = sum(ind, na.rm = TRUE)), by = id2]
			d.COMB$meaning <- do.call(rbind, strsplit(d.COMB$id2, '_-_'))[,1]
				
			## n.post
			n.post <- nrow(d.COMB)
				
			## Warning
			if(n.pre != n.post) w2 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')
				
			## Count issues per meaning
			Results <- d.COMB[, .(issues = sum(sumind == 0), total = .N), by = meaning]
				
			## If Results is empty, replace by NAs
			if(nrow(Results) == 0){
				if(length(uniq.meaning) == 0) uniq.meaning <- '(NOT ENTERED)'
				Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = 0, total = 0))
				}

			## Close if
			}

		## Return output
		return(list(Warnings = data.table(n.pre = n.pre, n.post = n.post), Results = Results))
		}

	##################################################################################################################################################
	## 2.3. 'Outer' function
	##################################################################################################################################################

	## Outer function check 2.3	
	run_check_2.3 <- function(dir.data, nb_row = NB){
		
		## If dir.data doesn't end with /, add it
		if(substr(dir.data, nchar(dir.data), nchar(dir.data)) != '/') dir.data <- paste0(dir.data, '/')
		
		## Get all files, use only files ending on .csv
		files <- dir(dir.data, pattern = '.csv')
		
		## Stop if OBSERVATION_PERIODS.csv is not available
		if(!any(grepl('OBSERVATION_PERIODS', files))) stop('PERSONS.csv is not in dir.data. Execution halted')
		
		## Combine info in one matrix
		m <- rbind(
			expand.grid(TABLE = 'EVENTS', d.DATES = files[grepl('^EVENTS', files)], 					v.DATES = c('start_date_record'), 				v.MEANING = 'meaning_of_event', 		stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICINES', d.DATES = files[grepl('^MEDICINES', files)], 				v.DATES = c('date_dispensing', 'date_prescription'), 	v.MEANING = 'meaning_of_drug_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'PROCEDURES', d.DATES = files[grepl('^PROCEDURES', files)], 				v.DATES = c('procedure_date'), 				v.MEANING = 'meaning_of_procedure', 	stringsAsFactors = F),
			expand.grid(TABLE = 'VACCINES', d.DATES = files[grepl('^VACCINES', files)], 					v.DATES = c('vx_record_date', 'vx_admin_date'), 	v.MEANING = 'meaning_of_vx_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICAL_OBSERVATIONS', d.DATES = files[grepl('^MEDICAL_OBSERVATIONS', files)], 	v.DATES = c('mo_date'), 					v.MEANING = 'mo_meaning', 			stringsAsFactors = F),
			expand.grid(TABLE = 'SURVEY_ID', d.DATES = files[grepl('^SURVEY_ID', files)], 				v.DATES = c('survey_date'), 					v.MEANING = 'survey_meaning', 		stringsAsFactors = F),
			expand.grid(TABLE = 'SURVEY_OBSERVATIONS', d.DATES = files[grepl('^SURVEY_OBSERVATIONS', files)], 	v.DATES = c('so_date'), 					v.MEANING = 'so_meaning', 			stringsAsFactors = F)
			)
		
		## Output objects
		Warnings <- vector('list', length = nrow(m))
		Results <- vector('list', length = nrow(m))
		
		## Import d.OBSERVATION_PERIODS (+ basic check if input is correctly formatted). Replace character vallues '' and ' ' by NA
		
		d.OBSERVATION_PERIODS <- IMPORT_PATTERN(pat = "OBSERVATION_PERIODS", dir = dir.data, colls = c('person_id', 'op_start_date', 'op_end_date'))
		d.OBSERVATION_PERIODS.colnames <- colnames(d.OBSERVATION_PERIODS)	
		#lapply(c('day_of_birth', 'month_of_birth', 'year_of_birth'), function(x) d.PERSONS <- d.PERSONS[, eval(x) := as.numeric(get(x))])
		
		if(ncol(d.OBSERVATION_PERIODS) == 1 | length(colnames(d.OBSERVATION_PERIODS)) != length(d.OBSERVATION_PERIODS.colnames)) stop('Problem with importing OBSERVATION_PERIODS.csv, please check if formatted correctly.') 
		#which.char <- which(sapply(1:ncol(d.OBSERVATION_PERIODS), FUN = function(i) class(d.OBSERVATION_PERIODS[[i]]) == 'character'))
		#for(i in which.char) if(any(d.OBSERVATION_PERIODS[[i]] == ' ' | d.OBSERVATION_PERIODS[[i]] == '', na.rm = T)) d.OBSERVATION_PERIODS[[i]][!is.na(d.OBSERVATION_PERIODS[[i]]) & (d.OBSERVATION_PERIODS[[i]] == ' ' | d.OBSERVATION_PERIODS[[i]] == '')] <- NA
		#rm(which.char)
		
		## If d.OBSERVATION_PERIODS is empty, give an error
		if(nrow(d.OBSERVATION_PERIODS) == 0) stop('OBSERVATION_PERIODS.csv is empty.')
		
		## Start for loop
		for(i in 1:nrow(m)){

			## Read data file i + some basic checks		
			d.DATES <- fread(paste0(dir.data, m$d.DATES[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)
			d.DATES.colnames <- colnames(fread(paste0(dir.data, m$d.DATES[i]), sep = ',', nrows = 0, stringsAsFactors = FALSE))
			if(ncol(d.DATES) == 1 | length(colnames(d.DATES)) != length(d.DATES.colnames)) stop(paste0('Problem with importing ', m$d.DATES[i], ', please check if formatted correctly.'))
			if(nrow(d.DATES) == 0) next 	## If the data file is empty - continue to next		

			## Split data in 'numchunks' chunks to limit memory issues
			numchunks <- ceiling(nrow(d.DATES) / nb_row)
			all.Results <- list()		## Temporarily store output		
			all.Warnings <- list()		## Temporarily store output

			## Start for loop per chunk
			for(k in 1:numchunks){
				  
				## Select chunk
				if(k < numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):(k * nb_row),]
				if(k == numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):nrow(d.DATES),]
			
				## Formatting empty strings
				which.char <- which(sapply(1:ncol(tmp.d.DATES), FUN = function(j) class(tmp.d.DATES[[j]]) == 'character'))
				for(j in which.char) if(any(tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '', na.rm = T)) tmp.d.DATES[[j]][!is.na(tmp.d.DATES[[j]]) & (tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '')] <- NA
			
				## Run inner function on chunk
				tmp.Out <- fun_check_2.3(d.OBSERVATION_PERIODS = d.OBSERVATION_PERIODS, d.DATES = tmp.d.DATES, v.DATES = m$v.DATES[i], v.MEANING = m$v.MEANING[i])
				all.Results[[k]] <- as.data.table(tmp.Out["Results"])
				all.Warnings[[k]] <- as.data.table(tmp.Out["Warnings"])
				
				## Remove objects and close loop
				rm(tmp.d.DATES, tmp.Out) 
				}	  
			
			## Combine the chunk-specific results and warnings
			all.Results <- do.call(rbind, all.Results)
			all.Results <- all.Results[, lapply(.SD, sum), .SDcols = c('Results.issues', 'Results.total'), by = Results.meaning] 
			colnames(all.Results) <- c('meaning', 'issues', 'total') 
			all.Warnings <- do.call(rbind, all.Warnings)
			all.Warnings <- all.Warnings[, lapply(.SD, sum), .SDcols = c('Warnings.n.pre', 'Warnings.n.post')] 
			all.Warnings <- paste('Unable to perform check for', all.Warnings[['Warnings.n.pre']] - all.Warnings[['Warnings.n.post']], 'out of', all.Warnings[['Warnings.n.pre']], 'rows.')
				
			## Store output
			tmp <- list(Results = all.Results, Warnings = all.Warnings)
			Warnings[[i]] <- as.data.frame(cbind(m[i,c('TABLE', 'd.DATES', 'v.DATES')], Warning = as.character(tmp$Warnings), stringsAsFactors = FALSE))
			Results[[i]] <- as.data.frame(cbind(m[i,][rep(seq_len(nrow(m[i,])), each = nrow(tmp$Results)),], tmp$Results, stringsAsFactors = FALSE))
	
			## Remove objects and close loop
			rm(d.DATES.colnames, d.DATES, tmp, which.char, all.Results, all.Warnings)
			}

		## Combine all results
		Warnings <- do.call(rbind, Warnings)
		Warnings <- Warnings[!is.na(Warnings$Warning),]
		Warnings <- Warnings[!grepl('Unable to perform check for 0 out of ', Warnings$Warning, fixed = TRUE),]	## Exclude non-issues
		Results <- do.call(rbind, Results)
		
		## Combine per combination of TABLE, v.DATES and Meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 3) colnames(tmp) <- c('TABLE', 'v.DATES', 'Meaning')
		if(ncol(tmp) != 3) stop('Incorrect number of columns when combining results.')
		Results_Comb <- cbind(tmp, Results_Comb)
		Results_Comb$id.tmp <- NULL
		rm(tmp1, tmp2, tmp)
		
		## Also aggregate over meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb_aggr <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb_aggr$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 2) colnames(tmp) <- c('TABLE', 'v.DATES')
		if(ncol(tmp) != 2) stop('Incorrect number of columns when combining results.')
		Results_Comb_aggr <- cbind(tmp, Results_Comb_aggr)
		Results_Comb_aggr$id.tmp <- NULL

		## If a count is lower than 5 but > 0, replace by '<5'
		Results_Comb_HideSmallCounts <- Results_Comb
		Results_Comb_HideSmallCounts$Issues[Results_Comb_HideSmallCounts$Issues < 5 & Results_Comb_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Total[Results_Comb_HideSmallCounts$Total < 5 & Results_Comb_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Issues <- as.character(Results_Comb_HideSmallCounts$Issues)
		Results_Comb_HideSmallCounts$Total <- as.character(Results_Comb_HideSmallCounts$Total)
		
		## Same for aggregated results
		Results_Comb_aggr_HideSmallCounts <- Results_Comb_aggr
		Results_Comb_aggr_HideSmallCounts$Issues[Results_Comb_aggr_HideSmallCounts$Issues < 5 & Results_Comb_aggr_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Total[Results_Comb_aggr_HideSmallCounts$Total < 5 & Results_Comb_aggr_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Issues <- as.character(Results_Comb_aggr_HideSmallCounts$Issues)
		Results_Comb_aggr_HideSmallCounts$Total <- as.character(Results_Comb_aggr_HideSmallCounts$Total)

		## Always set both to character for consistency
		Results_Comb$Issues <- as.character(Results_Comb$Issues)
		Results_Comb$Total <- as.character(Results_Comb$Total)
		Results_Comb_aggr$Issues <- as.character(Results_Comb_aggr$Issues)
		Results_Comb_aggr$Total <- as.character(Results_Comb_aggr$Total)

		## Return output and end function
		return(list(Warnings = Warnings, Results = Results_Comb, Results_HideSmallCounts = Results_Comb_HideSmallCounts, Results_aggr = Results_Comb_aggr, Results_aggr_HideSmallCounts = Results_Comb_aggr_HideSmallCounts))
		}

##################################################################################################################################################
## CHECK 2.4
##################################################################################################################################################

	## Description: This function checks how many rows in a given table contain a person ID that cannot be found in the PERSONS table

	##################################################################################################################################################
	## 2.4. 'Inner' function
	##################################################################################################################################################

	## Inner function check 2.4
	fun_check_2.4 <- function(d.PERSONS, d, v.MEANING){

		## INPUT CHECKS
		w1 <- w2 <- NA

		## Variables required
		var.x <- c('person_id', v.MEANING)
		var.y <- c('person_id')

		## Check
		if(any(!(var.y %in% colnames(d.PERSONS))) | any(!(var.x %in% colnames(d)))){
			w1 <- 'At least one required column is either not in the data or is named incorrectly. Execution halted.'
			return(list(Warnings = w1, Results = data.frame(Meaning = NA, issues = NA, total = NA)))
			}

		## n.pre
		n.pre <- nrow(d)

		## Set person_id variables to character in both datasets
		d.PERSONS[['person_id']] <- as.character(d.PERSONS[['person_id']])
		d[['person_id']] <- as.character(d[['person_id']])

		## If meaning == NA, replace with (NOT ENTERED)
		d[,(v.MEANING) := ifelse(is.na(get(v.MEANING)), '(NOT ENTERED)', get(v.MEANING))]
		uniq.meaning <- unique(d[[v.MEANING]])

		## Remove missing values
		d <- d[!is.na(person_id),]
		d.PERSONS <- d.PERSONS[!is.na(person_id),]
	
		## n.post
		n.post <- nrow(d)

		## Check, per meaning, if person_id is in d.PERSONS
		den <- rep(NA, length(uniq.meaning))
		num <- den
		#for(x in uniq.meaning){
		#	uniq.PERSONS <- unique(d[d[[v.MEANING]] == x, person_id])
		#	den[which(uniq.meaning == x)] <- length(uniq.PERSONS)
		#	num[which(uniq.meaning == x)] <- sum(!(uniq.PERSONS %in% unique(d.PERSONS[, person_id])))
		#	}

		## The above counts the unique persons. But this will not yield correct results when splitting the data in chunks (because a subject may occur in more than one chunk)
		## Therefore, below we simply count the number of rows with a person id value not in the PERSONS table.
		for(x in uniq.meaning){
			tmp <- d[d[[v.MEANING]] == x, person_id]
			den[which(uniq.meaning == x)] <- length(tmp)
			num[which(uniq.meaning == x)] <- sum(!(tmp %in% unique(d.PERSONS[, person_id])))
			}

		## Warning
		if(n.pre != n.post){
			w2 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')
			}

		## Results
		Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = num, total = den))

		## Set NAs to 0
		Results$issues[is.na(Results$issues)] <- 0
		Results$total[is.na(Results$total)] <- 0

		## Return output
		return(list(Warnings = data.table(n.pre = n.pre, n.post = n.post), Results = Results))
		}


	##################################################################################################################################################
	## 2.4. 'Outer' function
	##################################################################################################################################################

	## Outer function check 2.4
	run_check_2.4 <- function(dir.data, nb_row = NB){
		
		## If dir.data doesn't end with /, add it
		if(substr(dir.data, nchar(dir.data), nchar(dir.data)) != '/') dir.data <- paste0(dir.data, '/')
		
		## Get all files, use only files ending on .csv
		files <- dir(dir.data, pattern = '.csv')
		
		## Stop if PERSONS.csv is not available
		if(!any(grepl('PERSONS', files))) stop('PERSONS.csv is not in dir.data. Execution halted')
		
		## Combine info in one matrix
		m <- rbind(
			expand.grid(TABLE = 'EVENTS', 		d = files[grepl('^EVENTS', files)], 		v.MEANING = 'meaning_of_event', 		stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICINES', 		d = files[grepl('^MEDICINES', files)],  		v.MEANING = 'meaning_of_drug_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'PROCEDURES', 		d = files[grepl('^PROCEDURES', files)],  		v.MEANING = 'meaning_of_procedure', 	stringsAsFactors = F),
			expand.grid(TABLE = 'VACCINES', 		d = files[grepl('^VACCINES', files)],  			v.MEANING = 'meaning_of_vx_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICAL_OBSERVATIONS', 	d = files[grepl('^MEDICAL_OBSERVATIONS', files)],  	v.MEANING = 'mo_meaning', 	stringsAsFactors = F),
			expand.grid(TABLE = 'SURVEY_ID', 		d = files[grepl('^SURVEY_ID', files)], 			v.MEANING = 'survey_meaning', 			stringsAsFactors = F),
			expand.grid(TABLE = 'SURVEY_OBSERVATIONS', 		d = files[grepl('^SURVEY_OBSERVATIONS', files)], 			v.MEANING = 'so_meaning', 			stringsAsFactors = F)
			)
		
		## Output objects
		Warnings <- vector('list', length = nrow(m))
		Results <- vector('list', length = nrow(m))
		
		## Import d.PERSONS (+ basic check if input is correctly formatted). Replace character values '' and ' ' by NA.
		d.PERSONS <- IMPORT_PATTERN(pat = "PERSONS", dir = dir.data, colls = c('person_id'))
		d.PERSONS.colnames <- colnames(d.PERSONS)	
		
		#if(ncol(d.PERSONS) == 1 | length(colnames(d.PERSONS)) != length(d.PERSONS.colnames)) stop('Problem with importing PERSONS.csv, please check if formatted correctly.') 
		#which.char <- which(sapply(1:ncol(d.PERSONS), FUN = function(i) class(d.PERSONS[[i]]) == 'character'))
		#for(i in which.char) if(any(d.PERSONS[[i]] == ' ' | d.PERSONS[[i]] == '', na.rm = T)) d.PERSONS[[i]][!is.na(d.PERSONS[[i]]) & (d.PERSONS[[i]] == ' ' | d.PERSONS[[i]] == '')] <- NA
		#rm(which.char)
		
		## If d.PERSONS is empty, give an error
		if(nrow(d.PERSONS) == 0) stop('PERSONS.csv is empty.')
		
		## Start loop
		for(i in 1:nrow(m)){

			## Read data
			d <- fread(paste0(dir.data, m$d[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)
			d.colnames <- colnames(fread(paste0(dir.data, m$d[i]), sep = ',', nrows = 0, stringsAsFactors = FALSE))
			if(ncol(d) == 1 | length(colnames(d)) != length(d.colnames)) stop(paste0('Problem with importing ', m$d[i], ', please check if formatted correctly.'))
			if(nrow(d) == 0) next 	## If the data file is empty - continue to next		
			
			## Split data in 'numchunks' chunks to limit memory issues
			numchunks <- ceiling(nrow(d) / nb_row)
			all.Results <- list()		## Temporarily store output		
			all.Warnings <- list()		## Temporarily store output

			## Start for loop per chunk
			for(k in 1:numchunks){
				  
				## Note: See comment in inner function. Function now counts number of rows with person IDs not found, not unique number of person IDs that cannot be found

				## Select chunk
				if(k < numchunks) 	tmp.d <- d[((k - 1) * nb_row + 1):(k * nb_row),]
				if(k == numchunks) 	tmp.d <- d[((k - 1) * nb_row + 1):nrow(d),]

				## Formatting empty strings
				which.char <- which(sapply(1:ncol(tmp.d), FUN = function(j) class(tmp.d[[j]]) == 'character'))
				for(j in which.char) if(any(tmp.d[[j]] == ' ' | tmp.d[[j]] == '', na.rm = T)) tmp.d[[j]][!is.na(tmp.d[[j]]) & (tmp.d[[j]] == ' ' | tmp.d[[j]] == '')] <- NA

				## Run inner function on chunk
				tmp.Out <- fun_check_2.4(d.PERSONS = d.PERSONS, d = tmp.d, v.MEANING = m$v.MEANING[i])
				all.Results[[k]] <- as.data.table(tmp.Out["Results"])
				all.Warnings[[k]] <- as.data.table(tmp.Out["Warnings"])
				
				## Remove objects and close loop
				rm(tmp.d, tmp.Out) 
				}	  
			
			## Combine the chunk-specific results and warnings
			all.Results <- do.call(rbind, all.Results)
			all.Results <- all.Results[, lapply(.SD, sum), .SDcols = c('Results.issues', 'Results.total'), by = Results.meaning] 
			colnames(all.Results) <- c('meaning', 'issues', 'total') 
			all.Warnings <- do.call(rbind, all.Warnings)
			all.Warnings <- all.Warnings[, lapply(.SD, sum), .SDcols = c('Warnings.n.pre', 'Warnings.n.post')] 
			all.Warnings <- paste('Unable to perform check for', all.Warnings[['Warnings.n.pre']] - all.Warnings[['Warnings.n.post']], 'out of', all.Warnings[['Warnings.n.pre']], 'rows.')
				
			## Store output
			tmp <- list(Results = all.Results, Warnings = all.Warnings)
			Warnings[[i]] <- as.data.frame(cbind(m[i,c('TABLE', 'd')], Warning = as.character(tmp$Warnings), stringsAsFactors = FALSE))
			Results[[i]] <- as.data.frame(cbind(m[i,][rep(seq_len(nrow(m[i,])), each = nrow(tmp$Results)),], tmp$Results, stringsAsFactors = FALSE))
	
			## Remove objects and close loop
			rm(d.colnames, d, tmp, which.char, all.Results, all.Warnings)
			}

		## Combine all results
		Warnings <- do.call(rbind, Warnings)
		Warnings <- Warnings[!is.na(Warnings$Warning),]
		Warnings <- Warnings[!grepl('Unable to perform check for 0 out of ', Warnings$Warning, fixed = TRUE),]	## Exclude non-issues
		Results <- do.call(rbind, Results)

		## Combine per combination of TABLE, v.DATES and Meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$meaning, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$meaning, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 2) colnames(tmp) <- c('TABLE', 'Meaning')
		if(ncol(tmp) != 2) stop('Incorrect number of columns when combining results.')
		Results_Comb <- cbind(tmp, Results_Comb)
		Results_Comb$id.tmp <- NULL
		rm(tmp1, tmp2, tmp)
		
		## Also aggregate over meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb_aggr <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb_aggr$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 1) colnames(tmp) <- c('TABLE')
		if(ncol(tmp) != 1) stop('Incorrect number of columns when combining results.')
		Results_Comb_aggr <- cbind(tmp, Results_Comb_aggr)
		Results_Comb_aggr$id.tmp <- NULL

		## If a count is lower than 5 but > 0, replace by '<5'
		Results_Comb_HideSmallCounts <- Results_Comb
		Results_Comb_HideSmallCounts$Issues[Results_Comb_HideSmallCounts$Issues < 5 & Results_Comb_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Total[Results_Comb_HideSmallCounts$Total < 5 & Results_Comb_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Issues <- as.character(Results_Comb_HideSmallCounts$Issues)
		Results_Comb_HideSmallCounts$Total <- as.character(Results_Comb_HideSmallCounts$Total)
		
		## Same for aggregated results
		Results_Comb_aggr_HideSmallCounts <- Results_Comb_aggr
		Results_Comb_aggr_HideSmallCounts$Issues[Results_Comb_aggr_HideSmallCounts$Issues < 5 & Results_Comb_aggr_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Total[Results_Comb_aggr_HideSmallCounts$Total < 5 & Results_Comb_aggr_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Issues <- as.character(Results_Comb_aggr_HideSmallCounts$Issues)
		Results_Comb_aggr_HideSmallCounts$Total <- as.character(Results_Comb_aggr_HideSmallCounts$Total)

		## Always set both to character for consistency
		Results_Comb$Issues <- as.character(Results_Comb$Issues)
		Results_Comb$Total <- as.character(Results_Comb$Total)
		Results_Comb_aggr$Issues <- as.character(Results_Comb_aggr$Issues)
		Results_Comb_aggr$Total <- as.character(Results_Comb_aggr$Total)

		## Return output and end function
		return(list(Warnings = Warnings, Results = Results_Comb, Results_HideSmallCounts = Results_Comb_HideSmallCounts, Results_aggr = Results_Comb_aggr, Results_aggr_HideSmallCounts = Results_Comb_aggr_HideSmallCounts))
		}

##################################################################################################################################################
## CHECK 2.5
##################################################################################################################################################

	## Description: Counts how often a date takes place before the visit start date as specified in the visit occurence table.

	##################################################################################################################################################
	## 2.5. 'Inner' function
	##################################################################################################################################################

	## Inner function check 2.5
	fun_check_2.5 <- function(d.VIS_OCC, d.DATES, v.DATES, v.MEANING){

		## INPUT CHECKS
		w1 <- w2 <- NA

		## Variables required
		var.x <- c('visit_occurrence_id', v.DATES, v.MEANING)
		var.y <- c('visit_occurrence_id', 'visit_start_date')

		## Check
		if(any(!(var.y %in% colnames(d.VIS_OCC))) | any(!(var.x %in% colnames(d.DATES)))){
			w1 <- 'At least one required column is either not in the data or is named incorrectly. Execution halted.'
			return(list(Warnings = w1, Results = data.frame(Meaning = NA, issues = NA, total = NA)))
			}

		## n.pre
		n.pre <- nrow(d.DATES)

		## Set visit_occurrence_id variables to character in both datasets
		d.VIS_OCC[['visit_occurrence_id']] <- as.character(d.VIS_OCC[['visit_occurrence_id']])
		d.DATES[['visit_occurrence_id']] <- as.character(d.DATES[['visit_occurrence_id']])
		d.DATES[['person_id']] <- as.character(d.DATES[['person_id']])

		## If meaning == NA, replace with (NOT ENTERED)
		d.DATES[,(v.MEANING) := ifelse(is.na(get(v.MEANING)), '(NOT ENTERED)', get(v.MEANING))]
		uniq.meaning <- unique(d.DATES[[v.MEANING]])

		## Remove missing values
		d.DATES <- d.DATES[!is.na(get(v.DATES)) & !is.na(visit_occurrence_id),]
		d.VIS_OCC <- d.VIS_OCC[!is.na(visit_occurrence_id),]

		## Left join d.DATES and d.VIS_OCC
		d.DATES <- merge(d.DATES[,c(v.DATES, 'visit_occurrence_id', v.MEANING), with = F], d.VIS_OCC, by = 'visit_occurrence_id', all.x = TRUE, all.y = FALSE, allow.cartesian = F)

		## Set date values to class date, if not already done
		if(class(d.DATES[[v.DATES]]) != 'Date') d.DATES[,(v.DATES) := as.Date(as.character(d.DATES[[v.DATES]]), format = '%Y%m%d')]
		if(class(d.DATES[['visit_start_date']]) != 'Date') d.DATES[,visit_start_date := as.Date(as.character(d.DATES[['visit_start_date']]), format = '%Y%m%d')]
	
		## If v.DATES or visit_start_date is missing, remove.
		d.DATES <- d.DATES[complete.cases(d.DATES),]

		## n.post
		n.post <- nrow(d.DATES)

		## Warning
		if(n.pre != n.post) w2 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')

		## Count issues per meaning
		Results <- d.DATES[, .(issues = sum(get(v.DATES) < visit_start_date), total = .N), by = c(v.MEANING)]
		colnames(Results)[colnames(Results) == v.MEANING] <- 'meaning'

		## If Results is empty, replace by NAs
		if(nrow(Results) == 0){
			if(length(uniq.meaning) == 0) uniq.meaning <- '(NOT ENTERED)'
			Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = 0, total = 0))
			}

		## Return output
		return(list(Warnings = data.table(n.pre = n.pre, n.post = n.post), Results = Results))
		}


	##################################################################################################################################################
	## 2.5. 'Outer' function
	##################################################################################################################################################

	## Outer function check 2.5
	run_check_2.5 <- function(dir.data, nb_row = NB){

		## If dir.data doesn't end with /, add it
		if(substr(dir.data, nchar(dir.data), nchar(dir.data)) != '/') dir.data <- paste0(dir.data, '/')

		## Get all files, use only files ending on .csv
		files <- dir(dir.data, pattern = '.csv')

		## Combine info in one matrix
		m <- rbind(
			expand.grid(TABLE = 'EVENTS', d.DATES = files[grepl('^EVENTS', files)], 					v.DATES = c('start_date_record'), 				v.MEANING = 'meaning_of_event', 		stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICINES', d.DATES = files[grepl('^MEDICINES', files)], 				v.DATES = c('date_dispensing', 'date_prescription'), 	v.MEANING = 'meaning_of_drug_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'PROCEDURES', d.DATES = files[grepl('^PROCEDURES', files)], 				v.DATES = c('procedure_date'), 				v.MEANING = 'meaning_of_procedure', 	stringsAsFactors = F),
			expand.grid(TABLE = 'VACCINES', d.DATES = files[grepl('^VACCINES', files)], 					v.DATES = c('vx_record_date', 'vx_admin_date'), 	v.MEANING = 'meaning_of_vx_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICAL_OBSERVATIONS', d.DATES = files[grepl('^MEDICAL_OBSERVATIONS', files)], 	v.DATES = c('mo_date'), 					v.MEANING = 'mo_meaning', 			stringsAsFactors = F)
			)

		## Output objects
		Warnings <- vector('list', length = nrow(m))
		Results <- vector('list', length = nrow(m))

		## Collect all visit_occurrence_ids and start dates
		m0 <- expand.grid(TABLE = 'VISIT_OCCURRENCE', d = files[grepl('^VISIT_OCCURRENCE', files)], stringsAsFactors = F)
		d.VIS_OCC <- vector('list', length = nrow(m0))
		for(i in 1:nrow(m0)){
		 	d <- fread(paste0(dir.data, m0$d[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)[, c('visit_occurrence_id', 'visit_start_date'), with = F]
			d.colnames <- colnames(d)
			if(ncol(d) == 1 | length(colnames(d)) != length(d.colnames)) stop(paste0('Problem with importing ', m$d[i], ', please check if formatted correctly.'))
			if(all(c('visit_occurrence_id', 'visit_start_date') %in% d.colnames)) d.VIS_OCC[[i]] <- d[,c('visit_occurrence_id', 'visit_start_date')]
			if(!all(c('visit_occurrence_id', 'visit_start_date') %in% d.colnames)) d.VIS_OCC[[i]] <- NULL ## Only use vis_occ data when both variables are available
			d$visit_occurrence_id <- as.character(d$visit_occurrence_id) ## set visit_occurrence_id to character
			rm(d, d.colnames)
			}

		## rbind 
		d.VIS_OCC <- do.call(rbind, d.VIS_OCC)

		## Replace character values '' and ' ' by NA.
		which.char <- which(sapply(1:ncol(d.VIS_OCC), FUN = function(i) class(d.VIS_OCC[[i]]) == 'character'))
		for(i in which.char) if(any(d.VIS_OCC[[i]] == ' ' | d.VIS_OCC[[i]] == '', na.rm = T)) d.VIS_OCC[[i]][!is.na(d.VIS_OCC[[i]]) & (d.VIS_OCC[[i]] == ' ' | d.VIS_OCC[[i]] == '')] <- NA
		rm(which.char)

		## Remove duplicated rows, check if there are still duplicated visit_occurrence_id values after that. If so -> error
		d.VIS_OCC <- d.VIS_OCC[!duplicated(paste(d.VIS_OCC$visit_occurrence_id, d.VIS_OCC$visit_start_date)),]
		if(any(duplicated(d.VIS_OCC$visit_occurrence_id))) stop('VISIT_OCCURRENCE contains duplicated visit_occurrence_id values, with different start dates. Execution halted.')

		## If d.VIS_OCC is empty, give an error
		if(nrow(d.VIS_OCC) == 0) stop('No VISIT_OCCURRENCE data.')

		## Run loop
		for(i in 1:nrow(m)){

			## Read data
			d.DATES <- fread(paste0(dir.data, m$d.DATES[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)
			d.DATES.colnames <- colnames(fread(paste0(dir.data, m$d.DATES[i]), sep = ',', nrows = 0, stringsAsFactors = FALSE))
			if(ncol(d.DATES) == 1 | length(colnames(d.DATES)) != length(d.DATES.colnames)) stop(paste0('Problem with importing ', m$d.DATES[i], ', please check if formatted correctly.'))
			if(nrow(d.DATES) == 0) next 	## If the data file is empty - continue to next		

			## Split data in 'numchunks' chunks to limit memory issues - note, since the function counts unique ID values, we should first remove rows in duplicate
			numchunks <- ceiling(nrow(d.DATES) / nb_row)
			all.Results <- list()		## Temporarily store output		
			all.Warnings <- list()		## Temporarily store output

			## Start for loop per chunk
			for(k in 1:numchunks){
				  
				## Select chunk
				if(k < numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):(k * nb_row),]
				if(k == numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):nrow(d.DATES),]
			
				## Formatting empty strings
				which.char <- which(sapply(1:ncol(tmp.d.DATES), FUN = function(j) class(tmp.d.DATES[[j]]) == 'character'))
				for(j in which.char) if(any(tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '', na.rm = T)) tmp.d.DATES[[j]][!is.na(tmp.d.DATES[[j]]) & (tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '')] <- NA
			
				## Run inner function on chunk
				tmp.Out <- fun_check_2.5(d.VIS_OCC = d.VIS_OCC, d.DATES = tmp.d.DATES, v.DATES = m$v.DATES[i], v.MEANING = m$v.MEANING[i])
				all.Results[[k]] <- as.data.table(tmp.Out["Results"])
				all.Warnings[[k]] <- as.data.table(tmp.Out["Warnings"])
				
				## Remove objects and close loop
				rm(tmp.d.DATES, tmp.Out) 
				}	 	

			## Combine the chunk-specific results and warnings
			all.Results <- do.call(rbind, all.Results)
			all.Results <- all.Results[, lapply(.SD, sum), .SDcols = c('Results.issues', 'Results.total'), by = Results.meaning] 
			colnames(all.Results) <- c('meaning', 'issues', 'total') 
			all.Warnings <- do.call(rbind, all.Warnings)
			all.Warnings <- all.Warnings[, lapply(.SD, sum), .SDcols = c('Warnings.n.pre', 'Warnings.n.post')] 
			all.Warnings <- paste('Unable to perform check for', all.Warnings[['Warnings.n.pre']] - all.Warnings[['Warnings.n.post']], 'out of', all.Warnings[['Warnings.n.pre']], 'rows.')
				
			## Store output
			tmp <- list(Results = all.Results, Warnings = all.Warnings)
			Warnings[[i]] <- as.data.frame(cbind(m[i,c('TABLE', 'd.DATES', 'v.DATES')], Warning = as.character(tmp$Warnings), stringsAsFactors = FALSE))
			Results[[i]] <- as.data.frame(cbind(m[i,][rep(seq_len(nrow(m[i,])), each = nrow(tmp$Results)),], tmp$Results, stringsAsFactors = FALSE))
	
			## Remove objects and close loop
			rm(d.DATES.colnames, d.DATES, tmp, which.char, all.Results, all.Warnings)
			}

		## Combine all results
		Warnings <- do.call(rbind, Warnings)
		Warnings <- Warnings[!is.na(Warnings$Warning),]
		Warnings <- Warnings[!grepl('Unable to perform check for 0 out of ', Warnings$Warning, fixed = TRUE),]	## Exclude non-issues
		Results <- do.call(rbind, Results)
		
		## Combine per combination of TABLE, v.DATES and Meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 3) colnames(tmp) <- c('TABLE', 'v.DATES', 'Meaning')
		if(ncol(tmp) != 3) stop('Incorrect number of columns when combining results.')
		Results_Comb <- cbind(tmp, Results_Comb)
		Results_Comb$id.tmp <- NULL
		rm(tmp1, tmp2, tmp)
		
		## Also aggregate over meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb_aggr <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb_aggr$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 2) colnames(tmp) <- c('TABLE', 'v.DATES')
		if(ncol(tmp) != 2) stop('Incorrect number of columns when combining results.')
		Results_Comb_aggr <- cbind(tmp, Results_Comb_aggr)
		Results_Comb_aggr$id.tmp <- NULL

		## If a count is lower than 5 but > 0, replace by '<5'
		Results_Comb_HideSmallCounts <- Results_Comb
		Results_Comb_HideSmallCounts$Issues[Results_Comb_HideSmallCounts$Issues < 5 & Results_Comb_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Total[Results_Comb_HideSmallCounts$Total < 5 & Results_Comb_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Issues <- as.character(Results_Comb_HideSmallCounts$Issues)
		Results_Comb_HideSmallCounts$Total <- as.character(Results_Comb_HideSmallCounts$Total)
		
		## Same for aggregated results
		Results_Comb_aggr_HideSmallCounts <- Results_Comb_aggr
		Results_Comb_aggr_HideSmallCounts$Issues[Results_Comb_aggr_HideSmallCounts$Issues < 5 & Results_Comb_aggr_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Total[Results_Comb_aggr_HideSmallCounts$Total < 5 & Results_Comb_aggr_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Issues <- as.character(Results_Comb_aggr_HideSmallCounts$Issues)
		Results_Comb_aggr_HideSmallCounts$Total <- as.character(Results_Comb_aggr_HideSmallCounts$Total)

		## Always set both to character for consistency
		Results_Comb$Issues <- as.character(Results_Comb$Issues)
		Results_Comb$Total <- as.character(Results_Comb$Total)
		Results_Comb_aggr$Issues <- as.character(Results_Comb_aggr$Issues)
		Results_Comb_aggr$Total <- as.character(Results_Comb_aggr$Total)

		## Return output and end function
		return(list(Warnings = Warnings, Results = Results_Comb, Results_HideSmallCounts = Results_Comb_HideSmallCounts, Results_aggr = Results_Comb_aggr, Results_aggr_HideSmallCounts = Results_Comb_aggr_HideSmallCounts))
		}



##################################################################################################################################################
## CHECK 2.6
##################################################################################################################################################

	## Description: Counts how often a date takes place after the visit end date as specified in the visit occurence table.


	##################################################################################################################################################
	## 2.6. 'Inner' function
	##################################################################################################################################################

	## Inner function check 2.6
	fun_check_2.6 <- function(d.VIS_OCC, d.DATES, v.DATES, v.MEANING){

		## INPUT CHECKS
		w1 <- w2 <- NA

		## Variables required
		var.x <- c('visit_occurrence_id', v.DATES, v.MEANING)
		var.y <- c('visit_occurrence_id', 'visit_end_date')

		## Check
		if(any(!(var.y %in% colnames(d.VIS_OCC))) | any(!(var.x %in% colnames(d.DATES)))){
			w1 <- 'At least one required column is either not in the data or is named incorrectly. Execution halted.'
			return(list(Warnings = w1, Results = data.frame(Meaning = NA, issues = NA, total = NA)))
			}

		## n.pre
		n.pre <- nrow(d.DATES)

		## Set visit_occurrence_id variables to character in both datasets
		d.VIS_OCC[['visit_occurrence_id']] <- as.character(d.VIS_OCC[['visit_occurrence_id']])
		d.DATES[['visit_occurrence_id']] <- as.character(d.DATES[['visit_occurrence_id']])
		d.DATES[['person_id']] <- as.character(d.DATES[['person_id']])

		## If meaning == NA, replace with (NOT ENTERED)
		d.DATES[,(v.MEANING) := ifelse(is.na(get(v.MEANING)), '(NOT ENTERED)', get(v.MEANING))]
		uniq.meaning <- unique(d.DATES[[v.MEANING]])

		## Remove missing values
		d.DATES <- d.DATES[!is.na(get(v.DATES)) & !is.na(visit_occurrence_id),]
		d.VIS_OCC <- d.VIS_OCC[!is.na(visit_occurrence_id),]

		## Left join d.DATES and d.VIS_OCC
		d.DATES <- merge(d.DATES[,c(v.DATES, 'visit_occurrence_id', v.MEANING), with = F], d.VIS_OCC, by = 'visit_occurrence_id', all.x = TRUE, all.y = FALSE, allow.cartesian = F)

		## Set date values to class date, if not already done
		if(class(d.DATES[[v.DATES]]) != 'Date') d.DATES[,(v.DATES) := as.Date(as.character(d.DATES[[v.DATES]]), format = '%Y%m%d')]
		if(class(d.DATES[['visit_end_date']]) != 'Date') d.DATES[,visit_end_date := as.Date(as.character(d.DATES[['visit_end_date']]), format = '%Y%m%d')]

		## If v.DATES or visit_end_date is missing, remove.
		d.DATES <- d.DATES[complete.cases(d.DATES),]

		## n.post
		n.post <- nrow(d.DATES)

		## Warning
		if(n.pre != n.post) w2 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')
	
		## Count issues per meaning
		Results <- d.DATES[, .(issues = sum(get(v.DATES) > visit_end_date), total = .N), by = c(v.MEANING)]
		colnames(Results)[colnames(Results) == v.MEANING] <- 'meaning'

		## If Results is empty, replace by NAs
		if(nrow(Results) == 0){
			if(length(uniq.meaning) == 0) uniq.meaning <- '(NOT ENTERED)'
			Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = 0, total = 0))
			}

		## Return output
		return(list(Warnings = data.table(n.pre = n.pre, n.post = n.post), Results = Results))
		}


	##################################################################################################################################################
	## 2.6. 'Outer' function
	##################################################################################################################################################

	## Outer function check 2.6
	run_check_2.6 <- function(dir.data, nb_row = NB){

		## If dir.data doesn't end with /, add it
		if(substr(dir.data, nchar(dir.data), nchar(dir.data)) != '/') dir.data <- paste0(dir.data, '/')

		## Get all files, use only files ending on .csv
		files <- dir(dir.data, pattern = '.csv')

		## Combine info in one matrix
		m <- rbind(
			expand.grid(TABLE = 'EVENTS', d.DATES = files[grepl('^EVENTS', files)], 					v.DATES = c('start_date_record'), 				v.MEANING = 'meaning_of_event', 		stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICINES', d.DATES = files[grepl('^MEDICINES', files)], 				v.DATES = c('date_dispensing', 'date_prescription'), 	v.MEANING = 'meaning_of_drug_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'PROCEDURES', d.DATES = files[grepl('^PROCEDURES', files)], 				v.DATES = c('procedure_date'), 				v.MEANING = 'meaning_of_procedure', 	stringsAsFactors = F),
			expand.grid(TABLE = 'VACCINES', d.DATES = files[grepl('^VACCINES', files)], 					v.DATES = c('vx_record_date', 'vx_admin_date'), 	v.MEANING = 'meaning_of_vx_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICAL_OBSERVATIONS', d.DATES = files[grepl('^MEDICAL_OBSERVATIONS', files)], 	v.DATES = c('mo_date'), 					v.MEANING = 'mo_meaning', 			stringsAsFactors = F)
			)

		## Output objects
		Warnings <- vector('list', length = nrow(m))
		Results <- vector('list', length = nrow(m))

		## Collect all visit_occurrence_ids and start dates
		m0 <- expand.grid(TABLE = 'VISIT_OCCURRENCE', d = files[grepl('^VISIT_OCCURRENCE', files)], stringsAsFactors = F)
		d.VIS_OCC <- vector('list', length = nrow(m0))
		for(i in 1:nrow(m0)){
		 	d <- fread(paste0(dir.data, m0$d[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)[, c('visit_occurrence_id', 'visit_end_date'), with = F]
			d.colnames <- colnames(d)
			if(ncol(d) == 1 | length(colnames(d)) != length(d.colnames)) stop(paste0('Problem with importing ', m$d[i], ', please check if formatted correctly.'))
			if(all(c('visit_occurrence_id', 'visit_end_date') %in% d.colnames)) d.VIS_OCC[[i]] <- d[,c('visit_occurrence_id', 'visit_end_date')]
			if(!all(c('visit_occurrence_id', 'visit_end_date') %in% d.colnames)) d.VIS_OCC[[i]] <- NULL ## Only use vis_occ data when both variables are available
			d$visit_occurrence_id <- as.character(d$visit_occurrence_id) ## set visit_occurrence_id to character
			rm(d, d.colnames)
			}

		## rbind 
		d.VIS_OCC <- do.call(rbind, d.VIS_OCC)

		## Replace character values '' and ' ' by NA.
		which.char <- which(sapply(1:ncol(d.VIS_OCC), FUN = function(i) class(d.VIS_OCC[[i]]) == 'character'))
		for(i in which.char) if(any(d.VIS_OCC[[i]] == ' ' | d.VIS_OCC[[i]] == '', na.rm = T)) d.VIS_OCC[[i]][!is.na(d.VIS_OCC[[i]]) & (d.VIS_OCC[[i]] == ' ' | d.VIS_OCC[[i]] == '')] <- NA
		rm(which.char)

		## Remove duplicated rows, check if there are still duplicated visit_occurrence_id values after that. If so -> error
		d.VIS_OCC <- d.VIS_OCC[!duplicated(paste(d.VIS_OCC$visit_occurrence_id, d.VIS_OCC$visit_end_date)),]
		if(any(duplicated(d.VIS_OCC$visit_occurrence_id))) stop('VISIT_OCCURRENCE contains duplicated visit_occurrence_id values, with different end dates. Execution halted.')

		## If d.VIS_OCC is empty, give an error
		if(nrow(d.VIS_OCC) == 0) stop('No VISIT_OCCURRENCE data.')

		## Run loop
		for(i in 1:nrow(m)){

			## Read data
			d.DATES <- fread(paste0(dir.data, m$d.DATES[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)
			d.DATES.colnames <- colnames(fread(paste0(dir.data, m$d.DATES[i]), sep = ',', nrows = 0, stringsAsFactors = FALSE))
			if(ncol(d.DATES) == 1 | length(colnames(d.DATES)) != length(d.DATES.colnames)) stop(paste0('Problem with importing ', m$d.DATES[i], ', please check if formatted correctly.'))
			if(nrow(d.DATES) == 0) next 	## If the data file is empty - continue to next		

			## Split data in 'numchunks' chunks to limit memory issues - note, since the function counts unique ID values, we should first remove rows in duplicate
			numchunks <- ceiling(nrow(d.DATES) / nb_row)
			all.Results <- list()		## Temporarily store output		
			all.Warnings <- list()		## Temporarily store output

			## Start for loop per chunk
			for(k in 1:numchunks){
				  
				## Select chunk
				if(k < numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):(k * nb_row),]
				if(k == numchunks) 	tmp.d.DATES <- d.DATES[((k - 1) * nb_row + 1):nrow(d.DATES),]
			
				## Formatting empty strings
				which.char <- which(sapply(1:ncol(tmp.d.DATES), FUN = function(j) class(tmp.d.DATES[[j]]) == 'character'))
				for(j in which.char) if(any(tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '', na.rm = T)) tmp.d.DATES[[j]][!is.na(tmp.d.DATES[[j]]) & (tmp.d.DATES[[j]] == ' ' | tmp.d.DATES[[j]] == '')] <- NA
			
				## Run inner function on chunk
				tmp.Out <- fun_check_2.6(d.VIS_OCC = d.VIS_OCC, d.DATES = tmp.d.DATES, v.DATES = m$v.DATES[i], v.MEANING = m$v.MEANING[i])
				all.Results[[k]] <- as.data.table(tmp.Out["Results"])
				all.Warnings[[k]] <- as.data.table(tmp.Out["Warnings"])
				
				## Remove objects and close loop
				rm(tmp.d.DATES, tmp.Out) 
				}	 	

			## Combine the chunk-specific results and warnings
			all.Results <- do.call(rbind, all.Results)
			all.Results <- all.Results[, lapply(.SD, sum), .SDcols = c('Results.issues', 'Results.total'), by = Results.meaning] 
			colnames(all.Results) <- c('meaning', 'issues', 'total') 
			all.Warnings <- do.call(rbind, all.Warnings)
			all.Warnings <- all.Warnings[, lapply(.SD, sum), .SDcols = c('Warnings.n.pre', 'Warnings.n.post')] 
			all.Warnings <- paste('Unable to perform check for', all.Warnings[['Warnings.n.pre']] - all.Warnings[['Warnings.n.post']], 'out of', all.Warnings[['Warnings.n.pre']], 'rows.')
				
			## Store output
			tmp <- list(Results = all.Results, Warnings = all.Warnings)
			Warnings[[i]] <- as.data.frame(cbind(m[i,c('TABLE', 'd.DATES', 'v.DATES')], Warning = as.character(tmp$Warnings), stringsAsFactors = FALSE))
			Results[[i]] <- as.data.frame(cbind(m[i,][rep(seq_len(nrow(m[i,])), each = nrow(tmp$Results)),], tmp$Results, stringsAsFactors = FALSE))
	
			## Remove objects and close loop
			rm(d.DATES.colnames, d.DATES, tmp, which.char, all.Results, all.Warnings)
			}

		## Combine all results
		Warnings <- do.call(rbind, Warnings)
		Warnings <- Warnings[!is.na(Warnings$Warning),]
		Warnings <- Warnings[!grepl('Unable to perform check for 0 out of ', Warnings$Warning, fixed = TRUE),]	## Exclude non-issues
		Results <- do.call(rbind, Results)
		
		## Combine per combination of TABLE, v.DATES and Meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, Results$meaning, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 3) colnames(tmp) <- c('TABLE', 'v.DATES', 'Meaning')
		if(ncol(tmp) != 3) stop('Incorrect number of columns when combining results.')
		Results_Comb <- cbind(tmp, Results_Comb)
		Results_Comb$id.tmp <- NULL
		rm(tmp1, tmp2, tmp)
		
		## Also aggregate over meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$v.DATES, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb_aggr <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb_aggr$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 2) colnames(tmp) <- c('TABLE', 'v.DATES')
		if(ncol(tmp) != 2) stop('Incorrect number of columns when combining results.')
		Results_Comb_aggr <- cbind(tmp, Results_Comb_aggr)
		Results_Comb_aggr$id.tmp <- NULL

		## If a count is lower than 5 but > 0, replace by '<5'
		Results_Comb_HideSmallCounts <- Results_Comb
		Results_Comb_HideSmallCounts$Issues[Results_Comb_HideSmallCounts$Issues < 5 & Results_Comb_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Total[Results_Comb_HideSmallCounts$Total < 5 & Results_Comb_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Issues <- as.character(Results_Comb_HideSmallCounts$Issues)
		Results_Comb_HideSmallCounts$Total <- as.character(Results_Comb_HideSmallCounts$Total)
		
		## Same for aggregated results
		Results_Comb_aggr_HideSmallCounts <- Results_Comb_aggr
		Results_Comb_aggr_HideSmallCounts$Issues[Results_Comb_aggr_HideSmallCounts$Issues < 5 & Results_Comb_aggr_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Total[Results_Comb_aggr_HideSmallCounts$Total < 5 & Results_Comb_aggr_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Issues <- as.character(Results_Comb_aggr_HideSmallCounts$Issues)
		Results_Comb_aggr_HideSmallCounts$Total <- as.character(Results_Comb_aggr_HideSmallCounts$Total)

		## Always set both to character for consistency
		Results_Comb$Issues <- as.character(Results_Comb$Issues)
		Results_Comb$Total <- as.character(Results_Comb$Total)
		Results_Comb_aggr$Issues <- as.character(Results_Comb_aggr$Issues)
		Results_Comb_aggr$Total <- as.character(Results_Comb_aggr$Total)

		## Return output and end function
		return(list(Warnings = Warnings, Results = Results_Comb, Results_HideSmallCounts = Results_Comb_HideSmallCounts, Results_aggr = Results_Comb_aggr, Results_aggr_HideSmallCounts = Results_Comb_aggr_HideSmallCounts))
		}


##################################################################################################################################################
## CHECK 2.7
##################################################################################################################################################

	## Description: shows the percentage of entries for which the person_id value does not correspond to the person_id in VISIT_OCCURRENCE
	##

	##################################################################################################################################################
	## 2.7. 'Inner' function
	##################################################################################################################################################

	## Inner function check 2.7
	fun_check_2.7 <- function(d.VIS_OCC, d, v.MEANING){

		## INPUT CHECKS
		w1 <- w2 <- NA

		## Variables required
		var.x <- c('visit_occurrence_id', 'person_id', v.MEANING)
		var.y <- c('visit_occurrence_id', 'person_id')

		## Check
		if(any(!(var.y %in% colnames(d.VIS_OCC))) | any(!(var.x %in% colnames(d)))){
			w1 <- 'At least one required column is either not in the data or is named incorrectly. Execution halted.'
			return(list(Warnings = w1, Results = data.frame(Meaning = NA, issues = NA, total = NA)))
			}

		## n.pre
		n.pre <- nrow(d)

		## Set visit_occurrence_id variables to character in both datasets
		d.VIS_OCC[['visit_occurrence_id']] <- as.character(d.VIS_OCC[['visit_occurrence_id']])
		d[['visit_occurrence_id']] <- as.character(d[['visit_occurrence_id']])
		d[['person_id']] <- as.character(d[['person_id']])

		## If meaning == NA, replace with (NOT ENTERED)
		d[,(v.MEANING) := ifelse(is.na(get(v.MEANING)), '(NOT ENTERED)', get(v.MEANING))]
		uniq.meaning <- unique(d[[v.MEANING]])

		## Remove missing values
		d <- d[!is.na(visit_occurrence_id),]
		d.VIS_OCC <- d.VIS_OCC[!is.na(visit_occurrence_id),]

		## Left join d.DATES and d.VIS_OCC
		d <- merge(d[,c('person_id', 'visit_occurrence_id', v.MEANING), with = F], d.VIS_OCC, by = 'visit_occurrence_id', all.x = TRUE, all.y = FALSE, allow.cartesian = F)

		## If any value is missing, remove.
		d <- d[complete.cases(d),]

		## n.post
		n.post <- nrow(d)

		## Warning
		if(n.pre != n.post) w2 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')

		## Count issues per meaning
		Results <- d[, .(issues = sum(person_id.x != person_id.y), total = .N), by = c(v.MEANING)]
		colnames(Results)[colnames(Results) == v.MEANING] <- 'meaning'

		## If Results is empty, replace by NAs
		if(nrow(Results) == 0){
			if(length(uniq.meaning) == 0) uniq.meaning <- '(NOT ENTERED)'
			Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = 0, total = 0))
			}

		## Return output
		return(list(Warnings = data.table(n.pre = n.pre, n.post = n.post), Results = Results))
		}


	##################################################################################################################################################
	## 2.7. 'Outer' function
	##################################################################################################################################################

	## Outer function check 2.7
	run_check_2.7 <- function(dir.data, nb_row = NB){

		## If dir.data doesn't end with /, add it
		if(substr(dir.data, nchar(dir.data), nchar(dir.data)) != '/') dir.data <- paste0(dir.data, '/')

		## Get all files, use only files ending on .csv
		files <- dir(dir.data, pattern = '.csv')

		## Combine info in one matrix
		m <- rbind(
			expand.grid(TABLE = 'EVENTS', d = files[grepl('^EVENTS', files)], 					 	v.MEANING = 'meaning_of_event', 		stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICINES', d = files[grepl('^MEDICINES', files)], 				 	v.MEANING = 'meaning_of_drug_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'PROCEDURES', d = files[grepl('^PROCEDURES', files)], 					v.MEANING = 'meaning_of_procedure', 	stringsAsFactors = F),
			expand.grid(TABLE = 'VACCINES', d = files[grepl('^VACCINES', files)], 						v.MEANING = 'meaning_of_vx_record', 	stringsAsFactors = F),
			expand.grid(TABLE = 'MEDICAL_OBSERVATIONS', d = files[grepl('^MEDICAL_OBSERVATIONS', files)], 		v.MEANING = 'mo_meaning', 			stringsAsFactors = F)
			)

		## Output objects
		Warnings <- vector('list', length = nrow(m))
		Results <- vector('list', length = nrow(m))

		## Collect all visit_occurrence_ids and start dates
		m0 <- expand.grid(TABLE = 'VISIT_OCCURRENCE', d = files[grepl('^VISIT_OCCURRENCE', files)], stringsAsFactors = F)
		d.VIS_OCC <- vector('list', length = nrow(m0))
		for(i in 1:nrow(m0)){
			d <- fread(paste0(dir.data, m0$d[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)[,c('visit_occurrence_id', 'person_id'), with = F]
			d.colnames <- colnames(d)
			if(ncol(d) == 1 | length(colnames(d)) != length(d.colnames)) stop(paste0('Problem with importing ', m$d[i], ', please check if formatted correctly.'))
			if(all(c('visit_occurrence_id', 'person_id') %in% d.colnames)) d.VIS_OCC[[i]] <- d[,c('visit_occurrence_id', 'person_id')]
			if(!all(c('visit_occurrence_id', 'person_id') %in% d.colnames)) d.VIS_OCC[[i]] <- NULL ## Only use vis_occ data when both variables are available
			rm(d, d.colnames)
			}

		## rbind
		d.VIS_OCC <- do.call(rbind, d.VIS_OCC)

		## Replace character values '' and ' ' by NA.
		which.char <- which(sapply(1:ncol(d.VIS_OCC), FUN = function(i) class(d.VIS_OCC[[i]]) == 'character'))
		for(i in which.char) if(any(d.VIS_OCC[[i]] == ' ' | d.VIS_OCC[[i]] == '', na.rm = T)) d.VIS_OCC[[i]][!is.na(d.VIS_OCC[[i]]) & (d.VIS_OCC[[i]] == ' ' | d.VIS_OCC[[i]] == '')] <- NA
		rm(which.char)

		## Remove duplicated rows, check if there are still duplicated visit_occurrence_id values after that. If so -> error
		d.VIS_OCC <- d.VIS_OCC[!duplicated(paste(d.VIS_OCC$visit_occurrence_id, d.VIS_OCC$person_id)),]
		if(any(duplicated(d.VIS_OCC$visit_occurrence_id))) stop('VISIT_OCCURRENCE contains duplicated visit_occurrence_id values, with different person_id values. Execution halted.')

		## If d.VIS_OCC is empty, give an error
		if(nrow(d.VIS_OCC) == 0) stop('No VISIT_OCCURRENCE data.')

		## Run loop
		for(i in 1:nrow(m)){

			## Read data
			d <- fread(paste0(dir.data, m$d[i]), sep = ',', stringsAsFactors = FALSE, fill = TRUE)
			d.colnames <- colnames(fread(paste0(dir.data, m$d[i]), sep = ',', nrows = 0, stringsAsFactors = FALSE))
			if(ncol(d) == 1 | length(colnames(d)) != length(d.colnames)) stop(paste0('Problem with importing ', m$d[i], ', please check if formatted correctly.'))
			if(nrow(d) == 0) next 	## If the data file is empty - continue to next		
	
			## Split data in 'numchunks' chunks to limit memory issues - note, since the function counts unique ID values, we should first remove rows in duplicate
			numchunks <- ceiling(nrow(d) / nb_row)
			all.Results <- list()		## Temporarily store output		
			all.Warnings <- list()		## Temporarily store output

			## Start for loop per chunk
			for(k in 1:numchunks){
				  
				## Select chunk
				if(k < numchunks) 	tmp.d <- d[((k - 1) * nb_row + 1):(k * nb_row),]
				if(k == numchunks) 	tmp.d <- d[((k - 1) * nb_row + 1):nrow(d),]
		
				## Formatting empty strings
				which.char <- which(sapply(1:ncol(tmp.d), FUN = function(j) class(tmp.d[[j]]) == 'character'))
				for(j in which.char) if(any(tmp.d[[j]] == ' ' | tmp.d[[j]] == '', na.rm = T)) tmp.d[[j]][!is.na(tmp.d[[j]]) & (tmp.d[[j]] == ' ' | tmp.d[[j]] == '')] <- NA
			
				## Run inner function on chunk
				tmp.Out <- fun_check_2.7(d.VIS_OCC = d.VIS_OCC, d = tmp.d, v.MEANING = m$v.MEANING[i])
				all.Results[[k]] <- as.data.table(tmp.Out["Results"])
				all.Warnings[[k]] <- as.data.table(tmp.Out["Warnings"])
				
				## Remove objects and close loop
				rm(tmp.d, tmp.Out) 
				}

			## Combine the chunk-specific results and warnings
			all.Results <- do.call(rbind, all.Results)
			all.Results <- all.Results[, lapply(.SD, sum), .SDcols = c('Results.issues', 'Results.total'), by = Results.meaning] 
			colnames(all.Results) <- c('meaning', 'issues', 'total') 
			all.Warnings <- do.call(rbind, all.Warnings)
			all.Warnings <- all.Warnings[, lapply(.SD, sum), .SDcols = c('Warnings.n.pre', 'Warnings.n.post')] 
			all.Warnings <- paste('Unable to perform check for', all.Warnings[['Warnings.n.pre']] - all.Warnings[['Warnings.n.post']], 'out of', all.Warnings[['Warnings.n.pre']], 'rows.')
				
			## Store output
			tmp <- list(Results = all.Results, Warnings = all.Warnings)
			Warnings[[i]] <- as.data.frame(cbind(m[i,c('TABLE', 'd')], Warning = as.character(tmp$Warnings), stringsAsFactors = FALSE))
			Results[[i]] <- as.data.frame(cbind(m[i,][rep(seq_len(nrow(m[i,])), each = nrow(tmp$Results)),], tmp$Results, stringsAsFactors = FALSE))
	
			## Remove objects and close loop
			rm(d.colnames, d, tmp, which.char, all.Results, all.Warnings)
			}

		## Combine all results
		Warnings <- do.call(rbind, Warnings)
		Warnings <- Warnings[!is.na(Warnings$Warning),]
		Warnings <- Warnings[!grepl('Unable to perform check for 0 out of ', Warnings$Warning, fixed = TRUE),]	## Exclude non-issues
		Results <- do.call(rbind, Results)

		## Combine per combination of TABLE, v.DATES and Meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, Results$meaning, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, Results$meaning, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 2) colnames(tmp) <- c('TABLE', 'Meaning')
		if(ncol(tmp) != 2) stop('Incorrect number of columns when combining results.')
		Results_Comb <- cbind(tmp, Results_Comb)
		Results_Comb$id.tmp <- NULL
		rm(tmp1, tmp2, tmp)
		
		## Also aggregate over meaning
		tmp1 <- aggregate(Results$issues, list(paste(Results$TABLE, sep = '; ')), sum)
		tmp2 <- aggregate(Results$total, list(paste(Results$TABLE, sep = '; ')), sum)
		colnames(tmp1) <- c('id.tmp', 'Issues')
		colnames(tmp2) <- c('id.tmp', 'Total')
		Results_Comb_aggr <- merge(tmp1, tmp2, by = 'id.tmp')
		tmp <- as.data.frame(do.call(rbind, strsplit(Results_Comb_aggr$id.tmp, '; ')), stringsAsFactors = FALSE)
		if(ncol(tmp) == 1) colnames(tmp) <- c('TABLE')
		if(ncol(tmp) != 1) stop('Incorrect number of columns when combining results.')
		Results_Comb_aggr <- cbind(tmp, Results_Comb_aggr)
		Results_Comb_aggr$id.tmp <- NULL

		## If a count is lower than 5 but > 0, replace by '<5'
		Results_Comb_HideSmallCounts <- Results_Comb
		Results_Comb_HideSmallCounts$Issues[Results_Comb_HideSmallCounts$Issues < 5 & Results_Comb_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Total[Results_Comb_HideSmallCounts$Total < 5 & Results_Comb_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_HideSmallCounts$Issues <- as.character(Results_Comb_HideSmallCounts$Issues)
		Results_Comb_HideSmallCounts$Total <- as.character(Results_Comb_HideSmallCounts$Total)
		
		## Same for aggregated results
		Results_Comb_aggr_HideSmallCounts <- Results_Comb_aggr
		Results_Comb_aggr_HideSmallCounts$Issues[Results_Comb_aggr_HideSmallCounts$Issues < 5 & Results_Comb_aggr_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Total[Results_Comb_aggr_HideSmallCounts$Total < 5 & Results_Comb_aggr_HideSmallCounts$Total > 0] <- '<5' 		
		Results_Comb_aggr_HideSmallCounts$Issues <- as.character(Results_Comb_aggr_HideSmallCounts$Issues)
		Results_Comb_aggr_HideSmallCounts$Total <- as.character(Results_Comb_aggr_HideSmallCounts$Total)

		## Always set both to character for consistency
		Results_Comb$Issues <- as.character(Results_Comb$Issues)
		Results_Comb$Total <- as.character(Results_Comb$Total)
		Results_Comb_aggr$Issues <- as.character(Results_Comb_aggr$Issues)
		Results_Comb_aggr$Total <- as.character(Results_Comb_aggr$Total)

		## Return output and end function
		return(list(Warnings = Warnings, Results = Results_Comb, Results_HideSmallCounts = Results_Comb_HideSmallCounts, Results_aggr = Results_Comb_aggr, Results_aggr_HideSmallCounts = Results_Comb_aggr_HideSmallCounts))
		}


##################################################################################################################################################
## CHECK 2.8
##################################################################################################################################################

	## Description: Check if parents are <12 years older than child
	## (note: compared to the other checks, this check is more simple and therefore contains no inner/outer functions nor does it need to be run in chunks)


	##################################################################################################################################################
	## 2.8. Function
	##################################################################################################################################################

	## Outer function check 2.8 (note: no inner function is needed, only two datasets are compared)
	run_check_2.8 <- function(dir.data){

		## If dir.data doesn't end with /, add it
		if(substr(dir.data, nchar(dir.data), nchar(dir.data)) != '/') dir.data <- paste0(dir.data, '/')

		## Import PERSONS (+ basic check if input is correctly formatted). Replace '' and ' ' by NA.
		d.PERSONS <- IMPORT_PATTERN(pat = "PERSONS", dir = dir.data, colls = c('person_id', 'day_of_birth', 'month_of_birth', 'year_of_birth'))
		d.PERSONS.colnames <- colnames(d.PERSONS)	
		lapply(c('day_of_birth', 'month_of_birth', 'year_of_birth'), function(x) d.PERSONS <- d.PERSONS[, eval(x) := as.numeric(get(x))])
		
		if(ncol(d.PERSONS) == 1 | length(colnames(d.PERSONS)) != length(d.PERSONS.colnames)) stop('Problem with importing PERSONS.csv, please check if formatted correctly.') 
		#which.char <- which(sapply(1:ncol(d.PERSONS), FUN = function(i) class(d.PERSONS[[i]]) == 'character'))
		#for(i in which.char) if(any(d.PERSONS[[i]] == ' ' | d.PERSONS[[i]] == '', na.rm = T)) d.PERSONS[[i]][!is.na(d.PERSONS[[i]]) & (d.PERSONS[[i]] == ' ' | d.PERSONS[[i]] == '')] <- NA
		#rm(which.char)

		## If d.PERSONS is empty, give an error
		if(nrow(d.PERSONS) == 0) stop('PERSONS.csv is empty.')

		## Import PERSON_RELATIONSHIPS (+ basic check if input is correctly formatted). Replace '' and ' ' by NA.
		d.PER_REL <- fread(paste0(dir.data, 'PERSON_RELATIONSHIPS.csv'), sep = ',', stringsAsFactors = FALSE, fill = TRUE)
		d.PER_REL.colnames <- colnames(fread(paste0(dir.data, 'PERSON_RELATIONSHIPS.csv'), sep = ',', nrows = 0, stringsAsFactors = FALSE))
		if(ncol(d.PER_REL) == 1 | length(colnames(d.PER_REL)) != length(d.PER_REL.colnames)) stop('Problem with importing PERSON_RELATIONSHIPS.csv, please check if formatted correctly.') 
		which.char <- which(sapply(1:ncol(d.PER_REL), FUN = function(i) class(d.PER_REL[[i]]) == 'character'))
		for(i in which.char) if(any(d.PER_REL[[i]] == ' ' | d.PER_REL[[i]] == '', na.rm = T)) d.PER_REL[[i]][!is.na(d.PER_REL[[i]]) & (d.PER_REL[[i]] == ' ' | d.PER_REL[[i]] == '')] <- NA
		rm(which.char)

		## If d.PERSONS is empty, give an error
		if(nrow(d.PER_REL) == 0) stop('PERSON_RELATIONSHIPS.csv is empty.')

		## Variables required
		var.x <- c('person_id', 'related_id', 'meaning_of_relationship')
		var.y <- c('person_id', 'day_of_birth', 'month_of_birth', 'year_of_birth')
		if(any(!(var.y %in% colnames(d.PERSONS))) | any(!(var.x %in% colnames(d.PER_REL)))) stop('At least one required column is either not in the data or is named incorrectly. Execution halted.')

		## Set person_id variables to character in both datasets
		d.PERSONS[['person_id']] <- as.character(d.PERSONS[['person_id']])
		d.PER_REL[['related_id']] <- as.character(d.PER_REL[['related_id']])
		d.PER_REL[['person_id']] <- as.character(d.PER_REL[['person_id']])

		## Select mothers and fathers
		d.PER_REL$moth_fath <- grepl('mother', d.PER_REL$meaning_of_relationship, fixed = TRUE) | grepl('father', d.PER_REL$meaning_of_relationship, fixed = TRUE)
		d.PER_REL <- d.PER_REL[moth_fath == TRUE,]

		## n.pre
		n.pre <- nrow(d.PER_REL)

		## Create date_birth
		#d.PERSONS[, date_birth := paste(year_of_birth, month_of_birth, day_of_birth, sep = '-')] 
		#d.PERSONS[, c('year_of_birth', 'month_of_birth', 'day_of_birth') := NULL]

		## Note: If both day and month arre missing -> set to july 2nd. If only day is missing, use 15. If only month is missing, set to july
		d.PERSONS$day_of_birth[is.na(d.PERSONS$day_of_birth) & is.na(d.PERSONS$month_of_birth)] <- 2
		d.PERSONS$month_of_birth[is.na(d.PERSONS$day_of_birth) & is.na(d.PERSONS$month_of_birth)] <- 7
		d.PERSONS$day_of_birth[is.na(d.PERSONS$day_of_birth) & !is.na(d.PERSONS$month_of_birth)] <- 15
		d.PERSONS$month_of_birth[!is.na(d.PERSONS$day_of_birth) & is.na(d.PERSONS$month_of_birth)] <- 7
		d.PERSONS[, date_birth := paste(year_of_birth, month_of_birth, day_of_birth, sep = '-')] 

		## Merge the two tables
		d.comb <- merge(d.PER_REL, d.PERSONS[,c('person_id', 'date_birth')], by = 'person_id', all.x = T, all.y = F)
		colnames(d.comb)[colnames(d.comb) == 'date_birth'] <- 'date_birth_person'
		colnames(d.PERSONS)[colnames(d.PERSONS) == 'person_id'] <- 'related_id'	
		d.comb <- merge(d.comb, d.PERSONS[,c('related_id', 'date_birth')], by = 'related_id', all.x = T, all.y = F)
		colnames(d.comb)[colnames(d.comb) == 'date_birth'] <- 'date_birth_related'	

		## Set date values to class date, if not already done
		if(class(d.comb[,date_birth_person]) != 'Date') d.comb[,date_birth_person := as.Date(as.character(d.comb[,date_birth_person]), format = '%Y-%m-%d')]
		if(class(d.comb[,date_birth_related]) != 'Date') d.comb[,date_birth_related := as.Date(as.character(d.comb[,date_birth_related]), format = '%Y-%m-%d')]
	
		## Remove missing values
		d.comb <- d.comb[!is.na(d.comb[,date_birth_person]) & !is.na(d.comb[,date_birth_related]),]

		## n.post
		n.post <- nrow(d.comb)
	
		## Warning
		w1 <- NA
		if(n.pre != n.post) w1 <- paste('Unable to perform check for', n.pre - n.post, 'out of', n.pre, 'rows.')

		## Check if date_birth_related is at least 12 yrs after date_birth_person
		d.comb[, yrsdiff := as.numeric((date_birth_person - date_birth_related)/365.25)]
	
		## Count issues per meaning
		Results <- d.comb[, .(Issues = sum(as.numeric(yrsdiff < 12)), Total = .N)]

		## If Results is empty, replace by NAs
		if(nrow(Results) == 0){
			if(length(uniq.meaning) == 0) uniq.meaning <- '(NOT ENTERED)'
			Results <- as.data.table(data.frame(meaning = uniq.meaning, issues = 0, total = 0))
			}
		
		## If a count is lower than 5 but > 0, replace by '<5'
		Results_HideSmallCounts <- Results
		Results_HideSmallCounts$Issues[Results_HideSmallCounts$Issues < 5 & Results_HideSmallCounts$Issues > 0] <- '<5' 		
		Results_HideSmallCounts$Total[Results_HideSmallCounts$Total < 5 & Results_HideSmallCounts$Total > 0] <- '<5' 		
		Results_HideSmallCounts$Issues <- as.character(Results_HideSmallCounts$Issues)
		Results_HideSmallCounts$Total <- as.character(Results_HideSmallCounts$Total)
		
		## Return result
		return(list(Warnings = data.frame(Warnings = w1), Results = as.data.frame(Results), Results_HideSmallCounts = as.data.frame(Results_HideSmallCounts)))
		}
		


