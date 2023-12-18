###########################################
#
#  Name:	wuenic_ground_predicates.R
#  Author:	Tony BURTON
#			Strategic Information
#			Department of Immunization, Vaccines, and Biologics
#			World Health Organization
#
#  Date:	06 February 2010
#  Updated:	19 May 2010
#			30 June 2010
#			03 February 2011
#			05 April 2011
#			16 April 2011
#			06 May 2011 
#			20 May 2012
#			29 April 2012
#			02 May 2013  Modifications for revised database structure
#			23 June 2013 Added MCV2 & HepBB
#			03 May 2014 Added exclusion for reanalyzed survey results
#			01 July 2015 Set significant digits for coverage from admin and survey results.
#			20 April 2016 Added first dose of rubella and at least one IPV for selected countries	 			 
#
#  Depends on:	R ver 2.9.2
#			R libraries: base, RODBC
#			http://www.r-project.org/
#
#  Usage:   Rterm --vanilla -slave <wuenic_ground_predicates.R --args %1
#
#  Input:   WUENIC_DATA.mdb
#
#  Outputs: data.pl file of country-specific coverage data formatted as prolog predicates
#  		wuenic.dat country-specific data for report generation       
#
# TBD: Refactor code producing predicates for "new" vaccines
# TBD: In the survey description table, variable reportType, filter for for 'Yes" only    
########################################################################################
	rm(list=ls())
	library(RODBC)

	cmdLine <- commandArgs()
	country   <- tolower(cmdLine[5])

#country <- 'vnm' # country code for standalone processing/testing

#  Initialize output files
#  -----------------------

	File1Out <- "data.pl"   # country-specific data formatted as prolog predicates
	sink(File1Out)

	File2Out <- "wuenic.dat" # country-specific data for report generation
	cat('CountryName\tDate\tCountryCode\tVaccine\tYear\tSource\tValue\tLevel\tComment\n',
		file = File2Out,append=FALSE)

#  Open input database
#  --------------------
	FileNameIn <- "..\\input\\WUENIC_DATA.mdb"
	FileIn     <- file.path(getwd(),FileNameIn)

	F1         <- odbcConnectAccess(FileIn)
	
#  Country name, codes (UNICEF ACTY and ISO3 & date
#  ------------------------------------------------
	ISOCode  <- paste("'",toupper(country),"'",sep="")
	table        <- "COUNTRY"
	condition    <- paste("country =",ISOCode)

	sqlQueryString <- paste("select country, countryName",
					"from",table,
					"where",condition)

	d <- sqlQuery(F1,sqlQueryString)
	cty <- ISOCode
	CountryName <- d$countryName

	l<-paste("% ",d$countryName,"-",d$country,"\n")

	cat(l)
	cat("% ",date(),"\n\n")
	l <- paste("country(",tolower(country),",'",d$countryName,"').",sep="")
	cat(l,'\n')
	cat("date('",date(),"').\n",sep="")
	cat('\n')

# estimates required
# -------------------
	variables <- "country, vaccine, annum, presentation, comment"
	table     <- "[ESTIMATE_REQUIRED]"
	condition <- paste("country =",tolower(ISOCode),"AND annum >= 1997")

	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition)

	d <- sqlQuery(F1,sqlQueryString,as.is = TRUE)

	Presentation <- d$presentation
	Presentation[is.na(Presentation)] <- 'na'

	Comment <- d$comment
	Comment[is.na(Comment)] <- 'na'

	predicate <- 'estimate_required'
	cat(sprintf("%s(%s,%s,%s,%s,'%s').\n",
			 predicate,
                   tolower(d$country),
			 tolower(d$vaccine),
			 d$annum,
			 tolower(Presentation),
                   tolower(Comment)))
	cat('\n')

#     Administrative data           - Reported coverage, admin
#     National Estimate             - Reported coverage, official
#     Legacy WHO & UNICEF estimates - Estmated coverage, legacy
#  ---------------------------------------------------

extract_coverage_data <- function(table_name,CountryName,country_code,data_type,predicate_name) {
	variables <- "country, annum, vaccine, coverage"
	vaccine_list <- "('bcg','dtp1','dtp3','pol1','pol3','ipv1','mcv1','mcv2','rcv1','hepbb','hepb1','hepb3','hib1','hib3','pcv1','pcv3','rotac','yfv')"

	table     <- table_name

	condition <- paste("country =",country_code,
                         "AND annum >= 1997",
                         "AND coverage > 0",
				 "AND vaccine IN",vaccine_list)

	if (data_type == 'admin' | data_type == 'official') {
		variables <- "country, annum, vaccine, type, coverage"
		condition <- paste(condition," AND type ='",data_type,"'",sep="")
		}

	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition,
					"order by country,vaccine,annum")
		
	d <- sqlQuery(F1,sqlQueryString,as.is = TRUE)

	if (length(row.names(d)) > 0) {
		predicate <- predicate_name
		cov_formatted <- sprintf("%.0f",d$coverage)
 
		cat(sprintf("%s(%s,%s,%s,%s).\n",
			 predicate,
			 tolower(country),
			 tolower(d$vaccine),
			 d$annum,cov_formatted))
		cat('\n')
		write.table(data.frame(CountryName,date(),country,tolower(d$vaccine),d$annum,predicate_name,cov_formatted,'percent',''),
				file = File2Out,sep='\t', quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
		}
}

#  extract coverage data
	extract_coverage_data("REPORTED_COVERAGE",CountryName,cty,"admin","admin")
	extract_coverage_data("REPORTED_COVERAGE",CountryName,cty,"official","gov")
	extract_coverage_data("ESTIMATED_COVERAGE",CountryName,cty,"legacy","legacy")

#     Number children vaccinated    - Reported on Joint Reporting Form
#     Number children in target     - Reported on Joint Reporting Form 
#     Legacy WHO & UNCIEF estimate  - Coverage_estimates.Coverage_type = Legacy estimate (wgd)
#     ---------------------------------------------------------------------------------------

#  Reported number of children vaccinated & targetted
	variables <- "country, vaccine, annum, reportedDenom, reportedNum, coverage"
	table     <- "REPORTED_NUMERATOR_DENOMINATOR"
	condition <- paste("country =",cty,
                         "AND annum >= 1997 AND reportedNum > 0")

	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition,
					"order by country, vaccine, annum")
		
	d <- sqlQuery(F1,sqlQueryString)
	admin <- d
	
	if (length(row.names(d)) > 0) {
		predicate <- 'vaccinated'
		cat(sprintf("%s(%s,%s,%s,%.0f).\n",
			 predicate,
                   tolower(country),
			 tolower(d$vaccine),
			 d$annum,
			 d$reportedNum))
		cat('\n')
		write.table(data.frame(CountryName,date(),country,tolower(d$vaccine),d$annum,'vaccinated',format(d$reportedNum,digits=1),'count',''),
				file = File2Out,sep='\t', quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
		}

	variables <- "country, vaccine, annum, reportedDenom, reportedNum, coverage"
	table     <- "REPORTED_NUMERATOR_DENOMINATOR"
	condition <- paste("country =",cty,
                         "AND annum >= 1997 AND reportedDenom > 0")

	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition,
					"order by country, vaccine, annum")
		
	d <- sqlQuery(F1,sqlQueryString)
	admin <- d

	if (length(row.names(d)) > 0) {
		predicate <- 'target'
		cat(sprintf("%s(%s,%s,%s,%.0f).\n",
			 predicate,
                   tolower(country),
			 tolower(d$vaccine),
			 d$annum,
			 d$reportedDenom))
		cat('\n')
		write.table(data.frame(CountryName,date(),country,tolower(d$vaccine),d$annum,'target',format(d$reportedDenom,digits=1),'count',''),
				file = File2Out,sep='\t', quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
		}

#  UNPD birth
	variables <- "country, annum, indicator, valor"
	table     <- "DEMOGRAPHIC"
	condition <- paste("country =",cty,
                         "AND annum >= 1997 AND indicator ='Births' AND valor > 0")

	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition,
					"order by country, annum")
		
	d <- sqlQuery(F1,sqlQueryString)
	unpd_births <- d
	
	if (length(row.names(d)) > 0) {
		predicate <- 'births_UNPD'
		cat(sprintf("%s(%s,%s,%.0f).\n",
			 predicate,
                   tolower(country),
			 d$annum,
			 d$valor))
		cat('\n')
		write.table(data.frame(CountryName,date(),country,'',d$annum,'unpdBirths',d$valor,'count',''),
				file = File2Out,sep='\t', quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
		}

#  UNPD surviving infants
	variables <- "country, annum, indicator, valor"
	table     <- "DEMOGRAPHIC"
	condition <- paste("country =",cty,
                         "AND annum >= 1997 AND indicator ='SI' AND valor > 0")

	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition,
					"order by country, annum")
		
	d <- sqlQuery(F1,sqlQueryString)
	unpd_si <- d
	
	if (length(row.names(d)) > 0) {
		predicate <- 'si_UNPD'
		cat(sprintf("%s(%s,%s,%.0f).\n",
			 predicate,
                   tolower(country),
			 d$annum,
			 d$valor))
		cat('\n')
		write.table(data.frame(CountryName,date(),country,'',d$annum,'unpdSI',d$valor,'count',''),
				file = File2Out,sep='\t', quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)

		}

#  Coverage recalculated using number vaccinated / UNPD denominator
	
	if (length(row.names(admin)) > 0) {
		si     <- data.frame(toupper(country),unpd_si$annum,unpd_si$valor)
		births <- data.frame(toupper(country),unpd_births$annum,unpd_births$valor)

		recal_data <- merge(admin,births,by.x = c("country","annum"), by.y = c("toupper.country.","unpd_births.annum"))
		recal_data <- merge(recal_data,si,by.x = c("country","annum"), by.y = c("toupper.country.","unpd_si.annum"))
		covUNPDDenom <- recal_data$reportedNum * 100 / recal_data$unpd_si.valor

		bcg_index <- recal_data[,3] == 'BCG'
		covUNPDDenom[bcg_index] <- recal_data$reportedNum[bcg_index] * 100 / recal_data$unpd_births.valor[bcg_index]
		
		covUNPDDenom[covUNPDDenom > 139] <- 139

		write.table(data.frame(CountryName,date(),country,tolower(recal_data$vaccine),recal_data$annum,'covUNPDDenom',covUNPDDenom,'percent',''),
				file = File2Out,sep='\t', quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
		}

#  Survey results
#	variables <- paste("UNICEF_ACTY_code,",     ?
#					"Year_publcation,",         ?
#					"Title_original_language,", ? Title_english
#					"Survey_method,",           ?
#					"Year_data_collection,",    ?
#					"Percent_cards_seen,",      ?
#					"Vaccine,",                 ?
# 					"Confirmation_method,",     ? card, card or history, history
#					"Age_group,",               ?
#					"Cohort_year,",             ?
#					"Schedule_compliance,",     ?
#					"Number_children,",         ?
#					"Coverage_rate,",           ? > 0
#					"Comments")
#
# conditions = bcg, dtp1,dtp3,pol1,pol3,ipv1,mcv1,mcv2,rcv1,hepbb,hepb1,hepb3,hib1,hib3,pcv3,rotac,yfv

# Get survey ids for country
# --------------------------
	variables 	<- "*"
	table		<- "SURVEY_DESCRIPTION"
	condition	<- paste("ISO3 =",cty)
	
	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition,
					"order by surveyId")

	survey_desc <- sqlQuery(F1,sqlQueryString)

	surveyIds <- as.vector(survey_desc$surveyId)

# Get survey results for country
# ------------------------------
	surveyId_condition <- paste("('",surveyIds[1],sep="")
	for (i in 2:length(surveyIds)) {
		surveyId_condition <- paste(surveyId_condition,surveyIds[i],sep="','")
	}
 	surveyId_condition = paste(surveyId_condition,"')",sep="") 

	variables 	<- "*"
	table		<- "SURVEY_COVERAGE"
	condition 	<- paste("surveyId IN",surveyId_condition,
				   "AND reanalyzed = No",
				   "AND cohortYear >= 1997",
				   "AND validity = 'crude'",
				   "AND vaccine IN ('bcg','dtp1','dtp3','pol1','pol3','ipv1','mcv1','mcv2','rcv1','hepbb','hepb1','hepb3','hib1','hib3','pcv1','pcv3','rotac','yfv')")
	
	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition,
					"order by surveyId")

	survey_details <- sqlQuery(F1,sqlQueryString)

# Merge survey details with survey description
# --------------------------------------------
	survey <- merge(survey_details,survey_desc,by = c('surveyId'))

	if (nrow(survey) > 0 ) {
		survey$coverage[is.na(survey$coverage)] <- 0
		survey$cardsSeen[is.na(survey$cardsSeen)] <- 0
		survey$denominator[is.na(survey$denominator)] <- 0
		
		survey$cardsSeen[!is.numeric(survey$cardsSeen)] <- 0
		survey$coverage[!is.numeric(survey$coverage)]   <- 0
		
		Cards    <- format(survey$cardsSeen,digits=1)
		Coverage <- format(survey$coverage,digits=1)
		Children <- as.character(survey$denominator)
		
		predicate <- 'survey_results'
		cat(sprintf("%s(%s,%s,%s,%s,[title:'%s',type:'%s',yrcoll:'%s',cr:%s,confirm:'%s',age:'%s',timead:'',val:'%s',ss:%s],%s).\n\n",
			predicate,
			tolower(country),
			tolower(survey$vaccine),
			survey$cohortYear,
			tolower(survey$surveyId),
			gsub("'","",survey$surveyNameEnglish),
			survey$surveyType,
			substring(survey$collectBegin,1,4),
			Cards,
			sub(' +$','',tolower(survey$evidence)),
			tolower(survey$ageInterview),
			tolower(survey$validity),
			Children,
			Coverage))
		cat('\n')

		type <- paste(rep('survey: ',nrow(survey)),tolower(survey$evidence),': ',tolower(survey$ageInterview),sep='')
		write.table(data.frame(CountryName,date(),country,tolower(survey$vaccine),survey$cohortYear,type,Coverage,'percent',''),
				file = File2Out,sep='\t', quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)

		}

#  Legacy estimate for 1997
#  ------------------------
	variables <- "country, annum, vaccine, coverage"
	table     <- "ESTIMATED_COVERAGE"
	condition <- paste("country =",cty,
                         "AND annum = 1997", 
                         "AND vaccine IN ('bcg','dtp1','dtp3','pol3','ipv1','mcv1','mcv2','rcv1','hepbb','hepb3','hib3','pcv3','rotac')")

	sqlQueryString <- paste("select",variables,
					"from",table,
					"where",condition,
					"order by country, vaccine, annum")
		
	d <- sqlQuery(F1,sqlQueryString)

	vaccine <- as.character(d$vaccine)
	vaccine[is.na(vaccine)] <- 'na'

	yearBegin <- 1997
	yearEnd   <- 1997
	coverageBegin <- 'na'
	cov_formatted <- sprintf("%.0f",d$coverage)

	assigned_coverageBegin <- as.numeric(cov_formatted)
	assigned_coverageBegin[is.na(assigned_coverageBegin)] <- 'na'

	coverageEnd <- '_'
	assigned_coverageEnd <- 'na'

	predicate <- 'wgd'

	cat(sprintf("%s(%s,%s,%s,%s,%s,'%s',%s,%s,%s,%s).\n",
			 predicate,
			 tolower(country),
			 tolower(vaccine),
			 yearBegin,
			 yearEnd,
			 'assignAnchor',
		       'Legacy estimate.',
			 coverageBegin,
                   assigned_coverageBegin,
                   coverageEnd,
			 assigned_coverageEnd))
	cat('\n')

# working group decision - version 2 format
#
# represents both point and interval decisions as intervals
#
# point decisions are:
#	comment,
#	acceptReported, ignoreReported,
#	acceptSurvey, ignoreSurvey,
#	assignAnchor, assignWUENIC
#   interval decisions are:
#	reportedSegment, interpolateSegment, calibrateSegment.
# ----------------------------------------------------------
	table     <- "WGD"
	condition <- paste("country =",cty)

	sqlQueryString <- paste("select *",
					"from",table,
					"where",condition)

	d <- sqlQuery(F1,sqlQueryString)

	vaccine <- tolower(as.character(d$vaccine))

######## Note hack
######## Working group decision vaccine coded as all rather than na
########

	#vaccine[is.na(vaccine)] <- 'X'
	vaccine[vaccine == 'all'] <- 'X'

######## End vaccine X/all hack

######## Note hack
######## Working group decision justification coded in comment field
########

	#justification <- as.character(d$justification)
	justification <- as.character(d$comment)

######## End comment/justification hack

	yearBegin <- d$yearBegin
	yearBegin[is.na(yearBegin)] <- 1749   # Edward Jenner's year of birth

	surveyID <- tolower(as.character(d$identifyCoverage))
	surveyID[is.na(surveyID)] <- 'na'

	assigned_coverageBegin <- as.numeric(d$assignCoverage)
	assigned_coverageBegin[is.na(assigned_coverageBegin)] <- 'na'

	yearEnd <- d$yearEnd
	yearEnd[is.na(yearEnd)] <- 2203 # project maximun existance of WHO. Gott's principle, 2011 - 1948 (50%CI)

	coverageEnd <- 'na'
	assigned_coverageEnd <- 'na'

	predicate <- 'wgd'

	cat(sprintf("%s(%s,%s,%s,%s,%s,'%s',%s,%s,%s,%s).\n",
			 predicate,
			 tolower(country),
			 vaccine,
			 yearBegin,
			 yearEnd,
			 d$action,
			 justification,
			 surveyID,
			 assigned_coverageBegin,
			 coverageEnd,
			 assigned_coverageEnd))
	cat('\n')
sink()
odbcCloseAll()
