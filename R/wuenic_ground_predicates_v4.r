if(FALSE)
{
  library(Hmisc)

ccode = "bgd"
ccode = tolower(ccode)
CCODE = toupper(ccode)

mdb.tables <- function(fname)
  system2(c("mdb-tables", "-1", fname), stdout=TRUE)

mdb.get.table <- function(fname, table)
{
  s <- system2(c("mdb-schema", "-T", table, file), stdout=TRUE)
  start <- grep("^ \\($", s) + 1
  end <- grep("^\\);$", s) - 1
  s <- s[start:end]
  s <- strsplit(s, "\t")
  vnames <- sapply(s, function(x) {
      bracketed = x[2]
      if (substr(bracketed, 1, 1) == "[") 
        substr(bracketed, 2, nchar(bracketed) - 1)
      else bracketed
    })
  vnames <- makeNames(vnames, unique = TRUE, allow = NULL)
  types <- sapply(s, function(x) x[length(x)])
  datetime <- vnames[grep("DateTime", s)]
  f <- tempfile()
  system2(c("mdb-export", fname, table), stdout=f)
  csv.get(f, datetimevars=datetime, dateformat="%m/%d/%y")
}

fname = "countries/wuenic2024.mdb"
# tabs = mdb.tables(fname)
# tabs = tabs[tabs != "'data comparison$'_ImportErrors"]
# tabs = lapply(tabs, FUN=mdb.get.table, fname=fname)

t = mdb.get.table(fname, "COUNTRY")
rownames(t) = t$country
cname = t[t$country == CCODE, "countryName"]

# SELECT vaccine, annum, presentation, comment
#   FROM ESTIMATE_REQUIRED WHERE country = CCODE AND annum >= 1997

t = mdb.get.table(fname, "ESTIMATE_REQUIRED")
est_req = t[t$country == CCODE & t$annum >= 1997, 
    c("vaccine", "annum", "presentation", "comment")]

# SELECT annum, vaccine, coverage
#   FROM REPORTED_COVERAGE
#   WHERE country = CCODE AND annum >= 1997 AND coverage > 0 AND vaccine IN vaxs AND type = "admin"

vaxs = c('bcg', 'dtp1', 'dtp3', 'pol1', 'pol3', 'ipv1', 'mcv1', 'mcv2', 'rcv1',
    'hepbb', 'hepb1', 'hepb3', 'hib1', 'hib3', 'pcv1', 'pcv3', 'rotac', 'yfv')

t = mdb.get.table(fname, "REPORTED_COVERAGE")
t$vaccine = tolower(t$vaccine)
admin = t[t$country == CCODE & t$annum >= 1997 & t$coverage > 0 & t$vaccine %in% vaxs & t$type == "admin", 
    c("annum", "vaccine", "coverage")]

# SELECT annum, vaccine, coverage
#   FROM REPORTED_COVERAGE
#   WHERE country = CCODE AND annum >= 1997 AND coverage > 0 AND vaccine IN vaxs AND type = "official"

gov = t[t$country == CCODE & t$annum >= 1997 & t$coverage > 0 & t$vaccine %in% vaxs & t$type == "official", 
    c("annum", "vaccine", "coverage")]

# SELECT annum, vaccine, coverage
#   FROM ESTIMATED_COVERAGE
#   WHERE country = ccode AND annum >= 1997 AND coverage > 0 AND vaccine IN vaxs

t = mdb.get.table(fname, "ESTIMATED_COVERAGE")
t$vaccine = tolower(t$vaccine)
legacy = t[t$country == ccode & t$annum >= 1997 & t$coverage > 0 & t$vaccine %in% vaxs, 
    c("annum", "vaccine", "coverage")]

# SELECT vaccine, annum, reportedDenom, reportedNum, coverage
#   FROM REPORTED_NUMERATOR_DENOMINATOR
#   WHERE country = CCODE AND reportedNum > 0

t = mdb.get.table(fname, "REPORTED_NUMERATOR_DENOMINATOR_todeleate") # Name?
t$vaccine = tolower(t$vaccine)
vaccinated = t[t$country == CCODE & t$reportedNum > 0, 
    c("vaccine", "annum", "reportedNum")]
# check: a few lines with NA

target = t[t$country == CCODE & t$reportedNum > 0, 
    c("vaccine", "annum", "reportedDenom")]
# check: a few lines with NA

# SELECT annum, valor
#   FROM DEMOGRAPHIC
#   WHERE country = CCODE AND annum >= 1997 AND indicator = 'Births' AND valor > 0

t = mdb.get.table(fname, "DEMOGRAPHIC")
Births = t[t$country == CCODE & t$annum >= 1997 & t$Indicator == "Births" & t$Valor > 0,
               c("annum", "Valor")]

# SELECT annum, valor
#   FROM DEMOGRAPHIC
#   WHERE country = CCODE AND annum >= 1997 AND indicator = 'SI' AND valor > 0

SI = t[t$country == CCODE & t$annum >= 1997 & t$Indicator == "SI" & t$Valor > 0,
           c("annum", "Valor")]

# Coverage recalculated using number vaccinated / UNPD denominator
# Skipped that one since it isn't used in the WUENIC estimation.

# SELECT surveyId
#   FROM SURVEY_DESCRIPTION
#   WHERE ISO3 = CCODE

t = mdb.get.table(fname, "SURVEY_DESCRIPTION")
Ids = t[t$ISO3 == CCODE, ]

# SELECT *
#   FROM SURVEY_COVERAGE
#   WHERE surveyId IN Ids AND reanalyzed = "No" AND cohortYear >= 1997 AND validity = "crude" AND vaccine IN vaxs

t = mdb.get.table(fname, "SURVEY_COVERAGE")
t$vaccine = tolower(t$vaccine)
Svy = t[t$surveyId %in% Ids$surveyId & t$reanalyzed == 0 & t$cohortYear >= 1997 & t$validity == "crude" & t$vaccine %in% vaxs, ]

Survey = merge(Ids, Svy, by="surveyId")

# Some preprocessing - move to main program
#
# survey$coverage[is.na(survey$coverage)] <- 0
# survey$cardsSeen[is.na(survey$cardsSeen)] <- 0
# survey$denominator[is.na(survey$denominator)] <- 0
#
# survey$cardsSeen[!is.numeric(survey$cardsSeen)] <- 0
# survey$coverage[!is.numeric(survey$coverage)]   <- 0
#
# Cards    <- format(survey$cardsSeen,digits=1)
# Coverage <- format(survey$coverage,digits=1)
# Children <- as.character(survey$denominator)
# ... gsub("'","",survey$surveyNameEnglish),
# ... substring(survey$collectBegin,1,4),
# ... sub(' +$','',tolower(survey$evidence)),

survey_results = Survey[, c("vaccine", "cohortYear", "surveyId",
    "surveyNameEnglish", "surveyType", "collectBegin", "cardsSeen", "evidence",
    "ageInterview", "validity", "denominator", "coverage")]

# Legacy estimate for 1997 by working group decision
#
# SELECT vaccine, annum, coverage
#   FROM ESTIMATED_COVERAGE
#   WHERE country = CCODE AND annum = 1997 and vaccine IN vaxs

t = mdb.get.table(fname, "ESTIMATED_COVERAGE")
t$vaccine = tolower(t$vaccine)
leg1997 = t[t$country == ccode & t$annum == 1997 & t$vaccine %in% vaxs, 
           c("vaccine", "coverage")]

# Preprocessing
#
# vaccine[is.na(vaccine)] <- 'na' # Does not make sense here
# cov_formatted <- sprintf("%.0f",d$coverage)

wgd1997 = data.frame(vaccine=leg1997$vaccine, yearBegin=1997, yearEnd=1997,
  Dec="assignAnchor", Expl='Legacy estimate.', 
  coverageBegin=NA, assigned_coverageBegin=leg1997$coverage,
  coverageEnd=NA, assigned_coverageEnd=NA)

# SELECT *
#   FROM WGD
#   WHERE country = CCODE

t = mdb.get.table(fname, "WGD")
t$vaccine = tolower(t$vaccine)
wgd0 = t[t$country == CCODE, 
    c("vaccine", "identifyCoverage", "assignCoverage", "comment",
      "action",
      "yearBegin", "yearEnd")]

# Preprocessing: apply to all vaccines
# vaccine[vaccine == 'all'] <- 'X'
# surveyID <- tolower(as.character(d$identifyCoverage))
# surveyID[is.na(surveyID)] <- 'na'
# yearBegin <- d$yearBegin
# yearBegin[is.na(yearBegin)] <- 1749   # Edward Jenner's year of birth
# 
# surveyID <- tolower(as.character(d$identifyCoverage))
# surveyID[is.na(surveyID)] <- 'na'
# 
# assigned_coverageBegin <- as.numeric(d$assignCoverage)
# assigned_coverageBegin[is.na(assigned_coverageBegin)] <- 'na'
# 
# yearEnd <- d$yearEnd
# yearEnd[is.na(yearEnd)] <- 2203 # project maximun existance of WHO. Gott's principle, 2011 - 1948 (50%CI)

wgd = data.frame(vaccine=wgd0$vaccine,
    yearBegin=wgd0$yearBegin, yearEnd=wgd0$yearEnd, action=wgd0$action,
    comment=wgd0$comment, surveyId=wgd0$identifyCoverage,
    assigned_coverageBegin=wgd0$assignCoverage,
    coverageEnd=NA,
    assigned_coverageEnd=NA)
}