#' Load country name
#' 
#' @param mdb
#' path to database
#' 
#' @param ccode
#' Country code, e.g. afg
#' 
#' @return
#' Country name
#' 
country = function(mdb, ccode)
{
  t = mdb_get(mdb=mdb, tab="COUNTRY")
  t[t$country == toupper(ccode), "countryName"]
}

#' Check list of required estimates
#' 
#' @param mdb
#' path to database
#' 
#' @param ccode
#' Country code, e.g. afg
#' 
#' @return
#' Year by Vaccine table
#' 
#' @details
#' SELECT vaccine, annum
#'   FROM ESTIMATE_REQUIRED WHERE country = CCODE AND annum >= 1997
#
est_req = function(mdb, ccode)
{
  t = mdb_get(mdb=mdb, tab="ESTIMATE_REQUIRED")
  t = t[t$country == toupper(ccode) & t$annum >= 1997, c("vaccine", "annum")]
  t$vaccine = tolower(t$vaccine)
  
  r = YV.bool()
  unknown = which(!(unique(t$vaccine) %in% colnames(r)))
  if(length(unknown))
    warning("Unknown vaccine: %s", unknown)
  
  r[cbind(t$annum, t$vaccine)] = TRUE
  return(r)
}

wuenic.rub = function(mdb="countries/wuenic2023.mdb", ccode="ago")
{
  t = mdb_get(mdb=mdb, tab="ESTIMATE_REQUIRED")
  t = t[t$country == toupper(ccode) & t$annum >= 1997, c("vaccine", "annum", "comment")]
  t$vaccine = tolower(t$vaccine)
  t$comment = trimws(tolower(t$comment))
  
  r = YV.char()
  r[cbind(t$annum, t$vaccine)] = t$comment
  return(r)
}

# SELECT annum, vaccine, coverage
#   FROM REPORTED_COVERAGE
#   WHERE country = CCODE AND annum >= 1997 AND coverage > 0 AND vaccine IN vaxs AND type = "admin"

wuenic.admin = function(mdb="countries/wuenic2023.mdb", ccode="ssd")
{
  vaxs = c('bcg', 'dtp1', 'dtp3', 'pol1', 'pol3', 'ipv1', 'mcv1', 'mcv2', 'rcv1',
    'hepbb', 'hepb1', 'hepb3', 'hib1', 'hib3', 'pcv1', 'pcv3', 'rotac', 'yfv')
  t = mdb_get(mdb=mdb, tab="REPORTED_COVERAGE")
  t$country = toupper(t$country)
  t$vaccine = tolower(t$vaccine)
  t = t[t$country == toupper(ccode) & t$annum >= 1997 & t$coverage > 0 &
          t$vaccine %in% vaxs & t$type == "admin", 
    c("annum", "vaccine", "coverage")]
  t = t[complete.cases(t), ]
  
  r = YV.int()
  r[cbind(t$annum, t$vaccine)] = round(t$coverage)
  return(r)
}

wuenic.gov = function(mdb="countries/wuenic2023.mdb", ccode="can")
{
  vaxs = c('bcg', 'dtp1', 'dtp3', 'pol1', 'pol3', 'ipv1', 'mcv1', 'mcv2', 'rcv1',
           'hepbb', 'hepb1', 'hepb3', 'hib1', 'hib3', 'pcv1', 'pcv3', 'rotac', 'yfv')
  t = mdb_get(mdb=mdb, tab="REPORTED_COVERAGE")
  t$country = toupper(t$country)
  t$vaccine = tolower(t$vaccine)
  t = t[t$country == toupper(ccode) & t$annum >= 1997 & t$vaccine %in% vaxs &
    t$coverage > 0 & t$type == "official", c("annum", "vaccine", "coverage")]
  t = t[complete.cases(t), ]
  
  r = YV.int()
  r[cbind(t$annum, t$vaccine)] = round(t$coverage)
  return(r)
}

# SELECT annum, vaccine, coverage
#   FROM ESTIMATED_COVERAGE
#   WHERE country = ccode AND annum >= 1997 AND coverage > 0 AND vaccine IN vaxs

wuenic.leg = function(mdb="countries/wuenic2023.mdb", ccode="bgd")
{
  vaxs = c('bcg', 'dtp1', 'dtp3', 'pol1', 'pol3', 'ipv1', 'mcv1', 'mcv2', 'rcv1',
           'hepbb', 'hepb1', 'hepb3', 'hib1', 'hib3', 'pcv1', 'pcv3', 'rotac', 'yfv')
  t = mdb_get(mdb=mdb, tab="ESTIMATED_COVERAGE")
  t$vaccine = tolower(t$vaccine)
  t = t[t$country == ccode & t$annum >= 1997 & t$vaccine %in% vaxs &
          t$coverage > 0, c("annum", "vaccine", "coverage")]
  
  r = YV.int()
  r[cbind(t$annum, t$vaccine)] = t$coverage
  return(r)
}

# SELECT vaccine, annum, reportedDenom, reportedNum, coverage
#   FROM REPORTED_NUMERATOR_DENOMINATOR
#   WHERE country = CCODE AND reportedNum > 0
wuenic.vaccinated = function(mdb="countries/wuenic2023.mdb", ccode="nga")
{
  t = mdb_get(mdb=mdb, tab="REPORTED_NUMERATOR_DENOMINATOR")
  t$vaccine = tolower(t$vaccine)
  t = t[t$country == toupper(ccode) & t$reportedNum > 0, 
        c("vaccine", "annum", "reportedNum")]

  # check: a few lines with NA
  t = t[complete.cases(t), ]
  
  # check: unknown vaccines
  t = t[t$vaccine %in% Vn(), ]
  t = t[t$annum %in% Yn(), ]
  
  r = YV.int()
  r[cbind(t$annum, t$vaccine)] = tround(t$reportedNum)
  return(r)
}

# SELECT vaccine, annum, reportedDenom, reportedNum, coverage
#   FROM REPORTED_NUMERATOR_DENOMINATOR
#   WHERE country = CCODE AND reportedNum > 0
wuenic.target = function(mdb="countries/wuenic2023.mdb", ccode="bgd")
{
  t = mdb_get(mdb=mdb, tab="REPORTED_NUMERATOR_DENOMINATOR")
  t$vaccine = tolower(t$vaccine)
  t = t[t$country == toupper(ccode) & t$reportedDenom > 0, 
        c("vaccine", "annum", "reportedDenom")]
  
  # check: a few lines with NA
  t = t[complete.cases(t), ]
  
  # check: unknown vaccines
  t = t[t$vaccine %in% Vn(), ]
  t = t[t$annum %in% Yn(), ]
  
  r = YV.int()
  r[cbind(t$annum, t$vaccine)] = tround(t$reportedDenom)
  return(r)
}

# SELECT annum, valor
#   FROM DEMOGRAPHIC
#   WHERE country = CCODE AND annum >= 1997 AND indicator = 'Births' AND valor > 0
wuenic.births = function(mdb="countries/wuenic2023.mdb", ccode="bgd")
{
  t = mdb_get(mdb=mdb, tab="DEMOGRAPHIC")
  t = t[t$country == toupper(ccode) & t$annum >= 1997 & t$Indicator == "Births" & t$Valor > 0,
             c("annum", "Valor")]

  r = rep(NA, length(Yn()))
  names(r) = Yn()
  r[as.character(t$annum)] = t$Valor
  r = r[as.character(Yn())] # drop years outside range
  return(r)
}

# SELECT annum, valor
#   FROM DEMOGRAPHIC
#   WHERE country = CCODE AND annum >= 1997 AND indicator = 'SI' AND valor > 0

wuenic.si = function(mdb="countries/wuenic2023.mdb", ccode="bgd")
{
  t = mdb_get(mdb=mdb, tab="DEMOGRAPHIC")
  t = t[t$country == toupper(ccode) & t$annum >= 1997 & t$Indicator == "SI" & t$Valor > 0,
        c("annum", "Valor")]
  
  r = rep(NA, length(Yn()))
  names(r) = Yn()
  r[as.character(t$annum)] = t$Valor
  r = r[as.character(Yn())] # drop years outside range
  return(r)
}

wuenic.svy = function(mdb="countries/wuenic2023.mdb", ccode="mdg")
{
  # SELECT surveyId
  #   FROM SURVEY_DESCRIPTION
  #   WHERE ISO3 = CCODE
  t = mdb_get(mdb, "SURVEY_DESCRIPTION")
  Ids = t[t$ISO3 == toupper(ccode), ]

  # SELECT *
  #   FROM SURVEY_COVERAGE
  #   WHERE surveyId IN Ids AND reanalyzed = "No" AND cohortYear >= 1997 AND validity = "crude" AND vaccine IN vaxs
  
  vaxs = c('bcg', 'dtp1', 'dtp3', 'pol1', 'pol3', 'ipv1', 'mcv1', 'mcv2', 'rcv1',
           'hepbb', 'hepb1', 'hepb3', 'hib1', 'hib3', 'pcv1', 'pcv3', 'rotac', 'yfv')
  t = mdb_get(mdb, "SURVEY_COVERAGE")
  t$vaccine = tolower(t$vaccine)
  Svy = t[t$surveyId %in% Ids$surveyId & t$reanalyzed == 0 & t$cohortYear >= 1997
          & tolower(t$validity) == "crude" & t$vaccine %in% vaxs, ]

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
  
  Survey$denominator[is.na(Survey$denominator)] = 0
  Survey$surveyNameEnglish[Survey$surveyNameEnglish == ""] = "NA"
  
  data.frame(V=Survey$vaccine, Y=Survey$cohortYear,
    Yn=as.character(Survey$cohortYear), Id=tolower(Survey$surveyId), 
    Info.title=gsub("'", "", Survey$surveyNameEnglish),
    Info.type=Survey$surveyType,
    Info.yrcoll=Survey$collectBegin, Info.cr=Survey$cardsSeen,
    Info.confirm=trimws(tolower(Survey$evidence)),
    Info.age=Survey$ageInterview, Info.val=Survey$validity,
    Info.ss=Survey$denominator, Cov=as.numeric(format(Survey$coverage, digits=1)))
}

wuenic.dec = function(mdb="countries/wuenic2023.mdb", ccode="syr", Survey)
{
  vaxs = c('bcg','dtp1','dtp3','pol3','ipv1','mcv1','mcv2','rcv1','hepbb','hepb3','hib3','pcv3','rotac')
  # Legacy estimate for 1997 by working group decision
  #
  # SELECT vaccine, annum, coverage
  #   FROM ESTIMATED_COVERAGE
  #   WHERE country = CCODE AND annum = 1997 and vaccine IN vaxs
  t = mdb_get(mdb, "ESTIMATED_COVERAGE")
  t$vaccine = tolower(t$vaccine)
  leg1997 = t[t$country == ccode & t$annum == 1997 & t$vaccine %in% vaxs, 
              c("vaccine", "coverage")]

  # Preprocessing
  #
  # vaccine[is.na(vaccine)] <- 'na' # Does not make sense here
  # cov_formatted <- sprintf("%.0f",d$coverage)
  wgd1997 = data.frame(V=leg1997$vaccine, 
    Y0=rep(1997, nrow(leg1997)), Y1=rep(1997, nrow(leg1997)),
    Dec=rep("assignAnchor", nrow(leg1997)),
    Info=rep("Legacy estimate.", nrow(leg1997)), 
    Id=rep(NA, nrow(leg1997)), 
    Cov=round(leg1997$coverage))

  # SELECT *
  #   FROM WGD
  #   WHERE country = CCODE
  t = mdb_get(mdb, "WGD")
  t$vaccine = tolower(t$vaccine)
  t = t[t$country == toupper(ccode), 
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
  wgd1 = data.frame(V=t$vaccine, Y0=t$yearBegin, Y1=t$yearEnd, Dec=t$action,
    Info=t$comment, Id=tolower(t$identifyCoverage), Cov=t$assignCoverage)
  
  s = rbind(wgd1997, wgd1)
  
  # Fix strange year ranges
  s$Y0[is.na(s$Y0)] = min(Yn())
  s$Y1[is.na(s$Y1)] = max(Yn())
  s$Y0 = pmax(s$Y0, min(Yn()))
  s$Y1 = pmin(s$Y1, max(Yn()))
  
  # V = "all" means that a decision applies to all vaccines
  V.na = function(d)
  {
    if(d['V'] == "all")
      return(data.frame(V=Vn(), Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'], 
                        Id=d['Id'], Info=d['Info'], Cov=d['Cov'], row.names=NULL))
    
    data.frame(V=d['V'], Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'],
               Id=d['Id'], Info=d['Info'], Cov=d['Cov'])
  }
  
  s = apply(s, MARGIN=1, simplify=FALSE, FUN=V.na)
  s = do.call("rbind", s)
  
  # If a year range is given, apply decision to each included year
  Y.range = function(d)
  {
    data.frame(V=d['V'], Y=d['Y0']:min(max(Yn()), d['Y1']),
               Dec=d['Dec'], Id=d['Id'], Info=d['Info'], Cov=d['Cov'], row.names=NULL)
  }
  
  s = apply(s, MARGIN=1, simplify=FALSE, FUN=Y.range)
  s = do.call("rbind", s)
  
  # For "ignore survey", the Id is sometimes NA, meaning that it applies to all
  # surveys of a given year and vaccine
  Id.na = function(d, Idn)
  {
    if(d['Dec'] == "ignoreSurvey" & d['Id'] == "")
      return(data.frame(V=d['V'], Y=d['Y'], Dec=d['Dec'], 
                        Id=Idn, Info=d['Info'], Cov=d['Cov'], row.names=NULL))
    
    data.frame(V=d['V'], Y=d['Y'], Dec=d['Dec'],
               Id=d['Id'], Info=d['Info'], Cov=d['Cov'])
  }
  
  s = apply(s, MARGIN=1, simplify=FALSE, FUN=Id.na, Idn=unique(Survey$Id))
  s = do.call("rbind", s)
  
  # For "accept survey", the Id is sometimes NA, meaning that it applies to all
  # surveys of a given year and vaccine
  Id.na = function(d)
  {
    if(d['Dec'] == "acceptSurvey" & d['Id'] == "")
      return(data.frame(V=d['V'], Y=d['Y'], Dec=d['Dec'],
               Id=unique(Survey$Id[Survey$Y == d['Y']]),
               Info=d['Info'], Cov=d['Cov'], row.names=NULL))
    
    data.frame(V=d['V'], Y=d['Y'], Dec=d['Dec'],
      Id=d['Id'], Info=d['Info'], Cov=d['Cov'])
  }
  
  s = apply(s, MARGIN=1, simplify=FALSE, FUN=Id.na)
  s = do.call("rbind", s)
  
  if(!all(s$Id[s$Dec == "acceptSurvey"] %in% unique(Survey$Id)))
  {
    warning("Jamaica? Wrong survey Ids in wgd = acceptSurvey")
    # Please remove this when the database has been corrected
    if(ccode == "jam")
    {
      s$Cov[s$Dec == "acceptSurvey"] = s$Id[s$Dec == "acceptSurvey"]
      s$Id[s$Dec == "acceptSurvey"] = "jam2005231"
    }
  }
  
  s$Cov = as.integer(s$Cov)
  s$Y = as.character(s$Y)
  return(s)
}

# Read date from pl-file
#
wuenic.load = function(fname = "data.pl")
{
  # load_files("data.pl", [encoding(iso_latin_1)])
  rolog::once(call("load_files", fname,
    list(call("encoding", quote(iso_latin_1)))))
  
  # date(Date)
  s = rolog::once(call("date", expression(Date)))
  Date = iconv(s$Date, from="latin1", to="latin1")
  
  list(Date=Date)
}
