wuenic.estimate = function(ccode="bdi", fname="countries/bdi.pl", outname="wuenic.txt")
{
  # 02_load
  s = wuenic.load(fname)
  Code=s$Code
  if(Code != ccode)
    stop("Wrong country code/data file: ", ccode)
  
  Date1=s$Date
  firstRubellaAtSecondMCV=s$firstRubellaAtSecondMCV
  Legacy1=s$Legacy
  Gov1=s$Gov
  Admin1=s$Admin
  Vaccinated1=s$Vaccinated
  Target1=s$Target
  Surviving1=s$Surviving
  Births1=s$Births
  Survey1=s$Survey
  Decisions1=s$Decisions
  
  # 01_mdb
  Date = file.info("countries/wuenic2023.mdb")$mtime

  loc = Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  Date = format(Date, "%a %b %d %H:%M:%S %Y")
  Sys.setlocale("LC_TIME", loc)
  Date = Date1 # Hack to obtain the same date as in the pl-file

  Country = wuenic.country(ccode=ccode)
  Ereq = wuenic.est_req(ccode=ccode)
  Rub = wuenic.rub(ccode=ccode)
  Admin = wuenic.admin(ccode=ccode)
  Gov = wuenic.gov(ccode=ccode)
  Legacy = wuenic.leg(ccode=ccode)
  Vaccinated = wuenic.vaccinated(ccode=ccode)
  Target = wuenic.target(ccode=ccode)
  Births = wuenic.births(ccode=ccode)
  Surviving = wuenic.si(ccode=ccode)
  Survey = wuenic.svy(ccode=ccode)
  Decisions = wuenic.dec(ccode=ccode, Survey=Survey)

  # 03_rep
  s = wuenic.reported(Admin, Gov, Decisions)
  Rep.Cov = s$Cov
  Rep.Src = s$Src
  Rep.Expl = s$Expl

  # 04_check
  s = wuenic.check(Rep.Cov, Decisions)
  reject = s$Reject
  Rej.Info = s$Info

  # 05_ts
  s = wuenic.ts(Ereq, Rep.Cov, Rep.Src, reject)
  TS.Cov = s$Cov
  TS.Src = s$Src
  
  # 06_survey
  s = wuenic.survey(Survey, Decisions)
  Svy.Cov = s$Cov
  Svy.Expl = s$Expl
  Svy.Expl.Acc = s$Expl.Acc
  Svy.Excl = s$Excl
  
  # 07_anchor
  s = wuenic.anchor(TS.Cov, TS.Src, Svy.Cov, Svy.Expl, Decisions)
  Anchor.Cov = s$Cov
  Anchor.Rule = s$Rule
  Anchor.Info = s$Info
  
  # 08_calibrate
  s = wuenic.calibrate(TS.Cov, TS.Src, Anchor.Cov, Anchor.Rule, Anchor.Info, Decisions)
  Cov = s$Cov
  Rule = s$Rule
  Info = s$Info
  
  # 09_toplevel
  s = wuenic.top(Ereq, Rule, Info, Cov, Rub, firstRubellaAtSecondMCV, Decisions)
  Cov = s$Cov
  Bounded = s$Bounded
  Rule = s$Rule
  Info = s$Info
  
  # 10_goc
  s = wuenic.goc(Ereq, Rep.Cov, Svy.Cov, Cov, Rule, Rub, Decisions,
                 Vaccinated, Births, Surviving)
  GoC = s$GoC
  GoC.Expl = s$Expl
  
  # 11_legacy
  s = wuenic.legacy(Bounded, Legacy)
  Change = s$Change
  
  # 12_explanations
  s = wuenic.explanations(Rep.Expl, Svy.Excl, Svy.Expl.Acc, Rej.Info,
                          Decisions, Info, Change, GoC.Expl)
  Expl = s$Expl
  
  Vaccine = Vn()
  Year = Yn()
  VY = expand.grid(Year, Vaccine, stringsAsFactors=FALSE)
  
  include = Ereq & !is.na(Bounded)
  VY = cbind(Y=VY$Var1[include], V=VY$Var2[include])
  
  Table = data.frame(
    Country=rep(Country, nrow(VY)),
    ProductionDate=rep(Date, nrow(VY)),
    ISOCountryCode=rep(Code, nrow(VY)),
    Vaccine=VY[, "V"],
    Year=VY[, "Y"],
    WUENIC=Bounded[VY],
    WUENICPreviousRevision=Legacy[VY],
    GradeOfConfidence=GoC[VY],
    AdministrativeCoverage=Admin[VY],
    GovernmentEstimate=Gov[VY],
    ReportedCoverage=Rep.Cov[VY],
    ChildrenVaccinated=Vaccinated[VY],
    ChildrenInTarget=Target[VY],
    BirthsUNPD=Births[VY[, "Y"]],
    SurvivingInfantsUNPD=Surviving[VY[, "Y"]],
    ReportedTimeSeries=TS.Cov[VY],
    ReportedTimeSeriesSource=TS.Src[VY],
    SurveyInformation=Svy.Cov[VY],
    Rule=Rule[VY],
    Comment=Expl[VY])
  
  write.table(Table, outname, quote=FALSE, row.names=FALSE, sep="\t", na="")
}

# Test
args = commandArgs(trailingOnly=TRUE)
if(length(args))
{
  library(rolog)
  ccode = tools::file_path_sans_ext(args[1])
  fname = sprintf("countries/%s.pl", ccode)
  outname = sprintf("out/%s.txt", ccode)
  wuenic::wuenic.estimate(ccode, fname, outname)
}
