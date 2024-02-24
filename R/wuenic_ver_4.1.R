wuenic.estimate = function(fname="data.pl", outname="wuenic.out")
{
  # 02_load
  s = wuenic.load(fname)
  Country=s$Country
  Code=s$Code
  Date=s$Date
  Ereq=s$Ereq
  Rub=s$Rub
  firstRubellaAtSecondMCV=s$firstRubellaAtSecondMCV
  Legacy=s$Legacy
  Gov=s$Gov
  Admin=s$Admin
  Vaccinated=s$Vaccinated
  Target=s$Target
  Surviving=s$Surviving
  Births=s$Births
  Survey=s$Survey
  Decisions=s$Decisions

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
    Country=Country,
    ProductionDate=Date,
    ISOCountryCode=Code,
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
  fname = sprintf("xsb/%s.pl", ccode)
  outname = sprintf("out/%s.R41.txt", ccode)
  wuenic::wuenic.estimate(fname, outname)
}

