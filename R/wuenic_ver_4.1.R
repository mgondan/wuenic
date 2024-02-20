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
    Comment=Text[VY])

dir.create("out", showWarnings = FALSE)
write.table(Table, sprintf("out/%s.pl.R41.txt", Code), quote=FALSE, row.names=FALSE, sep="\t", na="")
