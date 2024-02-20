# Collect explanations in natural language terms
#
# Prolog
# collect_explanations(C, V, Y, Explanations) :-
#     findall(Expl, explanation(C, V, Y, Expl), Explanations).

# explanation(C, V, Y, Expl) :-
#     survey_reason_to_exclude(C, V, Y, _, Expl).
# explanation(C, V, Y, Expl) :-
#     survey_modified(C, V, Y, _, Expl, _).

Expl = Svy.Excl # from 06_survey

# explanation(C, V, Y, Expl) :-
#     reported_reason_to_exclude(C, V, Y, _, Expl).

Expl[] = sprintf("%s%s", Expl, Rej.Info) # from 04_check

# explanation(C, V, Y, Expl) :-
#     decision(C, V, Y, comment, Expl, _, _).
#
# Multiple decisions for the same year * vaccine combination
index = Decisions[Decisions$Dec == "comment", ]
index = aggregate(list(Info=index$Info), list(Y=index$Y, V=index$V), FUN=paste, collapse="")
Expl[cbind(index$Y, index$V)] = 
  sprintf("%s%s", Expl[cbind(index$Y, index$V)], index$Info)

# explanation(C, V, Y, Expl) :-
#     decision(C, V, Y, acceptSurvey, Expl, _, _).

index = Decisions[Decisions$Dec == "acceptSurvey", ]
if(nrow(index))
  for(i in 1:nrow(index))
    Expl[index$Y[i], index$V[i]] =
      sprintf("%s%s", Expl[index$Y[i], index$V[i]], index$Info[i])

# explanation(C, V, Y, Expl) :-
#     decision(C, V, Y, acceptReported, Expl, _, _).

index = Decisions[Decisions$Dec == "acceptReported", ]
if(nrow(index))
  for(i in 1:nrow(index))
    Expl[index$Y[i], index$V[i]] =
      sprintf("%s%s", Expl[index$Y[i], index$V[i]], index$Info[i])

# explanation(C, V, Y, Expl) :-
#     decision(C, V, Y, ignoreGov, Expl, _, _).

index = Decisions[Decisions$Dec == "ignoreGov", ]
if(nrow(index))
  for(i in 1:nrow(index))
    Expl[index$Y[i], index$V[i]] =
      sprintf("%s%s", Expl[index$Y[i], index$V[i]], index$Info[i])

Text = YV.char()
Text[] = sprintf("%s %s %s %s", Info, Expl, Change, GoC.Expl)

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
