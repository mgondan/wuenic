# Collect explanations in natural language terms
#
# Prolog
# collect_explanations(C, V, Y, Explanations) :-
#     findall(Expl, explanation(C, V, Y, Expl), Explanations).

# explanation(C, V, Y, Expl) :-
#     survey_reason_to_exclude(C, V, Y, _, Expl).
# explanation(C, V, Y, Expl) :-
#     survey_modified(C, V, Y, _, Expl, _).

Expl = Svy.Excl

# explanation(C, V, Y, Expl) :-
#     reported_reason_to_exclude(C, V, Y, _, Expl).
#
# % Reasons to exclude reported data are:
# %  1. Working group decision.
# %  2. Coverage > 100%
# %  3. Inconsistent temporal changes (sawtooth or sudden change most
# %     recent year)
# reported_reason_to_exclude(C, V, Y, Reason, Expl) :-
#     reported(C, V, Y, _, _),
#     decision(C, V, Y, ignoreReported, Expl0, _, _),
#     Reason = wdg,
#     concat_atom(['Reported data excluded. ', Expl0], Expl).

ignore = Decisions[Decisions$Dec == "ignoreReported", ]
if(nrow(ignore))
  for(i in 1:nrow(ignore))
  {
    if(!is.na(Rep.Cov[ignore$Y[i], ignore$V[i]]))
      Expl[ignore$Y[i], ignore$V[i]] = sprintf("%sReported data excluded. %s",
        Expl[ignore$Y[i], ignore$V[i]], ignore$Info[i])
  }


# reported_reason_to_exclude(C, V, Y, Reason, Expl) :-
#     reported(C, V, Y, _, Coverage),
#     not(decision(C, V, Y, acceptReported, _, _, _)),
#     Coverage > 100,
#     Reason = 100,
#     concat_atom(['Reported data excluded because ', Coverage,
#     ' percent greater than 100 percent. '], Expl).

# index = Rep.Cov > 100
# accept = Decisions[Decisions$Dec == "acceptReported", ]
# index[accept$Y, accept$V] = FALSE
# Expl[which(index)] = sprintf(
#     "%sReported data excluded because %i percent greater than 100 percent. ", 
#     Expl[which(index)], Rep.Cov[which(index)])

# reported_reason_to_exclude(C, V, Y, _, Expl) :-
#     reported(C, V, Y, _, Coverage),
#     not(decision(C, V, Y, acceptReported, _, _, _)),
#     Prec is Y - 1,
#     Succ is Y + 1,
#     reported(C, V, Prec, _, CoveragePrec),
#     reported(C, V, Succ, _, CoverageSucc),
#     sawtooth_threshold(Threshold),
#     Coverage - CoveragePrec > Threshold,
#     Coverage - CoverageSucc > Threshold,
#     concat_atom(
#       [ 'Reported data excluded due to an increase from ', CoveragePrec,
#         ' percent to ', Coverage, ' percent with decrease ', CoverageSucc,
#         ' percent. '
#       ], Expl).
#
# reported_reason_to_exclude(C, V, Y, _, Expl) :-
#     reported(C, V, Y, _, Coverage),
#     not(decision(C, V, Y, acceptReported, _, _, _)),
#     Prec is Y - 1,
#     Succ is Y + 1,
#     reported(C, V, Prec, _, CoveragePrec),
#     reported(C, V, Succ, _, CoverageSucc),
#     sawtooth_threshold(Threshold),
#     CoveragePrec - Coverage > Threshold,
#     CoverageSucc - Coverage > Threshold,
#     concat_atom(
#       [ 'Reported data excluded due to decline in reported coverage from ',
#         CoveragePrec, ' percent to ', Coverage, ' percent with increase to ',
#         CoverageSucc,' percent. '], Expl).

index = which(Rej.Info != "")
Expl[index] = sprintf("%s%s", Expl[index], Rej.Info[index])

# explanation(C, V, Y, Expl) :-
#     decision(C, V, Y, comment, Expl, _, _).

index = Decisions[Decisions$Dec == "comment", ]
if(nrow(index))
  for(i in 1:nrow(index))
    Expl[index$Y[i], index$V[i]] =
      sprintf("%s%s", Expl[index$Y[i], index$V[i]], index$Info[i])

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
