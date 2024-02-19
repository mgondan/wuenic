# % Confidence in reported coverage if it is an anchor point. Otherwise,
# % no confidence
# conf_reported(C, V, Y, Support) :-
#   reported(C, V, Y, _, _),
#   wuenic_I(C, V, Y, Rule, _, _),
#   !,
#   (   member(Rule, ['R:', 'R: AP'])
#   ->  Support = 'R+'
#   ;   Support = 'R-'
#   ).

Conf.Rep = YV.char()
index = !is.na(Rep.Cov) & Rule %in% c("R:", "R: AP")
Conf.Rep[index] = "R+"
index = !is.na(Rep.Cov) & !(Rule %in% c("R:", "R: AP"))
Conf.Rep[index] = "R-"

# % No confidence in surveys if _any_ survey deviates too much from WUENIC
# % coverage
# conf_survey(C, V, Y, Support) :-
#   wuenic_I(C, V, Y, _, _, Cov0),
#   estimate_required(C, V, Year, _, _),
#   survey(C, V, Year, _, Coverage),
#   confidence_survey_scope(Scope),
#   abs(Y - Year) =< Scope,
#   confidence_survey_threshold(Threshold),
#   abs(Cov0 - Coverage) > Threshold,
#   !,
#   Support = 'S-'.
#
# % Confidence only if all surveys are consistent with WUENIC coverage
# conf_survey(C, V, Y, Support) :-
#   wuenic_I(C, V, Y, _, _, _Cov0),
#   estimate_required(C, V, Year, _, _),
#   survey(C, V, Year, _, _Coverage),
#   confidence_survey_scope(Scope),
#   abs(Y - Year) =< Scope,
#   !,
#   Support = 'S+'.

# MG, discuss: It seems as if surveys are ignored if there is no estimate
# required. Unclear if intended, since the scope of surveys spans +/- 2 years.
# Example: arm/pcv3 2015 (Svy.Min from 2013)
index = which(!Ereq, arr.ind=TRUE)

Svy.Min = Svy.Cov
Svy.Min[cbind(index[, "Y"], index[, "V"])] = NA # here
Svy.Min = suppressWarnings({rollapply(Svy.Min, width=1 + 2*svy.scope(), partial=1, FUN=min, na.rm=TRUE)})
rownames(Svy.Min) = rownames(Svy.Cov)

Svy.Max = Svy.Cov
Svy.Max[cbind(index[, "Y"], index[, "V"])] = NA # here
Svy.Max = suppressWarnings({rollapply(Svy.Max, width=1 + 2*svy.scope(), partial=1, FUN=max, na.rm=TRUE)})
rownames(Svy.Max) = rownames(Svy.Cov)

Conf.Svy = YV.char()
index = which(!is.na(Cov) & Ereq & is.finite(Svy.Min) & is.finite(Svy.Max))
Conf.Svy[index] = "S+"

index = which(!is.na(Cov) & Ereq & Cov - Svy.Min > svy.thrs())
Conf.Svy[index] = "S-"

index = which(!is.na(Cov) & Ereq & Svy.Max - Cov > svy.thrs())
Conf.Svy[index] = "S-"

# % Births used for bcg and hepb birth dose
# denominator(C, V, Y, Coverage) :-
#   member(V, [bcg, hepbb]),
#   !,
#   vaccinated0(C, V, Y, Vaccinated),
#   births_UNPD(C, Y, Births),
#   Coverage is Vaccinated / Births * 100.
#
# % Surviving infants for remaining vaccines
# denominator(C, V, Y, Coverage) :-
#   vaccinated0(C, V, Y, Vaccinated),
#   si_UNPD(C, Y, SI),
#   Coverage is Vaccinated / SI * 100.

Den = YV.real()
index = c("bcg", "hepbb")
Den[, index] = Vaccinated[, index] / Births * 100

index = setdiff(Vn(), c("bcg", "hepbb"))
Den[, index] = Vaccinated[, index] / Surviving * 100

# % Recalculate coverage using reported number of children vaccinated and
# % births and surviving infants from UNPD estimates.
# conf_denominator(C, V, Y, Support) :-
#   vaccinated0(C, V, Y, _),
#   births_UNPD(C, Y, _),
#   si_UNPD(C, Y, _),
#   wuenic_I(C, V, Y, _Rule, _Expl, Cov0),
#   denominator(C, V, Y, Coverage),
#   !,
#   confidence_UNPD_threshold(Threshold),
#   (   abs(Coverage - Cov0) < Threshold % MG: < inconsistent with conf_survey
#   ->  Support = 'D+'
#   ;   Support = 'D-'
#   ).

Conf.Den = YV.char()
index = which(abs(Cov - Den) < unpd.thrs())
Conf.Den[index] = "D+"
index = which(abs(Cov - Den) >= unpd.thrs())
Conf.Den[index] = "D-"

# 10. Confidence depends on converging evidence from the different sources.
# % 1 star = low confidence, ..., 3 stars = high confidence
# %
# % Low confidence
# confidence(_C, _V, _Y, Expl, Grade) :-
#   !,
#   Expl = 'GoC=No accepted empirical data',
#   Grade = 1.

GoC = YV.int()
GoC.Expl = YV.char()
GoC[] = 1
GoC.Expl[] = "GoC=No accepted empirical data"

# % Confidence in one or two sources, two stars
# confidence(C, V, Y, Expl, Grade) :-
#   conf_reported(C, V, Y, 'R+'),
#   conf_survey(C, V, Y, 'S+'),
#   !,
#   Expl = 'GoC=R+ S+',
#   Grade = 2.
#
# confidence(C, V, Y, Expl, Grade) :-
#   conf_survey(C, V, Y, 'S+'),
#   conf_denominator(C, V, Y, 'D+'),
#   !,
#   Expl = 'GoC=S+ D+',
#   Grade = 2.
#
# confidence(C, V, Y, Expl, Grade) :-
#   conf_reported(C, V, Y, 'R+'),
#   conf_denominator(C, V, Y, 'D+'),
#   !,
#   Expl = 'GoC=R+ D+',
#   Grade = 2.
#
# confidence(C, V, Y, Expl, Grade) :-
#   conf_reported(C, V, Y, 'R+'),
#   !,
#   Expl = 'GoC=R+',
#   Grade = 2.
#
# confidence(C, V, Y, Expl, Grade) :-
#   conf_survey(C, V, Y, 'S+'),
#   !,
#   Expl = 'GoC=S+',
#   Grade = 2.
#
# confidence(C, V, Y, Expl, Grade) :-
#   conf_denominator(C, V, Y, 'D+'),
#   !,
#   Expl = 'GoC=D+',
#   Grade = 2.

index = which(Conf.Rep == "R+")
GoC[index] = 2
GoC.Expl[index] = "GoC=R+"

index = which(Conf.Svy == "S+")
GoC[index] = 2
GoC.Expl[index] = "GoC=S+"

index = which(Conf.Den == "D+")
GoC[index] = 2
GoC.Expl[index] = "GoC=D+"

index = which(Conf.Rep == "R+" & Conf.Svy == "S+")
GoC[index] = 2
GoC.Expl[index] = "GoC=R+ S+"

index = which(Conf.Svy == "S+" & Conf.Den == "D+")
GoC[index] = 2
GoC.Expl[index] = "GoC=S+ D+"

index = which(Conf.Rep == "R+" & Conf.Den == "D+")
GoC[index] = 2
GoC.Expl[index] = "GoC=R+ D+"

# % Check if any source is challenged
# challenge(C, V, Y, 'R-') :-
#   conf_reported(C, V, Y, 'R-').
#
# challenge(C, V, Y, 'S-') :-
#   conf_survey(C, V, Y, 'S-').
#
# challenge(C, V, Y, 'D-') :-
#   conf_denominator(C, V, Y, 'D-').
#
# % Todo list from V3
# %
# % 1. Simplify previous rule to a check for an anchor point
# %
# % supporting_survey_in_scope(C, V, Y, Rule) :-
# %     survey(C, V, Y, _, _),
# %     wuenic_I(C, V, Y, 'S: AP', _, _).
# %
# % 2. Rewrite rule to look at relationship between estimate rule and
# %    surveys in scope rule. For example, take randomness into account,
# %    e.g. probability for inconsistent results increases with the number of
# %    surveys and decreases with the sample size of the surveys.

Chall = YV.char()
Chall[] = ""

index = which(Conf.Den == "D-")
Chall[index] = "D-"

index = which(Conf.Rep == "R-")
Chall[index] = sprintf("%sR-", Chall[index])

index = which(Conf.Svy == "S-")
Chall[index] = sprintf("%sS-", Chall[index])

# % If any estimate has been challenged, confidence is low
# confidence(C, V, Y, Expl, Grade) :-
#   setof(Expl0, challenge(C, V, Y, Expl0), List),
#   !,
#   concat_atom(['Estimate challenged by: ' | List], Expl),
#   Grade = 1.

index = Chall != ""
GoC[index] = 1
GoC.Expl[index] = sprintf("Estimate challenged by: %s", Chall[index])

# % Confidence in both reported, surveys, and sold vaccines
# confidence(C, V, Y, Expl, Grade) :-
#   conf_reported(C, V, Y, 'R+'),
#   conf_survey(C, V, Y, 'S+'),
#   conf_denominator(C, V, Y, 'D+'),
#   !,
#   Expl = 'GoC=R+ S+ D+',
#   Grade = 3.

index = which(Conf.Rep == "R+" & Conf.Svy == "S+" & Conf.Den =="D+")
GoC[index] = 3
GoC.Expl[index] = "GoC=R+ S+ D+"

# % Confidence rated by working group
# confidence(C, V, Y, Expl, Grade) :-
#   decision(C, V, Y, assignGoC, Expl0, _, Grade0),
#   !,
#   concat_atom(['GoC=Assigned by working group. ', Expl0], Expl),
#   Grade = Grade0.

index = Decisions[Decisions$Dec == "assignGoC", ]
GoC[cbind(index$Y, index$V)] = index$Cov # Cov is GoC here
GoC.Expl[cbind(index$Y, index$V)] = sprintf("GoC=Assigned by working group. %s",
    index$Info)

# % Copy rcv1 from mcv2
# %
# % MG, discuss: this differs from the rule in wuenic_I
# confidence(C, rcv1, Y, Expl, Grade) :-
#   estimate_required(C, rcv1, Y, _, mcv2),
#   !,
#   confidence(C, mcv2, Y, Expl, Grade).

index = which(Ereq[, "rcv1"] & Rub[, "rcv1"] == "mcv2")
GoC[index, "rcv1"] = GoC[index, "mcv2"]
GoC.Expl[index, "rcv1"] = GoC.Expl[index, "mcv2"]

# % Copy rcv1 from mcv1
# confidence(C, rcv1, Y, Expl, Grade) :-
#   estimate_required(C, rcv1, Y, _, na),
#   !,
#   confidence(C, mcv1, Y, Expl, Grade).

index = Ereq[, "rcv1"] & is.na(Rub[, "rcv1"])
GoC[index, "rcv1"] = GoC[index, "mcv1"]
GoC.Expl[index, "rcv1"] = GoC.Expl[index, "mcv1"]

# % Flag modifications in the program code that change the coverage
# % estimates
# change_from_previous(C, V, Y, Coverage, Change) :-
#     legacy(C, V, Y, Legacy),
#     Legacy \= Coverage,
#     !,
#     concat_atom(['Estimate of ', Coverage,
#         ' percent changed from previous revision value of ',
#         Legacy,' percent. '], Change).
#
# change_from_previous(_C, _V, _Y, _, '').

Change = YV.char()
Change[] = ""
index = which(Bounded != Legacy)
Change[index] = sprintf(
  "Estimate of %i percent changed from previous revision value of %i percent. ",
  Bounded[index], Legacy[index])

# % Collect explanations in natural language terms
# collect_explanations(C, V, Y, Explanations) :-
#     findall(Expl, explanation(C, V, Y, Expl), Explanations).

Expl = YV.char()
Expl[] = ""

# explanation(C, V, Y, Expl) :-
#     survey_reason_to_exclude(C, V, Y, _, Expl).
#
# % Reasons to exclude a survey include:
# %    Sample size < 300,
# %    The working group decides to exclude the survey.
# %
# % used for explanation/4
# survey_reason_to_exclude(C, V, Y, ID, Expl) :-
#     survey_for_analysis(C, V, Y, ID, Description, _),
#     not(decision(C, V, Y, acceptSurvey, Expl, ID, _)),
#     member(ss:Size, Description),
#     Size < 300,
#     concat_atom(['Survey results ignored. Sample size ', Size,
#         ' less than 300. '], Expl).

cnf    = Survey$Info.confirm == "card or history"
age    = Survey$Info.age %in% c("12-23 m", "18-29 m", "15-26 m", "24-35 m")
# accept = Survey$Id %in% Decisions$Id[Decisions$Dec == "acceptSurvey"]
size   = Survey$Info.ss < 300
index  = Survey[cnf & age & size, ]

accept   = Decisions[Decisions$Dec == "acceptSurvey" & !is.na(Decisions$Id), ]

# MG, discuss: bgd, bcg/1999: twice the same message, without Survey ID
if(nrow(index))
  for(i in 1:nrow(index))
  {
    if(!any(index$V[i] == accept$V & index$Y[i] == accept$Y & index$Id[i] == accept$Id))
      Expl[index$Yn[i], index$V[i]] = sprintf(
        "%sSurvey results ignored. Sample size %i less than 300. ", 
        Expl[index$Yn[i], index$V[i]], index$Info.ss[i])
  }

# % V4: Keeps explanations for the same survey together
# survey_reason_to_exclude(C, V, Y, ID, Expl) :-
#     survey_for_analysis(C, V, Y, ID, Description, _),
#     (   decision(C, V, Y, ignoreSurvey, Expl0, ID, _)
#     ;   decision(C, V, Y, ignoreSurvey, Expl0, na, _)
#     ),
#     member(title:Title, Description),
#     concat_atom([Title, ' results ignored by working group. ', Expl0],
#     Expl).

# Some surveys are ignored by the working group (by year and vaccine, no Id)
ignore = Decisions[Decisions$Dec == "ignoreSurvey" & is.na(Decisions$Id), ]
if(nrow(ignore))
  for(i in 1:nrow(ignore))
  {
    Ids = which(!is.na(Svy.Ana[ignore$Y[i], ignore$V[i], , drop=FALSE]), arr.ind=TRUE)
    Ids = dimnames(Svy.Ana)$Id[Ids[, "Id"]]
    if(length(Ids))
      for(j in 1:length(Ids))
      {
        Expl[ignore$Y[i], ignore$V[i]] = sprintf(
          "%s%s results ignored by working group. %s",
          Expl[ignore$Y[i], ignore$V[i]],
          Survey$Info.title[Survey$Id == Ids[j]][1], ignore$Info[i])
      }
  }

# Some surveys are ignored by the working group
ignore = Decisions[Decisions$Dec == "ignoreSurvey" & !is.na(Decisions$Id), ]
if(nrow(ignore))
  for(i in 1:nrow(ignore))
    if(!is.na(Svy.Ana[ignore$Y[i], ignore$V[i], ignore$Id[i]]))
    {
      Expl[ignore$Y[i], ignore$V[i]] = sprintf(
        "%s%s results ignored by working group. %s",
        Expl[ignore$Y[i], ignore$V[i]],
        Survey$Info.title[Survey$Id == ignore$Id[i]][1], ignore$Info[i])
    }

# explanation(C, V, Y, Expl) :-
#     survey_modified(C, V, Y, _, Expl, _).

index = which(!is.na(Svy.Info), arr.ind=TRUE)
if(nrow(index))
  for(i in 1:nrow(index))
  {
    Expl[index[i, "Y"], index[i, "V"]] = sprintf("%s%s",
      Expl[index[i, "Y"], index[i, "V"]],
      Svy.Info[index[i, "Y"], index[i, "V"], index[i, "Id"]])
  }

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
