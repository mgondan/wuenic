library(zoo)
library(rolog)
consult("xsb/bgd.pl")

Vn = c("bcg", "dtp1", "dtp3", "hepb3", "hib3", "ipv1", "mcv1", "mcv2", "pcv3",
  "pol1", "pol3", "rcv1")
Yn = 1997:2022

sawtooth = 10
svy.thrs = 10

# the default setting
Rubella = YV_char
Rubella[, "rcv1"] = "mcv2"

YV = list(Yn, Vn)
YV_bool = matrix(FALSE, nrow=length(Yn), ncol=length(Vn), dimnames=YV)
YV_int = matrix(NA_integer_, nrow=length(Yn), ncol=length(Vn), dimnames=YV)
YV_real = matrix(NA_real_, nrow=length(Yn), ncol=length(Vn), dimnames=YV)
YV_char = matrix(NA_character_, nrow=length(Yn), ncol=length(Vn), dimnames=YV)

# Prolog atoms to R character strings
# Prolog variables to R character strings
# Prolog list elements name:elem to named list elements in R
#
atom2char = function(q)
{ if(is.expression(q))
    return(as.character(q))
  
  if(is.symbol(q))
    return(as.character(q))
  
  if(is.call(q))
  { args <- as.list(q)
    args[-1] <- lapply(args[-1], FUN=atom2char)
    return(as.call(args))
  }

  if(is.list(q) && !is.null(names(q)))
    return(lapply(q, atom2char))
  
  if(is.list(q))
  { n = NULL
    for(i in 1:length(q))
    { item = as.list(q[[i]])
      q[[i]] = atom2char(item[[3]])
      n = c(n, atom2char(item[[2]]))
    }
    names(q) = n
    return(q)
  }
  
  return(q)
}

# Read all pred(C, Y, Out) and save Out in a vector
rep3 = function(pred="births_UNPD")
{ q = call(pred, expression(C), expression(Y), expression(Out))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  s$Y = as.character(s$Y)

  m = NA + numeric(length(Yn))
  names(m) = Yn
  m[s$Y] = s$Out
  return(m)
}

# Read all pred(C, V, Y, Out) and save Out in a matrix
rep4 = function(pred="admin")
{ q = call(pred, expression(C), expression(V), expression(Y), expression(Out))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  s$Y = as.character(s$Y)

  index = which(!(s$V %in% Vn))
  if(length(index))
  { warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
    s = s[-index, ]
  }

  m = YV_int
  m[cbind(s$Y, s$V)] = s$Out
  return(m)
}

est_req = function()
{
  q = call("estimate_required", expression(C), expression(V), expression(Y), 
           expression(Comb), expression(A5))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  s$Y = as.character(s$Y)
  
  m = YV_bool
  m[cbind(s$Y, s$V)] = TRUE
  return(m)
}

rubella = function()
{
  q = call("estimate_required", expression(C), expression(V), expression(Y), 
           expression(Comb), expression(Rub))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  s$Y = as.character(s$Y)
  
  m = YV_char
  m[cbind(s$Y, s$V)] = s$Rub
  return(m)
}

# Multiple surveys per year, therefore no YV-matrix, but a long data frame
survey_results = function()
{ q = call("survey_results", expression(C), expression(V), expression(Y), 
    expression(Id), expression(Info), expression(Cov))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)

  index = which(!(s$V %in% Vn))
  if(length(index))
  { warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
    s = s[-index, ]    
  }

  return(s)
}

# Multiple decisions per year, therefore no YV-matrix, but a long data frame
wgd = function()
{
  # Convenience function: Replace NULL by NA in lists
  padNA = function(q, v)
  { qq = q[v]
    names(qq) = v
    qq[] = ifelse(lapply(qq, is.null), NA, qq)
    return(qq)
  }
  
  q = call("wgd", expression(C), expression(V), expression(Y0), expression(Y1),
           expression(Dec), expression(Info),
           expression(A1), expression(Cov), expression(A3), expression(A4))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, padNA,
    v=c("C", "V", "Y0", "Y1", "Dec", "Info", "Id", "Cov", "A3", "A4"))
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)

  # V = NA means that a decision applies to all vaccines
  V.na = function(d)
  { if(is.na(d['V']))
      return(data.frame(V=Vn, Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'], 
        Id=d['Id'], Info=d['Info'], Cov=d['Cov'], row.names=NULL))

    data.frame(V=d['V'], Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'],
      Id=d['Id'], Info=d['Info'], Cov=d['Cov'])
  }

  s = apply(s, MARGIN=1, simplify=FALSE, FUN=V.na)
  s = do.call("rbind", s)
  
  # If a year range is given, apply decision to each included year
  Y.range = function(d)
  { data.frame(V=d['V'], Y=d['Y0']:min(2022, d['Y1']),
      Dec=d['Dec'], Id=d['Id'], Info=d['Info'], Cov=d['Cov'], row.names=NULL)
  }

  s = apply(s, MARGIN=1, simplify=FALSE, FUN=Y.range)
  s = do.call("rbind", s)
  s$Cov = as.integer(s$Cov)
  s$Y = as.character(s$Y)
  return(s)
}

# 1. Load country-specific information
s = once(call("country", expression(Code), expression(Country)))
Code = atom2char(s$Code)
Country = atom2char(s$Country)

s = once(call("date", expression(Date)))
Date = atom2char(s$Date)

Ereq = est_req()
Rubella = rubella()
Admin = rep4("admin")
Gov = rep4("gov")
Legacy = rep4("legacy")
Vaccinated = rep4("vaccinated")
Target = rep4("target")
Births = rep3("births_UNPD")
Surviving = rep3("si_UNPD")
Survey = survey_results()
Decisions = wgd()

# 2. Add information from government and administration.
#
# Reported to WHO and UNICEF is government estimate. If government
# estimate missing, then reported is administrative data. If both
# missing, fail.
#
# R implementation is in reverse order, with the higher-order predicate
# eventually overwriting the lower predicate.

Rep.Cov = YV_real
Rep.Src = YV_char

# reported(C, V, Y, Source, Coverage) :-
#     admin0(C, V, Y, Cov0),
#     (   decision(C, V, Y, ignoreGov, _, _, _)
#     ;   not(gov(C, V, Y, _))
#     ),
#     not(decision(C, V, Y, ignoreAdmin, _, _, _)),
#     !,
#     Source = admin,
#     Coverage = Cov0.

Rep.Cov = Admin
Rep.Src = ifelse(is.na(Admin), NA, "admin")

# Working group decides to ignore admin data
ignore = Decisions$Dec == "ignoreAdmin"
index = cbind(Decisions$Y[ignore], Decisions$V[ignore])
Rep.Cov[index] = NA
Rep.Src[index] = NA

# Use admin data only if government data is invalid
# Todo: This code can be removed, data will be overwritten below

gov = !is.na(Gov)
ignore = Decisions$Dec == "ignoreGov"
gov[cbind(Decisions$Y[ignore], Decisions$V[ignore])] = FALSE
Rep.Cov[gov] = NA
Rep.Src[gov] = NA

# reported(C, V, Y, Source, Coverage) :-
#    gov(C, V, Y, Cov0),
#    not(decision(C, V, Y, ignoreGov, _, _, _)),
#    !,
#    Source = gov,
#    Coverage = Cov0.

gov = !is.na(Gov)
ignore = Decisions$Dec == "ignoreGov"
gov[cbind(Decisions$Y[ignore], Decisions$V[ignore])] = FALSE
Rep.Cov[gov] = Gov[gov]
Rep.Src[gov] = "gov"

# 3. Check reported data
#
# Reasons to exclude reported data are working group decisions, coverage > 100%
# or temporal inconsistency.
#
reject = YV_bool

# reported_rejected(C, V, Y) :-
#     reported(C, V, Y, _, Coverage),
#     not(reported_later(C, V, Y)),
#     Prec is Y - 1,
#     reported(C, V, Prec, _, PrecCov),
#     sawtooth_threshold(Threshold),
#     (   member(V, [pcv3, rotac])
#     ->  PrecCov - Coverage > Threshold
#     ;   abs(PrecCov - Coverage) > Threshold
#     ), !.

# Sudden decline in most recently reported data for new vaccines
V.new = Rep.Cov[, Vn %in% c("pcv3", "rotac"), drop=FALSE]

# Search for last reported data
Diff = apply(V.new, 2, diff, simplify=FALSE)                 # jumps
Diff = lapply(Diff, na.trim, sides="right")                  # last reported
Diff = lapply(Diff, rev)                                     # -> first
Diff = lapply(Diff, `[`, 1)                                  # jump of interest

Y = sapply(Diff, names)
V = names(Diff)
J = sapply(Diff, `<`, sawtooth)                              # check for decline

# Skip if J is NA (for vaccines with only NA reported)
reject[cbind(Y[!is.na(J)], V[!is.na(J)])] = J[!is.na(J)]

# Sudden change in most recently reported data for classic vaccines
V.new = Rep.Cov[, !(Vn %in% c("pcv3", "rotac")), drop=FALSE]
Diff = apply(V.new, 2, diff, simplify=FALSE)
Diff = lapply(Diff, na.trim, sides="right")
Diff = lapply(Diff, rev)
Diff = lapply(Diff, `[`, 1)
Diff = lapply(Diff, abs)                                     # up or down

Y = sapply(Diff, names)
V = names(Diff)
J = sapply(Diff, `>`, sawtooth)
reject[cbind(Y[!is.na(J)], V[!is.na(J)])] = J[!is.na(J)]

# reported_rejected(C, V, Y) :-
#     reported(C, V, Y, _, Coverage),
#     Prec is Y - 1,
#     Succ is Y + 1,
#     reported(C, V, Prec, _, PrecCov),
#     reported(C, V, Succ, _, SuccCov),
#     sawtooth_threshold(Threshold),
#     (   Coverage - PrecCov > Threshold,
#         Coverage - SuccCov > Threshold
#     ;   PrecCov - Coverage > Threshold,
#         SuccCov - Coverage > Threshold
#     ), !.

up    = apply(rbind(NA, Rep.Cov), 2, diff) > sawtooth
down  = apply(rbind(Rep.Cov, NA), 2, diff) < -sawtooth
index = which(up & down, arr.ind=TRUE)
reject[index] = TRUE

down  = apply(rbind(NA, Rep.Cov), 2, diff) < -sawtooth
up    = apply(rbind(Rep.Cov, NA), 2, diff) > sawtooth
index = which(down & up, arr.ind=TRUE)
reject[index] = TRUE

# % Implausible coverage
# reported_rejected(C, V, Y) :-
#     reported(C, V, Y, _, Coverage),
#     Coverage > 100,
#     !.

index = which(Rep.Cov > 100, arr.ind=TRUE)
reject[index] = TRUE

# reported_rejected(C, V, Y) :-
#     decision(C, V, Y, acceptReported, _, _, _),
#     !,
#     fail.

index = Decisions[Decisions$Dec == "acceptReported", ]
reject[cbind(index$Y, index$V)] = FALSE

# % Ignore dominates accept
# reported_rejected(C, V, Y) :-
#     decision(C, V, Y, ignoreReported, _Expl0, _, _),
#     !.

index = Decisions[Decisions$Dec == "ignoreReported", ]
reject[cbind(index$Y, index$V)] = TRUE

Rep.Cov[reject] = NA
Rep.Src[reject] = NA

# 4. Time series of reported data
#
# % Reported data available
# reported_time_series(C, V, Y, Source, Coverage) :-
#     estimate_required(C, V, Y, _, _),
#     reported(C, V, Y, Source0, Cov0),
#     not(reported_rejected(C, V, Y)),
#     !,
#     Source = Source0,
#     Coverage = Cov0.

index = Ereq
Rep.Cov[!index] = NA
Rep.Src[!index] = NA

# % Interpolation, no data/reported data excluded between two years
# reported_time_series(C, V, Y, Source, Coverage) :-
#     estimate_required(C, V, Y, _, _),
#     (   not(reported(C, V, Y, _, _))
#     ;   reported_rejected(C, V, Y)
#     ),
#     year_before_reported(C, V, Y, Prec, PrecCov),
#     year_after_reported(C, V, Y, Succ, SuccCov),
#     !,
#     Source = interpolated,
#     interpolate(Prec, PrecCov, Succ, SuccCov, Y, Coverage).

inter = apply(Rep.Cov, 2, na.approx, na.rm=FALSE)
index = Ereq & is.na(Rep.Cov) & !is.na(inter)
Rep.Cov[index] = round(inter[index])
Rep.Src[index] = "interpolated"

# % Extrapolation, latest required estimate
# reported_time_series(C, V, Y, Source, Coverage) :-
#     estimate_required(C, V, Y, _, _),
#     (   not(reported(C, V, Y, _, _))
#     ;   reported_rejected(C, V, Y)
#     ),
#     nearest_reported(C, V, Y, _Year, Cov0),
#     !,
#     Source = extrapolated,
#     Coverage = Cov0.

extra = apply(Rep.Cov, 2, zoo::na.locf, na.rm=FALSE)
index = Ereq & is.na(Rep.Cov) & !is.na(extra)
Rep.Cov[index] = round(extra[index])
Rep.Src[index] = "extrapolated"

extra = apply(Rep.Cov, 2, zoo::na.locf, na.rm=FALSE, fromLast=TRUE)
index = Ereq & is.na(Rep.Cov) & !is.na(extra)
Rep.Cov[index] = round(extra[index])
Rep.Src[index] = "extrapolated"

# 5. Survey data
#
# % Survey results passed for inclusion in the analysis include:
# % card or history results for cohorts 12-23, 18-29, 15-26, 24-35 months of age
# survey_for_analysis(C, V, Y, ID, Description, Coverage) :-
#     survey_results0(C, V, Y, ID, Description, Coverage),
#     member(confirm:'card or history', Description),
#     member(age:AgeCohort, Description),
#     member(AgeCohort, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']).

cnf = Survey$Info.confirm == "card or history"
age = Survey$Info.age %in% c("12-23 m", "18-29 m", "15-26 m", "24-35 m")
Svy.Ana = Survey[cnf & age, ]

# % Reasons to exclude a survey include:
# %    Sample size < 300,
# %    The working group decides to exclude the survey.
# survey_accepted(C, V, Y, ID, Coverage) :-
#     survey_for_analysis(C, V, Y, ID, Desc, Cov0),
#     (   decision(C, V, Y, acceptSurvey, _, ID, _)
#     ;   member(ss:Size, Desc),
#         Size >= 300,
#         not(decision(C, V, Y, ignoreSurvey, _, ID, _)),
#         not(decision(C, V, Y, ignoreSurvey, _, na, _))
#     ),
#     % Check if survey needs to be modified
#     (   survey_modified(C, V, Y, ID, _, Modified)
#     ->   Coverage = Modified
#     ;   Coverage = Cov0
#     ).

size   = Svy.Ana$Info.ss >= 300
ignore = Svy.Ana$Id %in% Decisions$Id[Decisions$Dec == "ignoreSurvey"]
year   = Svy.Ana$Y %in% Decisions$Y[Decisions$Dec == "ignoreSurvey" & is.na(Decisions$Id)]
accept = Svy.Ana$Id %in% Decisions$Id[Decisions$Dec == "acceptSurvey"]
index  = (size & !ignore & !year) | accept
Svy.Ana = Svy.Ana[index, ]

# % Recall bias is estimated by comparing the first and third dose of a vaccine
#
# vaccine(dtp3, dtp1).
# vaccine(pol3, pol1).
# vaccine(hib3, hib1).
# vaccine(hepb3, hepb1).
# vaccine(pcv3, pcv1).
#
# survey_modified(C, V, Y, ID, Expl, Coverage) :-
#     member(V, [dtp3, pol3, hib3, hepb3, pcv3]),
#
#     % Third dose, card only
#     survey_results0(C, V, Y, ID, DescriptionCard3Dose, C3Cov),
#     member(confirm:card, DescriptionCard3Dose),
#     member(age:AgeCohortCard3Dose, DescriptionCard3Dose),
#     member(AgeCohortCard3Dose, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),
#
#     % First dose, card or history
#     vaccine(V, First),
#     survey_results0(C, First, Y, ID, DescriptionCoH1Dose, CoH1Cov),
#     member(confirm:'card or history', DescriptionCoH1Dose),
#     member(age:AgeCohortCoH1, DescriptionCoH1Dose),
#     member(AgeCohortCoH1, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),
#
#     % First dose, card only
#     survey_results0(C, First, Y, ID, DescriptionCard1Dose, C1Cov),
#     C1Cov > 0,
#     member(confirm:card, DescriptionCard1Dose),
#     member(age:AgeCohortCard1Dose, DescriptionCard1Dose),
#     member(AgeCohortCard1Dose, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']),
#
#     Adj is C3Cov / C1Cov,
#     ThirdHistoryAdj is (CoH1Cov - C1Cov) * Adj,
#     CovAdjusted is C3Cov + ThirdHistoryAdj,
#     bound_0_100(CovAdjusted, Cov0),
#
#     survey_for_analysis(C, V, Y, ID, Description, SurveyCoverage),
#     Cov0 \= SurveyCoverage,
#
#     SurveyCovRounded is round(SurveyCoverage),
#     CH1 is round(CoH1Cov),
#     C1 is round(C1Cov),
#     C3 is round(C3Cov),
#     member(title:Title, Description),
#     concat_atom([Title, ' card or history results of ', SurveyCovRounded,
#         ' percent modifed for recall bias to ', Cov0,
#         ' percent based on 1st dose card or history coverage of ',
#         CH1, ' percent, 1st dose card only coverage of ',
#         C1, ' percent and 3rd dose card only coverage of ',
#         C3, ' percent. '], Expl),
#         Coverage = Cov0.

# Todo... (nicht vergnügungssteuerpflichtig = not subject to entertainment tax)

# % Survey information for given year. Multiple surveys are averaged.
# survey(C, V, Y, Expl, Coverage) :-
#     findall(Cov, survey_accepted(C, V, Y, _, Cov), [H | T]),
#     length([H | T], N),
#     sum_list([H | T], Sum),
#     Coverage is round(Sum / N),
#     concat_atom(['Survey evidence of ', Coverage, ' percent based on ',
#         N, ' survey(s). '], Expl).

Svy.Cov = YV_int
index = with(Svy.Ana, aggregate(Cov, by=list(Y=Y, V=V), FUN=mean))
Svy.Cov[cbind(index$Y, index$V)] = round(index$x)

Svy.Info = YV_char
index = with(Svy.Ana, aggregate(Cov, by=list(Y=Y, V=V), FUN=length))
Svy.Info[cbind(index$Y, index$V)] = sprintf(
  "Survey evidence of %g percent based on %i survey(s).", 
  Svy.Cov[cbind(index$Y, index$V)], index$x)

# 6. Determine coverage value at anchor points defined as years with multiple
#    data points (reported | survey | wgd).

Anchor.Rule = YV_char
Anchor.Info = YV_char
Anchor.Cov = YV_int

# % Survey results challenge reported
# anchor(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, _, _Cov0),
#     survey(C, V, Y, Expl0, Survey),
#     % survey_reported_threshold(Threshold),
#     % abs(Cov0 - Survey) > Threshold,
#     !,
#     Rule = 'S: AP',
#     concat_atom(['Survey evidence does not support reported data. Estimate based on survey results. ',
#     Expl0, ' '], Expl),
#     Coverage = Survey.

index = which(abs(Svy.Cov - Rep.Cov) > svy.thrs, arr.ind=TRUE)
Anchor.Rule[index] = "S: AP"
Anchor.Info[index] = sprintf(
  "Survey evidence does not support reported data. Estimate based on survey results. %s",
  Svy.Info[index])
Anchor.Cov[index] = Svy.Cov[index]

# % Survey results support reported
# anchor(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, Source, Cov0),
#     survey(C, V, Y, Expl0, Survey),
#     survey_reported_threshold(Threshold),
#     abs(Cov0 - Survey) =< Threshold,
#     !,
#     Rule = 'R: AP',
#     member(Source-Expl1,
#       [ gov-'Estimate informed by reported data supported by survey. ',
#         admin-'Estimate informed by reported administrative data supported by survey. ',
#         interpolated-'Estimate informed by interpolation between reported data supported by survey. ',
#         extrapolated-'Estimate based on extrapolation from data reported by national government supported by survey. '
#       ]),
#     concat_atom([Expl1, Expl0], Expl),
#     Coverage = Cov0.

info = c(
  gov="Estimate informed by reported data supported by survey. ",
  admin="Estimate informed by reported administrative data supported by survey. ",
  interpolated="Estimate informed by interpolation between reported data supported by survey. ",
  extrapolated="Estimate based on extrapolation from data reported by national government supported by survey. ")

index = which(abs(Svy.Cov - Rep.Cov) <= svy.thrs, arr.ind=TRUE)
Anchor.Rule[index] = "R: AP"
Anchor.Cov[index] = Rep.Cov[index]
Anchor.Info[index] = sprintf("%s %s", info[Rep.Src[index]], Svy.Info[index])

# % Reported value "anchored" by working group
# anchor(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, _, Cov0),
#     decision(C, V, Y, assignAnchor, Expl0, _, Cov0), % same Cov0
#     !,
#     Rule = 'R: AP',
#     Expl = Expl0,
#     Coverage = Cov0.

index = Decisions[Decisions$Dec == "assignAnchor", ]
equal = which(Rep.Cov[cbind(index$Y, index$V)] == index$Cov, arr.ind=TRUE)
index = index[equal, ]
Anchor.Rule[cbind(index$Y, index$V)] = "R: AP"
Anchor.Info[cbind(index$Y, index$V)] = index$Info
Anchor.Cov[cbind(index$Y, index$V)] = index$Cov

# % Working group assigns anchor point value.
# anchor(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, _, _Cov0),
#     decision(C, V, Y, assignAnchor, Expl0, _, Assigned),
#     % Cov0 \= Assigned,
#     !,
#     Rule = 'W: AP',
#     concat_atom(['Estimate of ', Assigned, ' percent assigned by working group. ',
#         Expl0], Expl),
#     Coverage = Assigned.

index = Decisions[Decisions$Dec == "assignAnchor", ]
neq = which(Rep.Cov[cbind(index$Y, index$V)] != index$Cov, arr.ind=TRUE)
index = index[neq, ]
Anchor.Rule[cbind(index$Y, index$V)] = "W: AP"
Anchor.Cov[cbind(index$Y, index$V)] = index$Cov
Anchor.Info[cbind(index$Y, index$V)] = sprintf(
  "Estimate of %g percent assigned by working group. ", index$Cov)

# % Level II: Estimate coverage by distinguishing different cases
# %
# % * Estimate at anchor point
# % * Estimate between anchor points
# % * Estimate before anchor point
# % * Estimate after anchor point
# % * No anchor points
#
# % No anchor points for any year, use reported
# wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, Source, Cov0),
#     !,
#     Rule = 'R:',
#     member(Source-Expl,
#       [ gov-'Estimate informed by reported data. ',
#         admin-'Estimate informed by reported administrative data. ',
#         interpolated-'Estimate informed by interpolation between reported data. ',
#         extrapolated-'Estimate informed by extrapolation from reported data. '
#       ]),
#     Coverage = Cov0.

info = c(
  gov="Estimate informed by reported data. ",
  admin="Estimate informed by reported administrative data. ",
  interpolated="Estimate informed by interpolation between reported data. ",
  extrapolated="Estimate informed by extrapolation from reported data. ")
W2.Cov = Rep.Cov

W2.Info = YV_char
W2.Info[] = info[Rep.Src]

W2.Rule = YV_char
W2.Rule[!is.na(Rep.Cov)] = "R:"

# % Before earliest/after latest anchor (not of type reported): calibrated
# wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, _, Cov0),
#     (   succ_anchor(C, V, Y, Anchor, AnchorRule, AnchorCov),
#         AnchorRule \= 'R: AP'
#     ;   prec_anchor(C, V, Y, Anchor, AnchorRule, AnchorCov),
#         AnchorRule \= 'R: AP'
#     ), !,
#     Rule = 'C:',
#     concat_atom(['Reported data calibrated to ', Anchor, ' levels. '], Expl),
#     reported_time_series(C, V, Anchor, _, ReportedAtAnchor),
#     Adj is AnchorCov - ReportedAtAnchor,
#     Coverage is round(Cov0 + Adj).

# Search for preceding anchor
index = !is.na(Rep.Cov) & is.na(Anchor.Cov)
Prec.Rule = apply(Anchor.Rule, 2, FUN=na.locf, na.rm=FALSE)
index = index & !is.na(Prec.Rule) & Prec.Rule != "R: AP"

Prec.Cov = apply(Anchor.Cov, 2, FUN=na.locf, na.rm=FALSE)

Prec.Year = YV_char
Prec.Year[] = Yn
Prec.Year[index] = NA
Prec.Year = apply(Prec.Year, 2, FUN=na.locf, na.rm=FALSE)

Rule = YV_char
Rule[index] = "C:"

Info = YV_char
Info[index] = sprintf("Reported data calibrated to %s levels. ", 
    Prec.Year[index])

yv = expand.grid(Y=Yn, V=Vn, stringsAsFactors=FALSE)
Adj = Anchor.Cov[cbind(c(Prec.Year), yv$V)] - Rep.Cov[cbind(c(Prec.Year), yv$V)]
Cov = YV_int
Cov[index] = Rep.Cov[index] + Adj[index]

# Search for next anchor
index = !is.na(Rep.Cov) & is.na(Anchor.Cov)
Succ.Rule = apply(Anchor.Rule, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)
index = index & !is.na(Succ.Rule) & Succ.Rule != "R: AP"

Succ.Cov = apply(Anchor.Cov, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)

Succ.Year = YV_char
Succ.Year[] = Yn
Succ.Year[index] = NA
Succ.Year = apply(Succ.Year, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)

Rule[index] = "C:"

Info[index] = sprintf("Reported data calibrated to %s levels. ", 
    Succ.Year[index])

yv = expand.grid(Y=Yn, V=Vn, stringsAsFactors=FALSE)
Adj = Anchor.Cov[cbind(c(Succ.Year), yv$V)] - Rep.Cov[cbind(c(Succ.Year), yv$V)]
Cov[index] = Rep.Cov[index] + Adj[index]

# % Before earliest/after latest anchor (of type reported)
# wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, Source, Cov0),
#     (   succ_anchor(C, V, Y, _Anchor, AnchorRule, _AnchorCov),
#         AnchorRule = 'R: AP'
#     ;   prec_anchor(C, V, Y, _Anchor, AnchorRule, _AnchorCov),
#         AnchorRule = 'R: AP'
#     ), !,
#     Rule = 'R:',
#     member(Source-Expl,
#       [ gov-'Estimate informed by reported data. ',
#         admin-'Estimate informed by reported data. ',
#         interpolated-'Estimate informed by interpolation between reported data. ',
#         extrapolated-'Estimate based on extrapolation from data reported by national government. '
#       ]),
#     Coverage = Cov0.

# Search for preceding anchor
index = !is.na(Rep.Cov) & is.na(Anchor.Cov)
Prec.Rule = apply(Anchor.Rule, 2, FUN=na.locf, na.rm=FALSE)
index = index & !is.na(Prec.Rule) & Prec.Rule == "R: AP"

info = c(gov="Estimate informed by reported data. ",
    admin="Estimate informed by reported data. ",
    interpolated="Estimate informed by interpolation between reported data. ",
    extrapolated="Estimate based on extrapolation from data reported by national government. ")

Rule[index] = "R:"
Info[index] = info[Rep.Src[index]]
Cov[index] = Rep.Cov[index]

# Search for next anchor
index = !is.na(Rep.Cov) & is.na(Anchor.Cov)
Succ.Rule = apply(Anchor.Rule, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)
index = index & !is.na(Succ.Rule) & Succ.Rule == "R: AP"

Rule[index] = "R:"
Info[index] = info[Rep.Src[index]]
Cov[index] = Rep.Cov[index]

# % Between other anchor points (not both of type "reported"): calibrate
# wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, _Source, _Cov0),
#     prec_anchor(C, V, Y, Prec, PrecRule, PrecCov),
#     succ_anchor(C, V, Y, Succ, SuccRule, SuccCov),
#     ( PrecRule \= 'R: AP' ; SuccRule \= 'R: AP' ),
#     !,
#     Rule = 'C:',
#     concat_atom(['Reported data calibrated to ', Prec,
#         ' and ', Succ, ' levels. '], Expl),
#     reported_time_series(C, V, Prec, _, PrecRep),
#     reported_time_series(C, V, Succ, _, SuccRep),
#     interpolate(Prec, PrecRep, Succ, SuccRep, Y, RepInterp),
#     interpolate(Prec, PrecCov, Succ, SuccCov, Y, AnchInterp),
#     Adj is AnchInterp - RepInterp,
#     Coverage is round(Reported + Adj).

index = !is.na(Rep.Cov) & is.na(Anchor.Cov)
Prec.Rule = apply(Anchor.Rule, 2, FUN=na.locf, na.rm=FALSE)
Succ.Rule = apply(Anchor.Rule, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)
index = index & !is.na(Prec.Rule) & !is.na(Succ.Rule)
index = index & (Prec.Rule != "R: AP" | Succ.Rule != "R: AP")

Prec.Cov = apply(Anchor.Cov, 2, FUN=na.locf, na.rm=FALSE)
Succ.Cov = apply(Anchor.Cov, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)

Rule[index] = "C:"

Prec.Year = YV_char
Prec.Year[] = Yn
Prec.Year[index] = NA
Prec.Year = apply(Prec.Year, 2, FUN=na.locf, na.rm=FALSE)

Succ.Year = YV_char
Succ.Year[] = Yn
Succ.Year[index] = NA
Succ.Year = apply(Succ.Year, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)

Info[index] = sprintf("Reported data calibrated to %s and %s levels. ", 
    Prec.Year[index], Succ.Year[index])

# interpolate Anchor.Cov for year without anchor
Itp1.Cov = apply(Anchor.Cov, 2, FUN=na.approx, na.rm=FALSE)

# interpolate Rep.Cov for year without anchor
Itp2.Cov = Rep.Cov
Itp2.Cov[index] = NA
Itp2.Cov = apply(Itp2.Cov, 2, FUN=na.approx, na.rm=FALSE)

yv = expand.grid(Y=Yn, V=Vn, stringsAsFactors=FALSE)
Adj = Itp1.Cov - Itp2.Cov
Cov[index] = round(Rep.Cov[index] + Adj[index])

# % Between anchor points: between two reported anchors
# wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, Source, Cov0),
#     prec_anchor(C, V, Y, _Prec, PrecRule, _),
#     PrecRule = 'R: AP',
#     succ_anchor(C, V, Y, _Succ, SuccRule, _),
#     SuccRule = 'R: AP',
#     !,
#     Rule = 'R:',
#     member(Source-Expl,
#       [ gov-'Estimate informed by reported data. ',
#         admin-'Estimate informed by reported administrative data. ',
#         interpolated-'Estimate informed by interpolation between reported data. '
#       ]),
#     Coverage = Cov0.

index = !is.na(Rep.Cov) & is.na(Anchor.Cov)
Prec.Rule = apply(Anchor.Rule, 2, FUN=na.locf, na.rm=FALSE)
Succ.Rule = apply(Anchor.Rule, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)
index = index & !is.na(Prec.Rule) & !is.na(Succ.Rule)
index = index & Prec.Rule == "R: AP" & Succ.Rule == "R: AP"

info = c(gov="Estimate informed by reported data. ",
    admin="Estimate informed by reported administrative data. ",
    interpolated="Estimate informed by interpolation between reported data. ")

Rule[index] = "R:"
Info[index] = info[Rep.Src[index]]
Cov[index] = Rep.Cov[index]

# % Between anchor points: interpolation forced by working group
# wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
#     decision(C, V, Y, interpolate, Expl0, _, _),
#     prec_anchor(C, V, Y, Prec, _, PrecCov),
#     succ_anchor(C, V, Y, Succ, _, SuccCov),
#     !,
#     Rule = 'W-I:',
#     concat_atom(['Estimate informed by interpolation between ', Prec,
#         ' and ', Succ, ' levels. ', Expl0], Expl),
#     interpolate(Prec, PrecCov, Succ, SuccCov, Y, Coverage).

index = Decisions[Decisions$Dec == "interpolate", ]
index = cbind(index$Y, index$V)

Prec.Rule = apply(Anchor.Rule, 2, FUN=na.locf, na.rm=FALSE)
Succ.Rule = apply(Anchor.Rule, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)

Prec.Cov = apply(Anchor.Cov, 2, FUN=na.locf, na.rm=FALSE)
Succ.Cov = apply(Anchor.Cov, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)

Prec.Year = YV_char
Prec.Year[] = Yn
Prec.Year[index] = NA
Prec.Year = apply(Prec.Year, 2, FUN=na.locf, na.rm=FALSE)

Succ.Year = YV_char
Succ.Year[] = Yn
Succ.Year[index] = NA
Succ.Year = apply(Succ.Year, 2, FUN=na.locf, fromLast=TRUE, na.rm=FALSE)

Rule[index] = "W-I:"
Info[index] = sprintf(
  "Estimate informed by interpolation between %s and %s levels. ",
  Prec.Year[index], Succ.Year[index])

Itp1.Cov = apply(Anchor.Cov, 2, FUN=na.approx, na.rm=FALSE)
Cov[index] = round(Itp1.Cov)

# % At anchor points
# wuenic_II(C, V, Y, Rule, Expl, Coverage) :-
#     anchor(C, V, Y, Rule0, Expl0, Cov0),
#     !,
#     Rule = Rule0,
#     Expl = Expl0,
#     Coverage = Cov0.

index = !is.na(Anchor.Cov)
Rule[index] = Anchor.Rule[index]
Info[index] = Anchor.Info[index]
Cov[index] = Anchor.Cov[index]

# % Obtain estimates for vaccine coverage. At Level 1, check for working group
# % decisions and work around obvious inconsistencies.
#
# % Estimate for RCV1 where RCV1 given at MCV1
# wuenic_I(C, rcv1, Y, Rule, Expl, Coverage) :-
#   estimate_required(C, rcv1, Y, _, _),
#   !,
#   wuenic_II(C, mcv1, Y, Rule, _, Coverage),
#   Expl = 'Estimate based on estimated MCV1. '.

index = Ereq[, "rcv1"]
Rule[index, "rcv1"] = Rule[index, "mcv1"]
Info[index, "rcv1"] = "Estimate based on estimated MCV1. "
Cov[index, "rcv1"] = Cov[index, "mcv1"]

# % Estimate for RCV1 where RCV1 given at MCV2
# wuenic_I(C, rcv1, Y, Rule, Expl, Coverage) :-
#   estimate_required(C, rcv1, Y, _, FirstRubellaDose),                   ***
#   firstRubellaAtSecondMCV(C, rcv1, Y, FirstRubellaDose),
#   !,
#   wuenic_II(C, mcv2, Y, Rule, _, Coverage),
#   Expl = 'First dose of rubella vaccine given with second dose of measles containing vaccine. Estimate based on MCV2 estimate'.

# Todo: check estimate_required, ***

Rule[, "rcv1"] = Rule[cbind(Yn, Rubella[, "rcv1"])]
Info[, "rcv1"] = ifelse(Rubella[, "rcv1"] == "mcv2", 
  "First dose of rubella vaccine given with second dose of measles containing vaccine. Estimate based on MCV2 estimate",
  "unclear when first dose of rubella vaccine has been given")
Cov[, "rcv1"] = Cov[cbind(Yn, Rubella[, "rcv1"])]

# % If DTP1 not reported: estimate using equation
# % If DTP3 > DTP1 (which is impossible), estimate coverage using equation
# wuenic_I(C, dtp1, Y, Rule, Expl, Coverage) :-
#   wuenic_II(C, dtp3, Y, _, _, DTP3),
#   !,
#   Rule = 'RMF:',
#   concat_atom(['Estimate based on DTP3 coverage of ', DTP3, '. '], Expl),
#   Coverage is round(-0.0058 * DTP3 * DTP3 + 1.3912 * DTP3 + 18.258).

index = !is.na(Cov[, "dtp1"]) & !is.na(Cov[, "dtp3"]) & Cov[, "dtp3"] > Cov[, "dtp1"]
index = index | is.na(Cov[, "dtp1"]) & !is.na(Cov[, "dtp3"])
Rule[index, "dtp1"] = "RMF:"
Info[index, "dtp1"] = sprintf("Estimate based on DTP3 coverage of %s. ",
  Cov[index, "dtp3"])
Cov[index, "dtp1"] =
  round(-0.0058 * Cov[index, "dtp3"]^2 + 1.3912 * Cov[index, "dtp3"] + 18.258)

# % Assigned by working group
# wuenic_I(C, V, Y, Rule, Expl, Coverage) :-
#   decision(C, V, Y, assignWUENIC, Expl0, _, Cov0),
#   !,
#   Rule = 'W:',
#   Expl = Expl0,
#   Coverage = Cov0.

index = Decisions[Decisions$Dec == "assignWUENIC", ]
Rule[cbind(index$Y, index$V)] = "W:"
Info[cbind(index$Y, index$V)] = index$Info
Cov[cbind(index$Y, index$V)] = index$Cov
