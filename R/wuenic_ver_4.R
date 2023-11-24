library(rolog)
consult("xsb/bgd.pl")

Vn = c("bcg", "dtp1", "dtp3", "hepb3", "hib3", "ipv1", "mcv1", "mcv2", "pcv3",
  "pol1", "pol3", "rcv1")
Yn = 1997:2022

sawtooth = 10
svy.threshold = 10

# Change atoms to character strings
# Change list elements of type name:elem to named list elements
atom2char = function(q)
{
  if(is.expression(q))
    return(as.character(q))
  
  if(is.symbol(q))
    return(as.character(q))
  
  if(is.call(q))
  {
    args <- as.list(q)
    args[-1] <- lapply(args[-1], FUN=atom2char)
    return(as.call(args))
  }

  if(is.list(q) && !is.null(names(q)))
    return(lapply(q, atom2char))
  
  if(is.list(q))
  {
    n = NULL
    for(i in 1:length(q))
    {
      item = as.list(q[[i]])
      q[[i]] = atom2char(item[[3]])
      n = c(n, atom2char(item[[2]]))
    }
    names(q) = n
    return(q)
  }
  
  return(q)
}

padNA = function(q, v)
{
  qq = q[v]
  names(qq) = v
  qq[] = ifelse(lapply(qq, is.null), NA, qq)
  return(qq)
}

once1 = function(pred="date")
{
  s = once(call(pred, expression(Date)))
  atom2char(s$Date)
}

once2 = function(pred="country")
{
  s = once(call(pred, expression(Code), expression(Name)))
  lapply(s, atom2char)
}

rep3 = function(pred="births_UNPD")
{
  q = call(pred, expression(C), expression(Y), expression(Children))
  
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  
  m = numeric(length(Yn))
  names(m) = Yn
  m[] = NA_real_
  m[as.character(s$Y)] = s$Children
  return(m)
}

rep4 = function(pred="admin")
{
  q = call(pred, expression(C), expression(V), expression(Y), expression(Cov))

  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  index = which(!(s$V %in% Vn))
  if(length(index))
  {
    warning("Unknown vaccine name(s): ", 
      paste(levels(as.factor(s$V[index])), collapse=", "))
    s = s[-index, ]    
  }
    
  m = matrix(NA_integer_, nrow=length(Yn), ncol=length(Vn), 
    dimnames=list(Yn, Vn))
  m[cbind(as.character(s$Y), s$V)] = s$Cov
  return(m)
}

estimate_required = function()
{
  q = call("estimate_required", expression(C), expression(V), expression(Y), 
           expression(Comb), expression(A5))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)

  m = matrix(FALSE, nrow=length(Yn), ncol=length(Vn), dimnames=list(Yn, Vn))
  m[cbind(as.character(s$Y), s$V)] = TRUE
  return(m)
}

survey_results = function()
{
  q = call("survey_results", expression(C), expression(V), expression(Y), 
           expression(Id), expression(Info), expression(Cov))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  index = which(!(s$V %in% Vn))
  if(length(index))
  {
    warning("Unknown vaccine name(s): ", 
      paste(levels(as.factor(s$V[index])), collapse=", "))
    s = s[-index, ]    
  }

  return(s)
}

decisions = function()
{
  q = call("wgd", expression(C), expression(V), expression(Y0), expression(Y1),
           expression(Dec), expression(Info), 
           expression(A1), expression(Cov), expression(A3), expression(A4))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, padNA,
             v=c("C", "V", "Y0", "Y1", "Dec", "Info", "Id", "Cov", "A3", "A4"))
  s = lapply(s, as.data.frame)

  # Handle V = NA
  s = do.call("rbind", s)
  s = apply(s, MARGIN=1, simplify=FALSE,
    FUN=function(d)
      if(is.na(d['V']))
        data.frame(V=Vn, Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'], Id=d['Id'], Info=d['Info'], Cov=d['Cov'], row.names=NULL)
      else 
        data.frame(V=d['V'], Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'], Id=d['Id'], Info=d['Info'], Cov=d['Cov']))
  
  # Handle Year range
  s = do.call("rbind", s)
  s = apply(s, MARGIN=1, simplify=FALSE,
    FUN=function(d)
      data.frame(V=d['V'], Y=d['Y0']:min(2022, d['Y1']), Dec=d['Dec'], Id=d['Id'], Info=d['Info'], Cov=d['Cov'], row.names=NULL))

  # Used mainly for indexing
  s = do.call("rbind", s)
  s$Cov = as.integer(s$Cov)
  s$Y = as.character(s$Y)
  return(s)
}

# Read country file
country = once2("country")
code = country$Code
country = country$Name
date = once1("date")

est = estimate_required()
adm = rep4("admin")
gov = rep4("gov")
lgc = rep4("legacy")
vac = rep4("vaccinated")
tgt = rep4("target")
bth = rep3("births_UNPD")
svi = rep3("si_UNPD")
svy = survey_results()
wgd = decisions()

# % Reported to WHO and UNICEF is government estimate. If government
# % estimate missing, then reported is administrative data. If both
# % missing, fail.
# reported(C, V, Y, Source, Coverage) :-
#    gov(C, V, Y, Cov0),
#    not(decision(C, V, Y, ignoreGov, _, _, _)),
#    !,
#    Source = gov,
#    Coverage = Cov0.
#
# reported(C, V, Y, Source, Coverage) :-
#     admin0(C, V, Y, Cov0),
#     (   decision(C, V, Y, ignoreGov, _, _, _)
#     ;   not(gov(C, V, Y, _))
#     ),
#     not(decision(C, V, Y, ignoreAdmin, _, _, _)),
#     !,
#     Source = admin,
#     Coverage = Cov0.

reported = matrix(NA_real_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))

source = matrix(NA_character_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))

# Fill with admin data
index = !is.na(adm)
gov.na = is.na(gov)
gov.na[cbind(wgd$Y[wgd$Dec == "ignoreGov"], wgd$V[wgd$Dec == "ignoreGov"])] = TRUE
index = index & gov.na
index[cbind(wgd$Y[wgd$Dec == "ignoreAdmin"], wgd$V[wgd$Dec == "ignoreAdmin"])] = FALSE
reported[index] = adm[index]
source[index] = "adm"

# Fill with gov data
index = !is.na(gov)
index[cbind(wgd$Y[wgd$Dec == "ignoreGov"], wgd$V[wgd$Dec == "ignoreGov"])] = FALSE
reported[index] = gov[index]
source[index] = "gov"

# % Reasons to exclude reported data are:
# % * Working group decision.
# % * Coverage > 100%
# % * Inconsistent temporal changes (sawtooth or sudden change)
#
# % Rejection dominates accepted, see below
# reported_rejected(C, V, Y) :-
#     decision(C, V, Y, ignoreReported, _Expl0, _, _),
#     !.
# % See above
# reported_rejected(C, V, Y) :-
#     decision(C, V, Y, acceptReported, _, _, _),
#     !,
#     fail.
#
# % Implausible coverage
# reported_rejected(C, V, Y) :-
#     reported(C, V, Y, _, Coverage),
#     Coverage > 100,
#     !.
#
# % Sudden jumps
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
#
# % Sudden change in most recently reported data for classic vaccines, or
# % sudden decline in most recently reported data for new vaccines
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

# Todo: Set reported directly to NA instead
rep_rejected = matrix(FALSE, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))

index = !apply(reported, 2, is.na)
index = apply(index, 2, which)
index = lapply(index, rev)
index = sapply(index, "[", 1)
index = cbind(index, 1:length(Vn))
index = index[!(Vn %in% c("pcv3", "rotac")), , drop=FALSE]
index = na.exclude(index)
jump = apply(rbind(NA, reported), 2, diff)
rep_rejected[index] = abs(jump[index]) > sawtooth

index = !apply(reported, 2, is.na)
index = apply(index, 2, which)
index = lapply(index, rev)
index = sapply(index, "[", 1)
index = cbind(index, 1:length(Vn))
index = index[Vn %in% c("pcv3", "rotac"), , drop=FALSE]
index = na.exclude(index)
jump = apply(rbind(NA, reported), 2, diff)
rep_rejected[index] = jump[index] < -sawtooth

up = rbind(0, diff(reported)) > sawtooth
down = rbind(diff(reported), 0) < -sawtooth
index = which(up & down, arr.ind=TRUE)
rep_rejected[index] = TRUE

down = rbind(0, diff(reported)) < -sawtooth
up = rbind(diff(reported), 0) > sawtooth
index = which(down & up, arr.ind=TRUE)
rep_rejected[index] = TRUE

index = !is.na(reported)
index = index & reported > 100
rep_rejected[index] = TRUE

index = wgd[wgd$Dec == "acceptReported", c("Y", "V")]
rep_rejected[as.matrix(index)] = FALSE

index = wgd[wgd$Dec == "ignoreReported", c("Y", "V")]
rep_rejected[as.matrix(index)] = TRUE

# % Time series of reported data
# %
# % Reported data available
# reported_time_series(C, V, Y, Source, Coverage) :-
#     estimate_required(C, V, Y, _, _),
#     reported(C, V, Y, Source0, Cov0),
#     not(reported_rejected(C, V, Y)),
#     !,
#     Source = Source0,
#     Coverage = Cov0.
#
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
#
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

rep_ts = matrix(NA_integer_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))
rep_src = matrix(NA_character_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))

itp = reported
itp[rep_rejected] = NA
itp = apply(itp, 2, zoo::na.approx, na.rm=FALSE)
index = est
index = index & (is.na(reported) | rep_rejected)
index = index & !is.na(itp)
rep_ts[index] = itp[index]                   # todo: consider rounding numbers
rep_src[index] = "interpolated"

etp = apply(itp, 2, zoo::na.locf, na.rm=FALSE)
etp = apply(etp, 2, zoo::na.locf, na.rm=FALSE, fromLast=TRUE)
index = est
index = index & (is.na(reported) | rep_rejected)
index = index & is.na(itp) & !is.na(etp)
rep_ts[index] = etp[index]
rep_src[index] = "extrapolated"

index = est
index = index & !is.na(reported)
index = index & !rep_rejected
rep_ts[index] = reported[index]
rep_src[index] = source[index]

# % Survey results passed for inclusion in the analysis include:
# % card or history results for cohorts 12-23, 18-29, 15-26, 24-35 months
# % of age
# survey_for_analysis(C, V, Y, ID, Description, Coverage) :-
#     survey_results0(C, V, Y, ID, Description, Coverage),
#     member(confirm:'card or history', Description),
#     member(age:AgeCohort, Description),
#     member(AgeCohort, ['12-23 m', '18-29 m', '15-26 m', '24-35 m']).
index = svy$Info.confirm == "card or history"
index = index & svy$Info.age %in% c("12-23 m", "18-29 m", "15-26 m", "24-35 m")
svy.ana = svy[index, ]

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

index = svy.ana$Info.ss >= 300
index = index & !(svy.ana$Id %in% wgd$Id[wgd$Dec == "ignoreSurvey"])
index = index & !(svy.ana$Y %in% wgd$Y[wgd$Dec == "ignoreSurvey" & is.na(wgd$Id)])
index = index | svy.ana$Id %in% wgd$Id[wgd$Dec == "acceptSurvey"]

# Todo: modify survey (recall bias etc.)

svy.ana = svy.ana[index, ]

# % Survey information for given year. Multiple surveys are averaged.
# survey(C, V, Y, Expl, Coverage) :-
#     findall(Cov, survey_accepted(C, V, Y, _, Cov), [H | T]),
#     length([H | T], N),
#     sum_list([H | T], Sum),
#     Coverage is round(Sum / N),
#     concat_atom(['Survey evidence of ', Coverage, ' percent based on ',
#         N, ' survey(s). '], Expl).

svy.cov = matrix(NA_integer_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))
i.cov = with(svy.ana, aggregate(Cov, by=list(Y=Y, V=V), FUN=mean))
svy.cov[cbind(i.cov$Y, i.cov$V)] = i.cov$x

svy.inf = matrix(NA_character_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))
i.inf = with(svy.ana, aggregate(Cov, by=list(Y=Y, V=V), FUN=length))
svy.inf[cbind(i.inf$Y, i.inf$V)] = 
  sprintf("Survey evidence of %s percent based on %i survey(s). ",
          svy.cov[cbind(i.inf$Y, i.inf$V)], i.inf$x)

# % Determine coverage value at anchor points defined as years where there
# % are multiple data points (reported | survey | wgd).
# %
# % Reported value "anchored" by working group
# anchor(C, V, Y, Rule, Expl, Coverage) :-
#     reported_time_series(C, V, Y, _, Cov0),
#     decision(C, V, Y, assignAnchor, Expl0, _, Cov0), % same Cov0
#     !,
#     Rule = 'R: AP',
#     Expl = Expl0,
#     Coverage = Cov0.
#
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
#
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
#
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

anchor.rul = matrix(NA_character_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))
anchor.inf = matrix(NA_character_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))
anchor.cov = matrix(NA_integer_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))

index = abs(svy.cov - rep_ts) > svy.threshold
index[is.na(index)] = FALSE
anchor.rul[index] = "S: AP"

anchor.inf[index] = 
  paste("Survey evidence does not support reported data. Estimate based on survey results.", 
        svy.inf[index], collapse=" ")
anchor.cov[index] = svy.cov[index]

index = abs(svy.cov - rep_ts) <= svy.threshold
index[is.na(index)] = FALSE
anchor.rul[index] = "R: AP"

info = c(
  gov="Estimate informed by reported data supported by survey. ",
  admin="Estimate informed by reported administrative data supported by survey. ",
  interpolated="Estimate informed by interpolation between reported data supported by survey. ",
  extrapolated="Estimate based on extrapolation from data reported by national government supported by survey. ")

anchor.inf[index] = paste(info[source[index]], svy.inf[index], collapse=" ")
anchor.cov[index] = rep_ts[index]

index = wgd[wgd$Dec == "assignAnchor", c("Y", "V", "Cov", "Info")]

anchor = matrix(NA_integer_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))
anchor[cbind(index$Y, index$V)] = round(index$Cov)

info = matrix(NA_character_, nrow=nrow(est), ncol=ncol(est),
  dimnames=dimnames(est))
info[cbind(index$Y, index$V)] = index$Info

index = anchor == rep_ts
index[is.na(index)] = FALSE
anchor.rul[index] = "R: AP"
anchor.inf[index] = info[index]
anchor.cov[index] = anchor[index]

index = anchor != rep_ts
index[is.na(index)] = FALSE
anchor.rul[index] = "W: AP"
anchor.inf[index] =
  sprintf("Estimate of %g percent assigned by working group. ", anchor[index])
anchor.cov[index] = anchor[index]

