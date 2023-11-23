library(rolog)
consult("xsb/bgd.pl")

Vn = c("bcg", "dtp1", "dtp3", "hepb3", "hib3", "ipv1", "mcv1", "mcv2", "pcv3",
  "pol1", "pol3", "rcv1")
Yn = 1997:2022
sawtooth = 10

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
  
  Info = matrix(NA_character_, nrow=length(Yn), ncol=length(Vn), 
    dimnames=list(Yn, Vn))
  Info[cbind(as.character(s$Y), s$V)] = s$Info.title
  # more to come
  Cov = matrix(NA_integer_, nrow=length(Yn), ncol=length(Vn), 
    dimnames=list(Yn, Vn))
  Cov[cbind(as.character(s$Y), s$V)] = s$Cov
  list(Info=Info, Cov=Cov)
}

decisions = function()
{
  q = call("wgd", expression(C), expression(V), expression(Y0), expression(Y1),
           expression(Dec), expression(Info), 
           expression(A1), expression(Cov), expression(A3), expression(A4))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, padNA,
             v=c("C", "V", "Y0", "Y1", "Dec", "Info", "A1", "Cov", "A3", "A4"))
  s = lapply(s, as.data.frame)

  # Handle V = NA
  s = do.call("rbind", s)
  s = apply(s, MARGIN=1, simplify=FALSE,
    FUN=function(d)
      if(is.na(d['V']))
        data.frame(V=Vn, Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'], Info=d['Info'], Cov=d['Cov'], row.names=NULL)
      else 
        data.frame(V=d['V'], Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'], Info=d['Info'], Cov=d['Cov']))
  
  # Handle Year range
  s = do.call("rbind", s)
  s = apply(s, MARGIN=1, simplify=FALSE,
    FUN=function(d)
      data.frame(V=d['V'], Y=d['Y0']:min(2022, d['Y1']), Dec=d['Dec'], Info=d['Info'], Cov=d['Cov'], row.names=NULL))
  s = do.call("rbind", s)

  Dec = matrix("", nrow=length(Yn), ncol=length(Vn), 
    dimnames=list(Yn, Vn))
  Dec[cbind(as.character(s$Y), s$V)] = s$Dec
  Info = matrix(NA_character_, nrow=length(Yn), ncol=length(Vn),
    dimnames=list(Yn, Vn))
  Info[cbind(as.character(s$Y), s$V)] = s$Info
  Cov = matrix(NA_integer_, nrow=length(Yn), ncol=length(Vn), 
    dimnames=list(Yn, Vn))
  Cov[cbind(as.character(s$Y), s$V)] = s$Cov
  list(Dec=Dec, Info=Info, Cov=Cov)
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
index = index & (wgd$Dec == "ignoreGov" | is.na(gov))
index = index & wgd$Dec != "ignoreAdmin"
reported[index] = adm[index]
source[index] = "adm"

# Fill with gov data
index = !is.na(gov)
index = index & wgd$Dec != "ignoreGov"
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

index = wgd$Dec == "acceptReported"
rep_rejected[index] = FALSE

index = wgd$Dec == "ignoreReported"
rep_rejected[index] = TRUE

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
rep_ts[index] = itp[index]
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
