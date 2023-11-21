library(rolog)
consult("xsb/bgd.pl")

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
  
  Yn = 1997:2022
  Vn = levels(as.factor(s$V))
  m = matrix(FALSE, nrow=length(Yn), ncol=length(Vn), dimnames=list(Yn, Vn))
  m[cbind(as.character(s$Y), s$V)] = s$Children
  return(m)
}

rep4 = function(pred="vaccinated")
{
  q = call(pred, expression(C), expression(V), expression(Y), expression(Cov))

  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  
  Yn = 1997:2022
  Vn = levels(as.factor(s$V))
  m = matrix(NA_integer_, nrow=length(Yn), ncol=length(Vn), dimnames=list(Yn, Vn))
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

  Yn = 1997:2022
  Vn = levels(as.factor(s$V))
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

  Yn = 1997:2022
  Vn = levels(as.factor(s$V))
  Info = matrix(NA_character_, nrow=length(Yn), ncol=length(Vn), 
    dimnames=list(Yn, Vn))
  Info[cbind(as.character(s$Y), s$V)] = s$Info.title
  # more to come
  Cov = matrix(NA_integer_, nrow=length(Yn), ncol=length(Vn), 
    dimnames=list(Yn, Vn))
  Cov[cbind(as.character(s$Y), s$V)] = s$Cov
  list(Info, Cov)
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
  Vn = levels(as.factor(s$V))
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

  Yn = 1997:2022
  Vn = levels(as.factor(s$V))
  Dec = matrix("", nrow=length(Yn), ncol=length(Vn), dimnames=list(Yn, Vn))
  Dec[cbind(as.character(s$Y), s$V)] = s$Dec
  Info = matrix("", nrow=length(Yn), ncol=length(Vn), dimnames=list(Yn, Vn))
  Info[cbind(as.character(s$Y), s$V)] = s$Info
  Cov = matrix(0L, nrow=length(Yn), ncol=length(Vn), dimnames=list(Yn, Vn))
  Cov[cbind(as.character(s$Y), s$V)] = s$Cov
  list(Dec, Info, Cov)
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
