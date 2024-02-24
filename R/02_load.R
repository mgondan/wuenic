# 10 km of code that import the country-specific information from the
# Prolog file
#
wuenic.load = function(fname = "data.pl")
{
  # load_files("data.pl", [encoding(iso_latin_1)])
  rolog::once(call("load_files", fname,
    list(call("encoding", quote(iso_latin_1)))))
  
  # country(Code, Country)
  s = rolog::once(call("country", expression(Code), expression(Country)))
  s = atom2char(s)
  Code = s$Code
  Country = s$Country
  
  # date(Date)
  s = rolog::once(call("date", expression(Date)))
  Date = atom2char(s$Date)
  
  # Store all entries with estimate_required(_, V, Y, _, _)
  Ereq = YV.bool()
  q = call("estimate_required", expression(C), expression(V), expression(Y),
           expression(A4), expression(A5))
  s = rolog::findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  s$Y = as.character(s$Y) # Year is heavily used for indexing
  
  # Warn if new vaccines are introduced that are not in the list
  #
  # Todo: Generate Vn on the fly, see above
  index = which(!(s$V %in% Vn()))
  if(length(index))
  {
    warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
    s = s[-index, ]
  }
  Ereq[cbind(s$Y, s$V)] = TRUE
  
  # Rubella is often given in combination with another vaccine. Information
  # about this is stored in estimate_required(_, V, Y, _, Rub)
  q = call("estimate_required", expression(C), expression(V), expression(Y), 
           expression(A4), expression(Rub))
  s = rolog::findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  s$Y = as.character(s$Y)
  
  Rub = YV.char()
  Rub[cbind(s$Y, s$V)] = s$Rub
  
  # Default setting
  #
  # firstRubellaAtSecondMCV(_C, rcv1, _Y, mcv2).
  firstRubellaAtSecondMCV = YV.char()
  firstRubellaAtSecondMCV[, "rcv1"] = "mcv2"
  
  # Vaccination data reported from administration
  #
  # admin(C, V, Y, Admin)
  Admin = YV.int()
  q = call("admin", as.name(Code), expression(V), expression(Y), expression(Adm))
  s = rolog::findall(q)
  if(length(s))
  {
    s = lapply(s, atom2char)
    s = lapply(s, as.data.frame)
    s = do.call("rbind", s)
    s$Y = as.character(s$Y)
    
    index = which(!(s$V %in% Vn()))
    if(length(index))
    {
      warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
      s = s[-index, ]
    }
    
    Admin[cbind(s$Y, s$V)] = s$Adm
  }
  
  # Vaccination data reported from government
  #
  # gov(C, V, Y, Gov)
  Gov = YV.int()
  q = call("gov", as.name(Code), expression(V), expression(Y), expression(Gov))
  s = findall(q)
  if(length(s))
  {
    s = lapply(s, atom2char)
    s = lapply(s, as.data.frame)
    s = do.call("rbind", s)
    s$Y = as.character(s$Y)
    
    index = which(!(s$V %in% Vn()))
    if(length(index))
    {
      warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
      s = s[-index, ]
    }
    
    Gov[cbind(s$Y, s$V)] = s$Gov
  }
  
  # Numbers from earlier Wuenic algorithms. This is needed to check if the
  # numbers are consistent across Wuenic versions.
  #
  # legacy(C, V, Y, Legacy)
  Legacy = YV.int()
  q = call("legacy", as.name(Code), expression(V), expression(Y), expression(Leg))
  s = findall(q)
  if(length(s))
  {
    s = lapply(s, atom2char)
    s = lapply(s, as.data.frame)
    s = do.call("rbind", s)
    s$Y = as.character(s$Y)
    
    index = which(!(s$V %in% Vn()))
    if(length(index))
    {
      warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
      s = s[-index, ]
    }
    
    Legacy[cbind(s$Y, s$V)] = s$Leg
  }
  
  # Some general statistics that are used for confidence grading
  #
  # vaccinated(C, V, Y, Vaccinated)
  Vaccinated = YV.int()
  q = call("vaccinated", 
           as.name(Code), expression(V), expression(Y), expression(Vac))
  s = findall(q)
  if(length(s))
  {
    s = lapply(s, atom2char)
    s = lapply(s, as.data.frame)
    s = do.call("rbind", s)
    s$Y = as.character(s$Y)
    
    index = which(!(s$V %in% Vn()))
    if(length(index))
    {
      warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
      s = s[-index, ]
    }
    
    Vaccinated[cbind(s$Y, s$V)] = s$Vac
  }
  
  # Some general statistics that are used for confidence grading
  #
  # target(C, V, Y, Target)
  Target = YV.int()
  q = call("target", 
           as.name(Code), expression(V), expression(Y), expression(Targ))
  s = findall(q)
  if(length(s))
  {
    s = lapply(s, atom2char)
    s = lapply(s, as.data.frame)
    s = do.call("rbind", s)
    s$Y = as.character(s$Y)
    
    index = which(!(s$V %in% Vn()))
    if(length(index))
    {
      warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
      s = s[-index, ]
    }
    
    Target[cbind(s$Y, s$V)] = s$Targ
  }
  
  # births_UNPD(C, V, Y, Births)
  q = call("births_UNPD", expression(C), expression(Y), expression(Births))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  s$Y = as.character(s$Y)
  
  Births = rep(NA, length(Yn()))
  names(Births) = Yn()
  Births[s$Y] = s$Births
  Births = Births[as.character(Yn())] # drop years outside range
  
  # si_UNPD(C, V, Y, Surviving)
  q = call("si_UNPD", expression(C), expression(Y), expression(Surv))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  s$Y = as.character(s$Y)
  
  Surviving = rep(NA, length(Yn()))
  names(Surviving) = Yn()
  Surviving[s$Y] = s$Surv
  Surviving = Surviving[as.character(Yn())]
  
  # Multiple surveys per vaccine * year combination, therefore no YV-matrix,
  # but a long data frame
  #
  # survey_results(C, V, Y, Id, Info, Cov)
  Survey = data.frame(V=NULL, Y=NULL, Yn=NULL, Id=NULL, Info=NULL, Cov=NULL)
  q = call("survey_results", as.name(Code), expression(V), expression(Y), 
           expression(Id), expression(Info), expression(Cov))
  s = findall(q)
  if(length(s))
  {
    s = lapply(s, atom2char)
    s = lapply(s, as.data.frame)
    s = do.call("rbind", s)
    s$Yn = as.character(s$Y)
    
    index = which(!(s$V %in% Vn()))
    if(length(index))
    {
      warning("Unknown vaccine(s): ", paste(unique(s$V[index]), collapse=", "))
      s = s[-index, ]    
    }
    
    Survey = s
  }
  
  # Multiple decisions per year * vaccine, therefore no YV-matrix, but a long
  # data frame
  #
  # wgd(C, V, Year0, Year1, Dec, Info, Id, Cov, _, _)
  q = call("wgd", expression(C), expression(V), expression(Y0), expression(Y1),
           expression(Dec), expression(Info),
           expression(Id), expression(Cov), expression(A3), expression(A4))
  s = findall(q)
  s = lapply(s, atom2char)
  s = lapply(s, padNA,
             v=c("C", "V", "Y0", "Y1", "Dec", "Info", "Id", "Cov", "A3", "A4"))
  s = lapply(s, as.data.frame)
  s = do.call("rbind", s)
  
  # Fix strange year ranges
  s$Y0 = pmax(s$Y0, min(Yn()))
  s$Y1 = pmin(s$Y1, max(Yn()))
  
  # V = NA means that a decision applies to all vaccines
  V.na = function(d)
  {
    if(is.na(d['V']))
      return(data.frame(V=Vn(), Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'], 
        Id=d['Id'], Info=d['Info'], Cov=d['Cov'], row.names=NULL))
    
    data.frame(V=d['V'], Y0=d['Y0'], Y1=d['Y1'], Dec=d['Dec'],
      Id=d['Id'], Info=d['Info'], Cov=d['Cov'])
  }
  
  s = apply(s, MARGIN=1, simplify=FALSE, FUN=V.na)
  s = do.call("rbind", s)
  
  # If a year range is given, apply decision to each included year
  Y.range = function(d)
  {
    data.frame(V=d['V'], Y=d['Y0']:min(max(Yn()), d['Y1']),
      Dec=d['Dec'], Id=d['Id'], Info=d['Info'], Cov=d['Cov'], row.names=NULL)
  }
  
  s = apply(s, MARGIN=1, simplify=FALSE, FUN=Y.range)
  s = do.call("rbind", s)
  
  # For "ignore survey", the Id is sometimes NA, meaning that it applies to all
  # surveys of a given year and vaccine
  Id.na = function(d, Idn)
  {
    if(d['Dec'] == "ignoreSurvey" & is.na(d['Id']))
      return(data.frame(V=d['V'], Y=d['Y'], Dec=d['Dec'], 
        Id=Idn, Info=d['Info'], Cov=d['Cov'], row.names=NULL))
    
    data.frame(V=d['V'], Y=d['Y'], Dec=d['Dec'],
      Id=d['Id'], Info=d['Info'], Cov=d['Cov'])
  }
  
  s = apply(s, MARGIN=1, simplify=FALSE, FUN=Id.na, Idn=unique(Survey$Id))
  s = do.call("rbind", s)
  
  s$Cov = as.integer(s$Cov)
  s$Y = as.character(s$Y)
  Decisions = s

  list(Country=Country, Code=Code, Date=Date,
    Ereq=Ereq, Rub=Rub, firstRubellaAtSecondMCV=firstRubellaAtSecondMCV,
    Legacy=Legacy, Gov=Gov, Admin=Admin,
    Vaccinated=Vaccinated, Target=Target, Surviving=Surviving, Births=Births,
    Survey=Survey, Decisions=Decisions)
}
