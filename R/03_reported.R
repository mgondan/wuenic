#' Add information from government and administration
#' 
#' Reported to WHO and UNICEF is government estimate. If government estimate
#' missing, then reported is administrative data. Working group decisions may
#' override this rule.
#'
#' In the following implementation, we start at the lowest level, which is the
#' administrative data. If the government also reports the coverage, the former
#' numbers are overwritten.
#' 
#' @param Admin
#' admin data
#' 
#' @param Gov
#' governemt data
#' 
#' @param Decisions
#' List of work group decisions
#' 
#' @return 
#' List with coverage and source, and some explanation
#'
wuenic.reported = function(Admin, Gov, Decisions)
{
  # This was the corresponding Prolog code
  # reported(C, V, Y, Source, Coverage) :-
  #     admin0(C, V, Y, Cov0),
  #     (   decision(C, V, Y, ignoreGov, _, _, _)
  #     ;   not(gov(C, V, Y, _))
  #     ),
  #     not(decision(C, V, Y, ignoreAdmin, _, _, _)),
  #     !,
  #     Source = admin,
  #     Coverage = Cov0.
  
  Cov = Admin
  Src = ifelse(is.na(Admin), NA, "admin")
  
  # If the working group decides to ignore the admin data, revert back to NA.
  ignore = (Decisions$Dec == "ignoreAdmin")
  index = cbind(Decisions$Y[ignore], Decisions$V[ignore])
  Cov[index] = NA
  Src[index] = NA
  
  # If we have valid government data, use the latter.
  #
  # reported(C, V, Y, Source, Coverage) :-
  #    gov(C, V, Y, Cov0),
  #    not(decision(C, V, Y, ignoreGov, _, _, _)),
  #    !,
  #    Source = gov,
  #    Coverage = Cov0.
  
  index = !is.na(Gov)
  ignore = (Decisions$Dec == "ignoreGov")
  index[cbind(Decisions$Y[ignore], Decisions$V[ignore])] = FALSE
  # Cov[index] = Gov[index]
  # Src[index] = "gov"
  Cov[index] = NA
  Src[index] = NA
  
  index = !is.na(Gov)
  ignore = (Decisions$Dec == "ignoreGov")
  index[cbind(Decisions$Y[ignore], Decisions$V[ignore])] = FALSE
  Cov[index] = Gov[index]
  Src[index] = "gov"
  
  # explanation(C, V, Y, Expl) :-
  #     decision(C, V, Y, acceptReported, Expl, _, _).
  Expl = YV.char("")
  index = Decisions[Decisions$Dec == "acceptReported", ]
  if(nrow(index))
    index = aggregate(list(Info=index$Info), 
      list(Y=index$Y, V=index$V), FUN=paste, collapse="")
  Expl[cbind(index$Y, index$V)] = index$Info
  
  # explanation(C, V, Y, Expl) :-
  #     decision(C, V, Y, ignoreGov, Expl, _, _).
  index = Decisions[Decisions$Dec == "ignoreGov", ]
  if(nrow(index))
    index = aggregate(list(Info=index$Info),
      list(Y=index$Y, V=index$V), FUN=paste, collapse="")
  Expl[cbind(index$Y, index$V)] = 
    sprintf("%s%s", Expl[cbind(index$Y, index$V)], index$Info)
  
  list(Cov=Cov, Src=Src, Expl=Expl)
}