#' Estimate coverage based on all information
#' 
#' Distinguish different cases
#'
#' * at anchor point
#' * between anchor points
#' * before anchor point
#' * after anchor point
#' * no anchor points
#' 
#' @param Ereq
#' Matrix with booleans, is an estimate required?
#' 
#' @param Rule
#' Decision rule (accept reported etc.)
#' 
#' @param Info
#' Additional information
#' 
#' @param Cov
#' Coverage estimate
#' 
#' @param Rub
#' Where to obtain the data for the rubella vaccination
#' 
#' @param firstRubellaAtSecondMCV
#' Where to obtain the data for the rubella vaccination (default)
#'
#' @param Decisions
#' List of work group decisions
#' 
#' @return 
#' List with coverage (incl. bounded 0...99) and source, and some explanation
#'
wuenic.top = function(Ereq, Rule, Info, Cov, Rub, firstRubellaAtSecondMCV, Decisions)
{
  # Obtain estimates for vaccine coverage. At Level 1, check for working group
  # decisions and work around obvious inconsistencies.
  #
  # Estimate for RCV1 where RCV1 given at MCV1
  # wuenic_I(C, rcv1, Y, Rule, Expl, Coverage) :-
  #   estimate_required(C, rcv1, Y, _, _),
  #   !,
  #   wuenic_II(C, mcv1, Y, Rule, _, Coverage),
  #   Expl = 'Estimate based on estimated MCV1. '.
  
  index = Ereq[, "rcv1"]
  Rule[index, "rcv1"] = Rule[index, "mcv1"]
  Info[index, "rcv1"] = "Estimate based on estimated MCV1. "
  Cov[index, "rcv1"] = Cov[index, "mcv1"]
  
  # Estimate for RCV1 where RCV1 given at MCV2
  # wuenic_I(C, rcv1, Y, Rule, Expl, Coverage) :-
  #     estimate_required(C, rcv1, Y, _, FirstRubellaDose),
  #     firstRubellaAtSecondMCV(C, rcv1, Y, FirstRubellaDose),
  #     !,
  #     wuenic_II(C, mcv2, Y, Rule, _, Coverage),
  #     Expl = 'First dose of rubella vaccine given with second dose of measles containing vaccine. Estimate based on MCV2 estimate'.
  
  index = Rub[, "rcv1"] == firstRubellaAtSecondMCV[, "rcv1"]
  index = as.character(Yn()[which(index)])
  if(length(index))
  {
    Rule[cbind(index, "rcv1")] = Rule[cbind(index, Rub[, "rcv1"][index])]
    Info[cbind(index, "rcv1")] = "First dose of rubella vaccine given with second dose of measles containing vaccine. Estimate based on MCV2 estimate"
    Cov[cbind(index, "rcv1")] = Cov[cbind(index, Rub[, "rcv1"][index])]
  }
  
  # If DTP1 not reported: estimate using equation
  # If DTP3 > DTP1 (which is impossible), estimate coverage using equation
  # wuenic_I(C, dtp1, Y, Rule, Expl, Coverage) :-
  #     wuenic_II(C, dtp3, Y, _, _, DTP3),
  #     !,
  #     Rule = 'RMF:',
  #     concat_atom(['Estimate based on DTP3 coverage of ', DTP3, '. '], Expl),
  #     Coverage is round(-0.0058 * DTP3 * DTP3 + 1.3912 * DTP3 + 18.258).
  
  index = !is.na(Cov[, "dtp1"]) & !is.na(Cov[, "dtp3"]) &
    Cov[, "dtp3"] > Cov[, "dtp1"]
  index = index | (is.na(Cov[, "dtp1"]) & !is.na(Cov[, "dtp3"]))
  Rule[index, "dtp1"] = "RMF:"
  Info[index, "dtp1"] = sprintf("Estimate based on DTP3 coverage of %.0f. ",
                                Cov[index, "dtp3"])
  Cov[index, "dtp1"] =
    tround(-0.0058 * Cov[index, "dtp3"]^2 + 1.3912 * Cov[index, "dtp3"] + 18.258)
  
  # Assigned by working group
  # wuenic_I(C, V, Y, Rule, Expl, Coverage) :-
  #     decision(C, V, Y, assignWUENIC, Expl0, _, Cov0),
  #     !,
  #     Rule = 'W:',
  #     Expl = Expl0,
  #     Coverage = Cov0.
  
  index = Decisions[Decisions$Dec == "assignWUENIC", ]
  Rule[cbind(index$Y, index$V)] = "W:"
  Info[cbind(index$Y, index$V)] = index$Info
  Cov[cbind(index$Y, index$V)] = index$Cov
  
  # Force between 0 and 99%
  #
  # bound_0_100(X, Y) :-
  #   Y is max(0, min(99, round(X))).
  Bounded = Cov
  Bounded[] = pmax(0, pmin(99, tround(Bounded)))
  
  list(Cov=Cov, Bounded=Bounded, Rule=Rule, Info=Info)
}