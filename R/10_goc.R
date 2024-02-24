wuenic.goc = function(Ereq, Rep.Cov, Svy.Cov, Cov, Rule, Rub, Decisions,
                      Vaccinated, Births, Surviving)
{
  # Confidence in reported coverage if it is an anchor point.
  #
  # Prolog
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
  
  # No confidence in surveys if any survey deviates too much from WUENIC coverage
  #
  # Prolog
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
  
  # Todo: It seems as if surveys are ignored if there is no estimate required.
  # Unclear if intended, since the scope of surveys spans +/- 2 years.
  # Example: arm/pcv3 2015 (Svy.Min from 2013)
  
  # Rolling minimum and maximum
  Svy.Min = Svy.Cov
  index = which(!Ereq, arr.ind=TRUE)
  Svy.Min[cbind(index[, "Y"], index[, "V"])] = NA # here
  Svy.Min = suppressWarnings({
    zoo::rollapply(Svy.Min, width=2 * svy.scope() + 1, partial=1,
                   FUN=min, na.rm=TRUE)
  })
  rownames(Svy.Min) = rownames(Svy.Cov)
  
  Svy.Max = Svy.Cov
  Svy.Max[cbind(index[, "Y"], index[, "V"])] = NA # here
  Svy.Max = suppressWarnings({
    zoo::rollapply(Svy.Max, width=2 * svy.scope() + 1, partial=1,
                   FUN=max, na.rm=TRUE)
  })
  rownames(Svy.Max) = rownames(Svy.Cov)
  
  # Default: S+
  Conf.Svy = YV.char()
  index = which(!is.na(Cov) & Ereq & is.finite(Svy.Min) & is.finite(Svy.Max))
  Conf.Svy[index] = "S+"
  
  # Change to S- if survey deviates from coverage
  index = which(!is.na(Cov) & Ereq & Cov - Svy.Min > svy.thrs())
  Conf.Svy[index] = "S-"
  index = which(!is.na(Cov) & Ereq & Svy.Max - Cov > svy.thrs())
  Conf.Svy[index] = "S-"
  
  # Next criterion, "denominator". Births used for bcg and hepb birth dose.
  # Surviving infants for remaining vaccines.
  #
  # Prolog
  # denominator(C, V, Y, Coverage) :-
  #   member(V, [bcg, hepbb]),
  #   !,
  #   vaccinated0(C, V, Y, Vaccinated),
  #   births_UNPD(C, Y, Births),
  #   Coverage is Vaccinated / Births * 100.
  #
  # denominator(C, V, Y, Coverage) :-
  #   vaccinated0(C, V, Y, Vaccinated),
  #   si_UNPD(C, Y, SI),
  #   Coverage is Vaccinated / SI * 100.
  Den = YV.real()
  index = c("bcg", "hepbb")
  Den[, index] = Vaccinated[, index] / Births * 100
  
  index = setdiff(Vn(), index)
  Den[, index] = Vaccinated[, index] / Surviving * 100

  # Recalculate coverage using reported number of children vaccinated and births
  # and surviving infants from UNPD estimates.
  #
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
  
  # Todo: note the inconsistency with conf_survey() that uses strict inequality
  index = which(abs(Cov - Den) >= unpd.thrs())
  Conf.Den[index] = "D-"

  # Confidence depends on converging evidence from the different sources.
  # 1 star = low confidence, ..., 3 stars = high confidence
  #
  # Low confidence
  # confidence(_C, _V, _Y, Expl, Grade) :-
  #   !,
  #   Expl = 'GoC=No accepted empirical data',
  #   Grade = 1.
  GoC = YV.int(1)
  Expl = YV.char("GoC=No accepted empirical data")
  
  # Confidence in one or two sources, two stars
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
  Expl[index] = "GoC=R+"
  
  index = which(Conf.Svy == "S+")
  GoC[index] = 2
  Expl[index] = "GoC=S+"
  
  index = which(Conf.Den == "D+")
  GoC[index] = 2
  Expl[index] = "GoC=D+"
  
  index = which(Conf.Rep == "R+" & Conf.Svy == "S+")
  GoC[index] = 2
  Expl[index] = "GoC=R+ S+"
  
  index = which(Conf.Svy == "S+" & Conf.Den == "D+")
  GoC[index] = 2
  Expl[index] = "GoC=S+ D+"
  
  index = which(Conf.Rep == "R+" & Conf.Den == "D+")
  GoC[index] = 2
  Expl[index] = "GoC=R+ D+"
  
  # Check if any source is challenged
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
  Chall = YV.char("")
  index = which(Conf.Den == "D-")
  Chall[index] = "D-"
  
  index = which(Conf.Rep == "R-")
  Chall[index] = sprintf("%sR-", Chall[index])
  
  index = which(Conf.Svy == "S-")
  Chall[index] = sprintf("%sS-", Chall[index])
  
  # If any estimate has been challenged, confidence is low
  # confidence(C, V, Y, Expl, Grade) :-
  #   setof(Expl0, challenge(C, V, Y, Expl0), List),
  #   !,
  #   concat_atom(['Estimate challenged by: ' | List], Expl),
  #   Grade = 1.
  index = Chall != ""
  GoC[index] = 1
  Expl[index] = sprintf("Estimate challenged by: %s", Chall[index])
  
  # Confidence in both reported, surveys, and sold vaccines
  # confidence(C, V, Y, Expl, Grade) :-
  #   conf_reported(C, V, Y, 'R+'),
  #   conf_survey(C, V, Y, 'S+'),
  #   conf_denominator(C, V, Y, 'D+'),
  #   !,
  #   Expl = 'GoC=R+ S+ D+',
  #   Grade = 3.
  index = which(Conf.Rep == "R+" & Conf.Svy == "S+" & Conf.Den =="D+")
  GoC[index] = 3
  Expl[index] = "GoC=R+ S+ D+"
  
  # Confidence rated by working group
  # confidence(C, V, Y, Expl, Grade) :-
  #   decision(C, V, Y, assignGoC, Expl0, _, Grade0),
  #   !,
  #   concat_atom(['GoC=Assigned by working group. ', Expl0], Expl),
  #   Grade = Grade0.
  index = Decisions[Decisions$Dec == "assignGoC", ]
  GoC[cbind(index$Y, index$V)] = index$Cov # Cov is GoC here
  Expl[cbind(index$Y, index$V)] =
    sprintf("GoC=Assigned by working group. %s", index$Info)
  
  # Copy rcv1 from mcv2
  #
  # Todo, discuss: this differs from the rule in wuenic_I
  # confidence(C, rcv1, Y, Expl, Grade) :-
  #   estimate_required(C, rcv1, Y, _, mcv2),
  #   !,
  #   confidence(C, mcv2, Y, Expl, Grade).
  index = which(Ereq[, "rcv1"] & Rub[, "rcv1"] == "mcv2")
  GoC[index, "rcv1"] = GoC[index, "mcv2"]
  Expl[index, "rcv1"] = Expl[index, "mcv2"]
  
  # Copy rcv1 from mcv1
  # confidence(C, rcv1, Y, Expl, Grade) :-
  #   estimate_required(C, rcv1, Y, _, na),
  #   !,
  #   confidence(C, mcv1, Y, Expl, Grade).
  index = Ereq[, "rcv1"] & is.na(Rub[, "rcv1"])
  GoC[index, "rcv1"] = GoC[index, "mcv1"]
  Expl[index, "rcv1"] = Expl[index, "mcv1"]
  
  list(GoC=GoC, Expl=Expl)
}
