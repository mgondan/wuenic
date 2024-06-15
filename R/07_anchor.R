#' Integrate estimates from reported data and surveys
#' 
#' Define the according anchor points (reported | survey | wgd)
#' 
#' @param TS.Cov
#' Coverage from time series
#' 
#' @param TS.Src
#' Source of the coverage from time series
#' 
#' @param Svy.Cov
#' Coverage from surveys
#' 
#' @param Svy.Expl
#' Explanation derived from surveys
#'
#' @param Decisions
#' List of work group decisions
#' 
#' @return 
#' List with Coverage, Rule/Source, and some explanation (info)
#'
wuenic.anchor = function(TS.Cov, TS.Src, Svy.Cov, Svy.Expl, Decisions)
{
  Rule = YV.char()
  Info = YV.char()
  Cov = YV.int()
  
  # If survey results challenge the reported coverage, the survey data is used.
  #
  # Prolog
  # anchor(C, V, Y, Rule, Expl, Coverage) :-
  #     reported_time_series(C, V, Y, _, _Cov0),
  #     survey(C, V, Y, Expl0, Survey),
  #     % survey_reported_threshold(Threshold),
  #     % abs(Cov0 - Survey) > Threshold,
  #     !,
  #     Rule = 'S: AP',
  #     concat_atom(['Survey evidence does not support reported data.
  #                   Estimate based on survey results. ',
  #     Expl0, ' '], Expl),
  #     Coverage = Survey.
  index = which(abs(Svy.Cov - TS.Cov) > svy.thrs(), arr.ind=TRUE)
  Rule[index] = "S: AP"
  Info[index] = sprintf(
    "Survey evidence does not support reported data. Estimate based on survey results. %s ",
    Svy.Expl[index])
  Cov[index] = Svy.Cov[index]
  
  # Survey results support reported
  #
  # Prolog
  # anchor(C, V, Y, Rule, Expl, Coverage) :-
  #     reported_time_series(C, V, Y, Source, Cov0),
  #     survey(C, V, Y, Expl0, Survey),
  #     survey_reported_threshold(Threshold),
  #     abs(Cov0 - Survey) =< Threshold,
  #     !,
  #     Rule = 'R: AP',
  #     member(Source-Expl1,
  #       [ gov-'Estimate informed by reported data supported by survey. ',
  #         admin-'Estimate informed by reported administrative data
  #                supported by survey. ',
  #         interpolated-'Estimate informed by interpolation between
  #                reported data supported by survey. ',
  #         extrapolated-'Estimate based on extrapolation from data reported
  #                by national government supported by survey. '
  #       ]),
  #     concat_atom([Expl1, Expl0], Expl),
  #     Coverage = Cov0.
  info = c(
    gov="Estimate informed by reported data supported by survey. ",
    admin="Estimate informed by reported administrative data supported by survey. ",
    interpolated="Estimate informed by interpolation between reported data supported by survey. ",
    extrapolated="Estimate based on extrapolation from data reported by national government supported by survey. ")
  
  index = which(abs(Svy.Cov - TS.Cov) <= svy.thrs(), arr.ind=TRUE)
  Rule[index] = "R: AP"
  Cov[index] = TS.Cov[index]
  Info[index] = sprintf("%s%s", info[TS.Src[index]], Svy.Expl[index])
  
  # Reported value "anchored" by working group decision
  #
  # Prolog
  # anchor(C, V, Y, Rule, Expl, Coverage) :-
  #     reported_time_series(C, V, Y, _, Cov0),
  #     decision(C, V, Y, assignAnchor, Expl0, _, Cov0), % same Cov0
  #     !,
  #     Rule = 'R: AP',
  #     Expl = Expl0,
  #     Coverage = Cov0.
  
  index = Decisions[Decisions$Dec == "assignAnchor", ]
  equal = which(TS.Cov[cbind(index$Y, index$V)] == index$Cov, arr.ind=TRUE)
  
  index = index[equal, ]
  Rule[cbind(index$Y, index$V)] = "R: AP"
  Info[cbind(index$Y, index$V)] = index$Info
  Cov[cbind(index$Y, index$V)] = index$Cov
  
  # Working group assigns anchor point value.
  #
  # Prolog
  # anchor(C, V, Y, Rule, Expl, Coverage) :-
  #     reported_time_series(C, V, Y, _, _Cov0),
  #     decision(C, V, Y, assignAnchor, Expl0, _, Assigned),
  #     % Cov0 \= Assigned,
  #     !,
  #     Rule = 'W: AP',
  #     concat_atom(['Estimate of ', Assigned,
  #         ' percent assigned by working group. ', Expl0], Expl),
  #     Coverage = Assigned.
  index = Decisions[Decisions$Dec == "assignAnchor", ]
  neq = which(TS.Cov[cbind(index$Y, index$V)] != index$Cov, arr.ind=TRUE)
  
  index = index[neq, ]
  Rule[cbind(index$Y, index$V)] = "W: AP"
  Cov[cbind(index$Y, index$V)] = index$Cov
  Info[cbind(index$Y, index$V)] = sprintf(
    "Estimate of %g percent assigned by working group. %s", 
    index$Cov, index$Info)
  
  list(Cov=Cov, Rule=Rule, Info=Info)
}