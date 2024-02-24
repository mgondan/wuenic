# Estimate coverage by distinguishing different cases
#
# * at anchor point
# * between anchor points
# * before anchor point
# * after anchor point
# * no anchor points

wuenic.calibrate = function(TS.Cov, TS.Src, Anchor.Cov, Anchor.Rule, Anchor.Info, Decisions)
{
  # Default: No anchor points for any year, use reported. If any other rule
  # applies, it is overwritten.
  #
  # Prolog
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
  Cov = TS.Cov
  
  info = c(
    gov="Estimate informed by reported data. ",
    admin="Estimate informed by reported administrative data. ",
    interpolated="Estimate informed by interpolation between reported data. ",
    extrapolated="Estimate informed by extrapolation from reported data. ")
  Info = YV.char()
  Info[] = info[TS.Src]
  
  Rule = YV.char()
  Rule[!is.na(TS.Cov)] = "R:"
  
  # Before earliest/after latest anchor (not of Type "reported"): calibrate, 
  # i.e. use anchored coverage and apply differences of reported data.
  #
  # Prolog
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
  index = !is.na(TS.Cov) & is.na(Anchor.Cov)
  Prec.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, na.rm=FALSE)
  index = index & !is.na(Prec.Rule) & Prec.Rule != "R: AP"
  
  # Copy coverage and year
  Prec.Cov = apply(Anchor.Cov, 2, FUN=zoo::na.locf, na.rm=FALSE)
  Prec.Year = YV.char(Yn())
  Prec.Year[index] = NA
  Prec.Year = apply(Prec.Year, 2, FUN=zoo::na.locf, na.rm=FALSE)
  
  Rule[index] = "C:"
  Info[index] = sprintf("Reported data calibrated to %s levels. ",
                        Prec.Year[index])
  
  # Apply differences
  yv = expand.grid(Y=Yn(), V=Vn(), stringsAsFactors=FALSE)
  Adj = Anchor.Cov[cbind(c(Prec.Year), yv$V)] - TS.Cov[cbind(c(Prec.Year), yv$V)]
  Cov[index] = TS.Cov[index] + Adj[index]
  
  # Same in the reverse direction
  index = !is.na(TS.Cov) & is.na(Anchor.Cov)
  Succ.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  index = index & !is.na(Succ.Rule) & Succ.Rule != "R: AP"
  
  Succ.Cov = apply(Anchor.Cov, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  Succ.Year = YV.char(Yn())
  Succ.Year[index] = NA
  Succ.Year = apply(Succ.Year, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  
  Rule[index] = "C:"
  Info[index] = sprintf("Reported data calibrated to %s levels. ", 
                        Succ.Year[index])
  
  yv = expand.grid(Y=Yn(), V=Vn(), stringsAsFactors=FALSE)
  Adj = Anchor.Cov[cbind(c(Succ.Year), yv$V)] - TS.Cov[cbind(c(Succ.Year), yv$V)]
  Cov[index] = TS.Cov[index] + Adj[index]
  
  # Before earliest/after latest anchor of Type "reported": extrapolate
  #
  # Prolog
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
  #         admin-'Estimate informed by reported administrative data. ',
  #         interpolated-'Estimate informed by interpolation between reported data. ',
  #         extrapolated-'Estimate based on extrapolation from data reported by national government. '
  #       ]),
  #     Coverage = Cov0.
  
  # Search for preceding anchor
  index = !is.na(TS.Cov) & is.na(Anchor.Cov)
  Prec.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, na.rm=FALSE)
  index = index & !is.na(Prec.Rule) & Prec.Rule == "R: AP"
  
  info = c(gov="Estimate informed by reported data. ",
           admin="Estimate informed by reported administrative data. ",
           interpolated="Estimate informed by interpolation between reported data. ",
           extrapolated="Estimate based on extrapolation from data reported by national government. ")
  
  Rule[index] = "R:"
  Info[index] = info[TS.Src[index]]
  Cov[index] = TS.Cov[index]
  
  # Search for next anchor
  index = !is.na(TS.Cov) & is.na(Anchor.Cov)
  Succ.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  index = index & !is.na(Succ.Rule) & Succ.Rule == "R: AP"
  
  Rule[index] = "R:"
  Info[index] = info[TS.Src[index]]
  Cov[index] = TS.Cov[index]
  
  # Between other anchor points (not both of Type "reported"): calibrate
  #
  # Prolog
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
  
  # Search for previous and next anchor point
  index = !is.na(TS.Cov) & is.na(Anchor.Cov)
  Prec.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, na.rm=FALSE)
  Succ.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  index = index & !is.na(Prec.Rule) & !is.na(Succ.Rule)
  index = index & (Prec.Rule != "R: AP" | Succ.Rule != "R: AP")
  
  # Interpolate coverage between anchors
  Prec.Cov = apply(Anchor.Cov, 2, FUN=zoo::na.locf, na.rm=FALSE)
  Succ.Cov = apply(Anchor.Cov, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  
  Prec.Year = YV.char(Yn())
  Prec.Year[index] = NA
  Prec.Year = apply(Prec.Year, 2, FUN=zoo::na.locf, na.rm=FALSE)
  
  Succ.Year = YV.char(Yn())
  Succ.Year[index] = NA
  Succ.Year = apply(Succ.Year, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  
  Rule[index] = "C:"
  Info[index] = sprintf("Reported data calibrated to %s and %s levels. ", 
                        Prec.Year[index], Succ.Year[index])
  
  Itp1.Cov = apply(Anchor.Cov, 2, FUN=zoo::na.approx, na.rm=FALSE)
  rownames(Itp1.Cov) = Yn()
  
  # Todo: this shouldn't be rounded
  Itp1.Cov[] = tround(Itp1.Cov)
  
  # Interpolate TS.Cov for year without anchors
  Itp2.Cov = TS.Cov
  Itp2.Cov[index] = NA
  Itp2.Cov = apply(Itp2.Cov, 2, FUN=zoo::na.approx, na.rm=FALSE)
  rownames(Itp2.Cov) = Yn()
  
  # Todo: this shouldn't be rounded
  Itp2.Cov[] = tround(Itp2.Cov)
  
  # Apply difference
  Adj = Itp1.Cov - Itp2.Cov
  Cov[index] = tround(TS.Cov[index] + Adj[index])
  
  # Between anchor points of type "reported": interpolate
  #
  # Prolog
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
  
  index = !is.na(TS.Cov) & is.na(Anchor.Cov)
  Prec.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, na.rm=FALSE)
  Succ.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  index = index & !is.na(Prec.Rule) & !is.na(Succ.Rule)
  index = index & Prec.Rule == "R: AP" & Succ.Rule == "R: AP"
  
  index1 = index & TS.Src %in% c("gov", "admin", "interpolated")
  info = c(gov="Estimate informed by reported data. ",
           admin="Estimate informed by reported administrative data. ",
           interpolated="Estimate informed by interpolation between reported data. ")
  
  Rule[index1] = "R:"
  Info[index1] = info[TS.Src[index1]]
  Cov[index1] = TS.Cov[index1]
  
  # Todo: if it is extrapolated, the rule fails and NA is returned. See the
  # corresponding Prolog rule. Maybe a misunderstanding on my side, because the
  # code is never reached.
  index2 = index & TS.Src == "extrapolated"
  Rule[index2] = NA
  Info[index2] = NA
  Cov[index2] = NA
  
  # Interpolation forced by working group
  #
  # Prolog
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
  
  Prec.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, na.rm=FALSE)
  Prec.Cov = apply(Anchor.Cov, 2, FUN=zoo::na.locf, na.rm=FALSE)
  Succ.Rule = apply(Anchor.Rule, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  Succ.Cov = apply(Anchor.Cov, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  
  Prec.Year = YV.char(Yn())
  Prec.Year[is.na(Anchor.Cov)] = NA
  Prec.Year = apply(Prec.Year, 2, FUN=zoo::na.locf, na.rm=FALSE)
  
  Succ.Year = YV.char(Yn())
  Succ.Year[is.na(Anchor.Cov)] = NA
  Succ.Year = apply(Succ.Year, 2, FUN=zoo::na.locf, fromLast=TRUE, na.rm=FALSE)
  
  WI = YV.char()
  WI[cbind(index$Y, index$V)] = index$Info
  WI[is.na(Prec.Rule) | is.na(Succ.Rule)] = NA
  
  index = !is.na(WI)
  Rule[index] = "W-I:"
  Info[index] = sprintf(
    "Estimate informed by interpolation between %s and %s levels. %s",
    Prec.Year[index], Succ.Year[index], WI[index])
  
  Itp1.Cov = apply(Anchor.Cov, 2, FUN=zoo::na.approx, na.rm=FALSE)
  rownames(Itp1.Cov) = rownames(Anchor.Cov)
  Cov[index] = tround(Itp1.Cov[index])
  
  # At anchor points
  #
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

  list(Rule=Rule, Info=Info, Cov=Cov)  
}