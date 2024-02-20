# Collect explanations in natural language terms
#
# Prolog
# collect_explanations(C, V, Y, Explanations) :-
#     findall(Expl, explanation(C, V, Y, Expl), Explanations).

# explanation(C, V, Y, Expl) :-
#     survey_reason_to_exclude(C, V, Y, _, Expl).
# explanation(C, V, Y, Expl) :-
#     survey_modified(C, V, Y, _, Expl, _).

Expl = Svy.Excl # from 06_survey

# explanation(C, V, Y, Expl) :-
#     reported_reason_to_exclude(C, V, Y, _, Expl).

Expl[] = sprintf("%s%s", Expl, Rej.Info) # from 04_check

# explanation(C, V, Y, Expl) :-
#     decision(C, V, Y, comment, Expl, _, _).
#
# Multiple decisions for the same year * vaccine combination
index = Decisions[Decisions$Dec == "comment", ]
if(nrow(index))
  index = aggregate(list(Info=index$Info), 
                    list(Y=index$Y, V=index$V), FUN=paste, collapse="")
Expl[cbind(index$Y, index$V)] = 
  sprintf("%s%s", Expl[cbind(index$Y, index$V)], index$Info)

# From 06_survey
Expl[] = sprintf("%s%s", Expl, Svy.Expl.Acc)

# From 03_reported
Expl[] = sprintf("%s%s", Expl, Rep.Expl)

# Combine with other texts
Expl[] = sprintf("%s %s %s %s", Info, Expl, Change, GoC.Expl)

