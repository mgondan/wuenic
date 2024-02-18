# 2. Add information from government and administration.
#
# Reported to WHO and UNICEF is government estimate. If government estimate
# missing, then reported is administrative data. Working group decisions may
# override this rule.
#
# In the following implementation, we start at the lowest level, which is the
# administrative data. If the government also reports the coverage, the former
# numbers are overwritten.
#
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

Rep.Cov = Admin
Rep.Src = ifelse(is.na(Admin), NA, "admin")

# If the working group decides to ignore the admin data, revert back to NA.
ignore = (Decisions$Dec == "ignoreAdmin")
index = cbind(Decisions$Y[ignore], Decisions$V[ignore])
Rep.Cov[index] = NA
Rep.Src[index] = NA

# If we have valid government data, use the latter.
#
# reported(C, V, Y, Source, Coverage) :-
#    gov(C, V, Y, Cov0),
#    not(decision(C, V, Y, ignoreGov, _, _, _)),
#    !,
#    Source = gov,
#    Coverage = Cov0.

index = !is.na(Gov)
ignore = Decisions$Dec == "ignoreGov"
index[cbind(Decisions$Y[ignore], Decisions$V[ignore])] = FALSE
Rep.Cov[index] = Gov[index]
Rep.Src[index] = "gov"