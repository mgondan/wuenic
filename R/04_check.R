library(zoo)

# 3. Check reported data
#
# Reasons to exclude reported data are working group decisions, coverage > 100%
# or temporal inconsistency (sudden jumps, sudden declines, etc.)
#
# This is the Prolog code
#
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

reject = YV.bool()

# Sudden change in most recently reported data for new vaccines. This applies
# only for the last reported data.
#
# Todo: This may generate inconsitencies over the years, since the "last
# reported data" changes from year to year.
#
# For pcv3 and rotac, we only check for a decline
jump = Rep.Cov[, Vn() %in% c("pcv3", "rotac"), drop=FALSE]

# Search for last reported data (that is, trim all NAs from the right)
jump = apply(jump, 2, na.trim, sides="right", simplify=FALSE)

# Determine jumps from year to year
jump = lapply(jump, diff)

# Obtain last jump (first revert, then pick element #1)
jump = lapply(jump, rev)
jump = lapply(jump, `[`, 1)

# Check if it is a decline below the sawtooth criterion
J = sapply(jump, `<`, -sawtooth())

# Extract name and year from element names
Y = sapply(jump, names)[which(J)]
V = names(jump)[which(J)]

# These entries should be rejected
reject[cbind(Y, V)] = J[!is.na(J)]

# Put together a string with the explanation. We need to shift the year to
# access the previous coverage.
prev = Rep.Cov
prev[] = rbind(NA, Rep.Cov[-length(Yn()), ])

# Do the same for the next year. Needed below.
succ = Rep.Cov
succ[] = rbind(Rep.Cov[-1, ], NA)

Rej.Info = YV.char("")
Rej.Info[cbind(Y, V)] = sprintf(
  "Reported data excluded due to decline in reported coverage from %i level to %i percent. ",
  prev[cbind(Y, V)], Rep.Cov[cbind(Y, V)])

# For the other vaccines, we check for an absolute change above the threshold.
jump = Rep.Cov[, !(Vn() %in% c("pcv3", "rotac")), drop=FALSE]
jump = apply(jump, 2, na.trim, sides="right", simplify=FALSE)
jump = lapply(jump, diff)
jump = lapply(jump, rev)
jump = lapply(jump, `[`, 1)
jump = lapply(jump, abs)  # up or down

J = sapply(jump, `>`, sawtooth())
Y = sapply(jump, names)[which(J)]
V = names(jump)[which(J)]
reject[cbind(Y, V)] = J[which(J)]

Rej.Info[cbind(Y, V)] = sprintf(
  "Reported data excluded due to sudden change in coverage from %i level to %i percent. ",
  prev[cbind(Y, V)], Rep.Cov[cbind(Y, V)])

# Reject sudden ups and downs above the sawtooth threshold
#
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
# First up, then down
up    = diff(rbind(NA, Rep.Cov)) > sawtooth()
down  = diff(rbind(Rep.Cov, NA)) < -sawtooth()
index = which(up & down, arr.ind=TRUE)
reject[index] = TRUE

Rej.Info[index] = sprintf(
  "Reported data excluded due to an increase from %i percent to %i percent with decrease to %i percent. ",
  prev[index], Rep.Cov[index], succ[index])

# First down, then up
down  = apply(rbind(NA, Rep.Cov), 2, diff) < -sawtooth()
up    = apply(rbind(Rep.Cov, NA), 2, diff) > sawtooth()
index = which(down & up, arr.ind=TRUE)
reject[index] = TRUE

Rej.Info[index] = sprintf(
  "Reported data excluded due to decline in reported coverage from %i percent to %i percent with increase to %i percent. ",
  prev[index], Rep.Cov[index], succ[index])

# Reject implausible coverage (i.e., above 100)
#
# Prolog
# reported_rejected(C, V, Y) :-
#     reported(C, V, Y, _, Coverage),
#     Coverage > 100,
#     !.

index = which(Rep.Cov > 100, arr.ind=TRUE)
reject[index] = TRUE
Rej.Info[index] = sprintf(
  "Reported data excluded because %i percent greater than 100 percent. %s", 
  Rep.Cov[index], Rej.Info[index])

# Check if a working group decision has been made to accept it anyway
#
# Prolog
# reported_rejected(C, V, Y) :-
#     decision(C, V, Y, acceptReported, _, _, _),
#     !,
#     fail.

accept = Decisions[Decisions$Dec == "acceptReported", ]
index = cbind(accept$Y, accept$V)
reject[index] = FALSE
Rej.Info[index] = ""

# Check if a working group decision has been made to finally ignore it
# (i.e., ignore dominates accept)
#
# Prolog
# reported_rejected(C, V, Y) :-
#     decision(C, V, Y, ignoreReported, _Expl0, _, _),
#     !.

ignore = Decisions[Decisions$Dec == "ignoreReported", ]
index = cbind(ignore$Y, ignore$V)
reject[index] = TRUE

Rej.Info[index] = ifelse(is.na(Rep.Cov[index]), 
  Rej.Info[index],
  sprintf("Reported data excluded. %s%s", ignore$Info, Rej.Info[index]))

# reject is used in the later course
