# 4. Generate time series based on reported data where an estimate is required.
# This means interpolation and extrapolation for years without reported
# coverage.
#
TS.Cov = YV.int()
TS.Src = YV.char()

# Reported data is available and has not been rejected.
#
# Prolog
# reported_time_series(C, V, Y, Source, Coverage) :-
#     estimate_required(C, V, Y, _, _),
#     reported(C, V, Y, Source0, Cov0),
#     not(reported_rejected(C, V, Y)),
#     !,
#     Source = Source0,
#     Coverage = Cov0.

index = Ereq & !reject 
TS.Cov[index] = Rep.Cov[index]
TS.Src[index] = Rep.Src[index]

# If reported data is unavailable or has been excluded, check if we can
# interpolate.
#
# Prolog
# reported_time_series(C, V, Y, Source, Coverage) :-
#     estimate_required(C, V, Y, _, _),
#     (   not(reported(C, V, Y, _, _))
#     ;   reported_rejected(C, V, Y)
#     ),
#     year_before_reported(C, V, Y, Prec, PrecCov),
#     year_after_reported(C, V, Y, Succ, SuccCov),
#     !,
#     Source = interpolated,
#     interpolate(Prec, PrecCov, Succ, SuccCov, Y, Coverage).

# First step: do the interpolation with zoo::na.approx
inter = Rep.Cov
inter[reject] = NA
inter = apply(inter, 2, na.approx, na.rm=FALSE)

# Second step: Fill those places where (a) an estimate is required,
# (b) interpolation was successful and (c) coverage is missing or has been
# rejected.

index = Ereq & !is.na(inter) & (is.na(Rep.Cov) | reject)
TS.Cov[index] = tround(inter[index])
TS.Src[index] = "interpolated"

# Extrapolation: Carry forward last reported estimate.
#
# Prolog
# reported_time_series(C, V, Y, Source, Coverage) :-
#     estimate_required(C, V, Y, _, _),
#     (   not(reported(C, V, Y, _, _))
#     ;   reported_rejected(C, V, Y)
#     ),
#     nearest_reported(C, V, Y, _Year, Cov0),
#     !,
#     Source = extrapolated,
#     Coverage = Cov0.

# First step: do the interpolation with zoo::na.locf
extra = Rep.Cov
extra[reject] = NA
extra = apply(extra, 2, na.locf, na.rm=FALSE)

# Second step: Fill those places where (a) an estimate is required,
# (b) extrapolation was successful and (c) coverage is missing or has been
# rejected. But do not overwrite interpolated (d).

index = Ereq & !is.na(extra) & (is.na(Rep.Cov) | reject) & is.na(inter)
TS.Cov[index] = tround(extra[index])
TS.Src[index] = "extrapolated"

# Extrapolation: Carry backward first reported estimate.
extra = Rep.Cov
extra[reject] = NA
extra = apply(extra, 2, na.locf, na.rm=FALSE, fromLast=TRUE)

# See above, 2nd step
index = Ereq & !is.na(extra) & (is.na(Rep.Cov) | reject) & is.na(inter)
TS.Cov[index] = tround(extra[index])
TS.Src[index] = "extrapolated"
